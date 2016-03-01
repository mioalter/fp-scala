import com.twitter.scalding._
import com.twitter.algebird.Aggregator.size
import com.twitter.scalding.mathematics.Matrix._

/**
 * A reworking, for personal edification, of 
 * https://github.com/scalding-io/ProgrammingWithScalding/blob/master/chapter9/src/main/scala/BookSimilarity.scala
 */

class TFIDF_Job(args : Args) extends Job(args) {

  val input = TextLine(args("input"))
  val output = args("output")
  
  val rawCorpus : TypedPipe[(String,String)] = TypedPipe.from(input).map(x => x.split("\\t")).map(y => (y(0),y(1)))
  
  val corpus = rawCorpus.flatMap(x => preprocessPlus(x._1, x._2))
  
  /**
   * We parse the document text into words with preprocess.
   * We then want to explode each (book : String, raw_text : String) 
   * row into a list of (book, word : String) rows, one for each word in the book, and flatten these all into a
   * single table/typedpipe.
   * In Scala, this is flatmap; in Hive it is a Lateral View Explode.
   * There is a hack: preprocess : (raw_text : String) => List[(word : String)]
   * but we want to feed it a (book : String, raw_text : String) and get a List[(book : String, word : String)]
   * where the first component just comes along for the ride.
   * We do this here with preprocessPlus.
   * 
   * And now, a gross digression on FP that can be freely ignored:
   * The general situation is, we have a function f : A => F[B],
   * and we want a function (C,A) => F[(C,B)]
   * (we want a list of pairs at the end, not a pair of lists hence F of a tuple).
   * To promote f to a function (C,A) => F[(C,B)] we need for F[_] to be an applicative.
   * In this case, 
   *   - we have a function pure : C => F[C],
   *   - we combine f and pure to make fPlus : (C,A) => (F[C],F[B])
   *   - we then use F's ap : (F[A => B], F[A]) => F[B] to  define a map flip : (F[C],F[B]) => F[(C,B)]
   *   - we compose these to get what we want
   * Here, we need that List is a monad to flatMap, but we only need that it is an applicative
   * to promote preprocess to preprocessPlus.
   */
  
  def preprocess(rawText : String) : List[String] = {
    rawText
      .toLowerCase
      .replaceAll("[^a-zA-Z0-9\\s]","")
      .split("\\s+")
      .toList
      .filter(_.length > 0)
  }
  
  def preprocessPlus(book : String, rawText : String) : List[(String, String)] = {
    for {
      a <- preprocess(rawText)
    } yield(book, a)
  }
  
  /**
   * Group By (book, word) to count number of times each word appears in each book
   * termCounts : UnsortedGrouped[(book : String, word : String), count: Long]
   */
  val termCounts = corpus.groupBy(identity).size
  
  /**
   * We *should* be able to compute termTotals by further rolling up termCounts
   * but I don't immediately see how to project out half of the key and group by the new key.
   * Ah-ha, we can do
   * termCounts
   * .map(x => (x._1._1, x._2))
   *   to project out the second component of (book,word)
   *   this produces key-value pairs (book, List(term_count : Int))
   *   We can then 
   * .groupBy(_._1)
   * .sumByKey 
   *   to concatenate those lists of counts into a single list of term counts for each book, then
   * .mapValues(x => x.sum) 
   *   to sum the counts in each list
   * Doing a different groupBy directly on corpus is certainly much clearer so let's stick with that for now.
   */
  
  val termTotals = corpus.groupBy(_._1).size
  
  /**
   * Now, we want to join termCounts and termTotals on book to compute the term frequency.
   * This may not be super glamorous, but we can just move the word from keys
   * (K,V) = ((book, word), count)
   * to values
   * (K',V') = (book, (word, count))
   * then do the join.
   */
  val termFrequencies = (
    termCounts
      .map{case ((book, word), count) => (book, (word, count))}
      .join(termTotals) // : (K',W) = (book, ((word, count), total))
      .map{case (book, ((word, count), total)) => ((book, word), count.toDouble / total)}
  )
  
  
  val documentCounts = (
    termCounts
      .groupBy(_._1._2) // ((book, word), count) group by word
      .size // the size of each group, that is, the number of documents in which a word appears
    )
  
  /**
   * termTotals has columns (book, num words in book)
   * numDocuments is equivalent to 
   * 
   * SELECT COUNT(*)
   * FROM termTotals
   * 
   * Although termTotals only has one row per book
   * termTotals.aggregate(size)
   * will group by book and give us a count of 1 per row
   * rather than the total number of rows
   * so we have to project out the counts first, then .aggregate(size)
   */ 
  val numDocuments = (
    termTotals // (book, num words in book)
      .map(_._1)
      .aggregate(size)
    )
  
  val invDocFrequencies = (
    documentCounts
      .cross(numDocuments)
      .map{case ((word, df), total) => (word, math.log(total.toDouble / df))}
    )
  
  val tfidf = (
    termFrequencies
      .map{ case ((book, word), tf) => (word, (book, tf)) }
      .join(invDocFrequencies)
      .map{ case (word, ((book, tf), idf) ) => (book, word, tf * idf) }
      .filter(x => x._3 > 0) // words that appear in every document have idf = 0, filter these out.
    )
  
  /**
   * Now, let's make this into a matrix
   * and very slickly compute cosine similarities
   * using the Matrix API.
   * We cannot directly convert a TypedPipe to a Matrix,
   * we have to convert to Pipe in between
   */
  
  val unTFIDF = tfidf.toPipe('book, 'word, 'tfidf)
  val corpusMatrix = unTFIDF.toMatrix[String, String, Double]('book, 'word, 'tfidf)
  
  /**
   * This is a books x words matrix
   * so each row is a word-vector representing a book.
   * To make these unit vectors, we row-normalize.
   * Normalizing the rows means that the cosine similarity
   * is just the dot product (we don't have to divide by the norms)
   * To compute these dot products en masse, we can just multiply the
   * row-normalized matrix by its transpose
   */
  
  val normedMatrix = corpusMatrix.rowL2Normalize
  val similaritiesMatrix = normedMatrix * normedMatrix.transpose
  val similaritiesTP : TypedPipe[(String, String, Double)] = TypedPipe.from[(String, String, Double)](similaritiesMatrix.pipe, ('row, 'col, 'val)) // ('row, 'col, 'val) are the default names when you convert a matrix to a pipe
  
  val similarities = (
    similaritiesTP
      .filter(x => x._1 < x._2) // the matrix is symmetric, take the upper triangle (ignoring the diagonal)
      .groupAll //send everything to one reducer to properly sort
      .sortBy(_._3) // sort by similarity
      .reverse // decreasing
  )

  similarities.values.write(TypedTsv[(String, String, Double)](output))
}
