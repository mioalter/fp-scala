import com.twitter.scalding._
import com.twitter.algebird.Aggregator.size
import com.twitter.scalding.mathematics.Matrix._

val input = TextLine("examples/input/books.txt")

val rawCorpus : TypedPipe[(String,String)] = TypedPipe.from(input).map(x => x.split("\\t")).map(y => (y(0),y(1)))

val corpus = rawCorpus.flatMap(x => preprocessPlus(x._1, x._2))

/**
 * We parse the document text into words with preprocess.
 * We then want to explode each (book : String, raw_text : String) 
 * row into a list of (book, word : String) rows and flatten these all into a
 * single table/typedpipe. In Scala, this is flatmap; in Hive it is a Lateral View Explode.
 * There is a hack: preprocess : String => List[String]
 * but we want to feed it a (String, String) and
 * have the first component just come along for the ride.
 * The general situation is, we have a function A => F[B],
 * and we want a function (C,A) => F[(C,B)]
 * (we want a list of pairs at the end, not a pair of lists hence F of a tuple).
 * To do this, we REVIEW IN HASKELL
 *
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

val corpusMatrix = (
  val unTFIDF = tfidf.toPipe('book, 'word, 'tfidf)
  unTFIDF.toMatrix[String, String, Double]('book, 'word, 'tfidf)
)

/**
 * This is a books x words matrix
 * so each row is a word-vector representing a book
 * to make these unit vectors, we row-normalize.
 * Normalizing the rows means that the cosine similarity
 * is just the dot product (we don't have to divide by the norms)
 * To compute these dot products en masse, we can just multiply the
 * row-normalized matrix by its transpose
 */

val normMat = corpusMatrix.rowL2Normalize
val similarities = normMat * normMat.transpose

//val simsTP : TypedPipe[(String, String, Double)] = TypedPipe.from[(String, String, Double)](similarities.pipe)
// have to figure out how to make this into a typed pipe or forget it and make into a pipe.