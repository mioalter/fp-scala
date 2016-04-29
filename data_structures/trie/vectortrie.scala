// Implement a Trie
//
// A trie is a prefix tree
// 
// Paths from the root are all prefixes of a string
// Each node is an Option which indicates if that prefix forms a valid word
// The edges are labeled with the characters.

//import scala.language.higherKinds

object Trie {

  sealed trait Trie {

    private def lookupOption(word : Vector[Char]) : Option[String] =
      (word, this) match {
        case (Vector(), T(None, _)) => None
        case (Vector(), T(Some(x),_)) => Some(x)
        case ((x +: xs), T(v,m)) => m.get(x).flatMap(_.lookupOption(xs))
        // Option's flatMap is exactly the thing that lets 
        // us avoid writing "case Some(t) => t.lookup(xs); case _ => None"
      }

    def lookup(word : String) : Option[String] = this.lookupOption(word.toVector)


    def isSubstring(q : String) : Boolean = {
    
      def subStringHelper(t : Trie)(qList : Vector[Char]) : Boolean =
        (qList,t) match {
          case (Vector(),_) => true
          case ((x +: xs), T(v,m)) => 
            m.get(x) match {
              case Some(nextTrie) => subStringHelper(nextTrie)(xs)
              case _ => false
            }
          } 
    
      subStringHelper(this)(q.toVector)      
  
  }   

    private def trieToString(n : Int)(t : Trie) : Vector[String] =
      t match {
        case T(value, map) => 
          val valueString = (" " * n) + value.toString + "\n" 
          // would be nice to make (" " * n) + x.toString + "\n" a generic function
          val mapString = 
            map
            .toVector
            .flatMap{case (char, trie) => 
              ((" " * (n + 1)) + char.toString + "\n") +: trieToString(n + 2)(trie)}
          valueString +: mapString
      }

    override val toString : String = trieToString(0)(this).foldLeft("")(_ + _)

    def insert(word : String) : Trie = 
      // TrieMonoidInstance.plus(this, toTrie(Vector(word)))
      TrieMonoidInstance.plus(this, wordToTrie(word))

  }


  case class T(value : Option[String], children : Map[Char, Trie]) extends Trie
  val EmptyTrie = T(None,Map())


  def wordToTrie(word : String) : Trie = {
    
    def vectToTrie(chars : Vector[Char]): Trie = 
      chars match {
        case Vector() => T(Some(word), Map())
        case (x +: xs) => T(None, Map((x, vectToTrie(xs))))
    }

    vectToTrie(word.toVector)

  }

  def wordsToTrie(words : Vector[String]) : Trie = 
    words.foldLeft[Trie](EmptyTrie){case (trie, word) => trie.insert(word)}

  // def toTrie(words : Vector[String]) : Trie = {
  
  //   val wordPairs = words.map(x => (x,x))
  
  //   def pairsToMap(pairs : Vector[(String, String)]) : Map[Char, Vector[(String, String)]] = {
  //     pairs
  //       .map{case (word, remains) => (remains.head, (word, remains.tail))}
  //       .groupBy(_._1) // group all the tuples whose remains start with the same character
  //       .mapValues(_.map(_._2)) // project the character out of the values its only in the keys 
  //       // and the values are just a List[(word, remains)] again
  //   }
  
  //   def makeTrie(wordsAndRemains : Vector[(String, String)]) : Trie = {
  
  //     val continuePairs = wordsAndRemains.filter(_._2 != "")  
  //     val doneWord = wordsAndRemains.filter(_._2 == "").map(_._1).headOption
  //     // if there is more than one element in this list, it means the same word
  //     // appeared more than once in the input so it's okay to take the first
  
  //     continuePairs match {
  //       case (x +: xs) =>
  //         val moreWordsMap = pairsToMap(continuePairs) // Map(firstLetter <- List[(words, remains)])
  //         T(doneWord, moreWordsMap.mapValues(x => makeTrie(x)))
  //       case Vector() => T(doneWord, Map())
  //     }  
  //   }
  
  //   makeTrie(wordPairs)
  
  // }

// }

// object TrieMonoid {

  // import Trie._

  trait Monoid[A] {
    def zero : A
    def plus(a : A, b : A) : A  
  }


  def addMap[A,B](a : Map[A,B], b : Map[A,B])(implicit ev : Monoid[B]) : Map[A,B] = {
    lazy val as = a.toVector
    lazy val bs = b.toVector
    lazy val asbs = as ++ bs // vector of (key,value) pairs
    lazy val kvs = asbs.groupBy(_._1).mapValues(_.map(_._2))
    // group by key, project the key out of the values
    // the values are now : Vector[B]
    kvs.mapValues(x => x.foldLeft(ev.zero)(ev.plus))
    // sum the values to get a B
    // I'm sure there is an existing implementation of this
    // but I'm on a plane without free internet
    }

  implicit object TrieMonoidInstance extends Monoid[Trie] {
    def zero : Trie = T(None, Map())
    def plus(a : Trie, b : Trie) : Trie = 
      (a,b) match {
        case (T(None, m), T(None,n)) => T(None, addMap(m, n)(TrieMonoidInstance))
        case (T(Some(x),m), T(None,n)) => T(Some(x), addMap(m, n)(TrieMonoidInstance))
        case (T(None, m), T(Some(y), n)) => T(Some(y), addMap(m, n)(TrieMonoidInstance))
        case (T(Some(x),m), T(Some(y),n)) if x == y => T(Some(x), addMap(m, n)(TrieMonoidInstance))
        case _ => zero

      }

  }

}

object Examples {
  import Trie._
  // import TrieMonoid._

  val t1 = wordsToTrie(Vector("yes","yaas","yez")) 
  val t2 = wordsToTrie(Vector("yest","yeast","yast"))


  // val tIns = insert(t1)("yays")
  val ts = TrieMonoidInstance.plus(t1,t2)

  val words = Vector("yes","yaas","yez") ++ Vector("yest","yeast","yast")
  // val tfold = words.map(wordToTrie).foldLeft(TrieMonoid.TreeMonoidInstance.zero)(TrieMonoid.TreeMonoidInstance.plus)
}
