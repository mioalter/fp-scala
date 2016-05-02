/**
 * Implement a Trie
 * 
 * References
 * - Purely Functional Data Structures by Okasaki
 * - Algorithm Design Manual by Skiena
 * 
 *  A trie is a prefix tree
 * The nodes are Option[String]s
 * and the branches are labeled with characters
 * The root node is a None,
 * and the unique path from the root to any node spells a word
 * the Option value at the node indicates if that is a word in our original list
 * word or just a prefix of a word (not a legitimate word).
 * We can quickly lookup a query word of length |q| to see if it's in our trie
 * by doing at most |q| character comparisons.
 *
 * We can use a trie to solve the substring problem:
 * given a string S and a pattern q
 * - does q appear in S?
 * - how many times does q appear in S?
 * We do this by making a trie of all the suffixes of S
 * and seeing if q is a prefix of anything in that trie.
 * We similarly compute the number of times q appears in S
 * by seeing, if it is a prefix of any suffixes, of how many.

 * This is a proof of concept, it not very efficient.
 * If this were a real project, we wouldn't jam everything into one object, either.
 * One big optimization is to collapse long paths of nodes with only one child into a single node
 * Another would be to make this lazy so we don't build more of the trie than we have to.
 * Another fast way to solve the substring problem is to use a suffix array.
 */

object Trie {

  sealed trait Trie {

    def lookup(query : String) : Option[String] = {

      def lookupOption(word : Vector[Char])(trie : Trie) : Option[String] =
        (word, trie) match {
          case (Vector(), T(None, _)) => None
          case (Vector(), T(Some(x),_)) => Some(x)
          case ((x +: xs), T(v,m)) => m.get(x).flatMap{case t => lookupOption(xs)(t)}
          // Option's flatMap is exactly the thing that lets 
          // us avoid writing "case Some(t) => lookup(xs)(t); case _ => None"
        }

      lookupOption(query.toVector)(this)
    }

    /**
     * The substring problem v1
     */
    def isSubstring(query : String) : Boolean = {
    
      def subStringHelper(queryChars : Vector[Char])(trie : Trie) : Boolean =
        (queryChars, trie) match {
          case (Vector(),_) => true
          case ((x +: xs), T(v,m)) => 
            m.get(x) match {
              case Some(nextTrie) => subStringHelper(xs)(nextTrie)
              case _ => false
            }
          } 
    
      subStringHelper(query.toVector)(this)
      // could also just use t.numSubstring(query) > 0
  
    }
  
    /**
     * The substring problem v2
     * Computes the number of times query appears in our trie
     */
    def numSubstring(query : String) : Int = {

      def numPrefixes(t : Trie) : Int =
        t match {
          case T(None, m) if m.size == 0 => 0
          case T(Some(x), m) if m.size == 0 => 1
          case T(None, m) => m.mapValues(numPrefixes).values.sum
          case T(Some(x),m) => m.mapValues(numPrefixes).values.sum + 1
          // if our query string exactly matches a word in our trie
          // we want to count the exact match and all the words
          // of which query is a prefix, hence the +1
        }    

      def numSubstringHelper(queryChars : Vector[Char])(trie : Trie) : Int = 
        (queryChars, trie) match {
          case (Vector(), _) => numPrefixes(trie)
          case (x +: xs, T(v,m)) =>
            m.get(x) match {
              case Some(nextTrie) => numSubstringHelper(xs)(nextTrie)
              case _ => 0
            }  
        }

      numSubstringHelper(query.toVector)(this)
    
    }   

    /**
     * Display a trie Left to Right where 
     * indentation indicates depth
     * the default toString is as deeply nested Maps which are impossible to read
     */
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

    /** 
     * We define a monoid instance for Trie
     * and use it to define insert
     */
    def insert(word : String) : Trie = 
      TrieMonoidInstance.plus(this, toTrie(word))

  }


  case class T(value : Option[String], children : Map[Char, Trie]) extends Trie
  val EmptyTrie = T(None,Map())


  /** 
   * Make a monoid instance for Tries
   * in terms of which we can define insert
   * to build tries from lists of words
   */
  trait Monoid[A] {
    def zero : A
    def plus(a : A, b : A) : A  
  }

  /**
   * the key is to be able to combine Maps whose values are a monoid 
   */
  def addMap[A,B](a : Map[A,B], b : Map[A,B])(implicit ev : Monoid[B]) : Map[A,B] = {
    lazy val asbs = a.toVector ++ b.toVector // vector of (key,value) pairs
    lazy val kvs = 
      asbs
        .groupBy(_._1) // group by key
        .mapValues(_.map(_._2)) // project the key out of the values
        // the values are now : Vector[B]
    kvs.mapValues(x => x.foldLeft(ev.zero)(ev.plus))
    // sum the values to get a B
    // I'm sure there is an existing implementation of this
    // in Algebird if not the standard library
    // but I'm on a plane without free internet
    }

  /** 
   * We can use addMap to combine the children of two nodes
   * then recursively use the Monoid instance
   * to combine the tries at the next level
   */
  implicit object TrieMonoidInstance extends Monoid[Trie] {
    def zero : Trie = EmptyTrie
    def plus(a : Trie, b : Trie) : Trie = 
      (a,b) match {
        case (T(None, m), T(None,n)) => T(None, addMap(m, n)(TrieMonoidInstance))
        case (T(Some(x),m), T(None,n)) => T(Some(x), addMap(m, n)(TrieMonoidInstance))
        case (T(None, m), T(Some(y), n)) => T(Some(y), addMap(m, n)(TrieMonoidInstance))
        case (T(Some(x),m), T(Some(y),n)) if x == y => T(Some(x), addMap(m, n)(TrieMonoidInstance))
        case _ => zero

      }

  }  


  def toTrie(word : String) : Trie = {
    
    def vectToTrie(chars : Vector[Char]): Trie = 
      chars match {
        case Vector() => T(Some(word), Map())
        case (x +: xs) => T(None, Map((x, vectToTrie(xs))))
    }

    vectToTrie(word.toVector)

  }

  def toTrie(words : Vector[String]) : Trie = 
    words.foldLeft[Trie](EmptyTrie){case (trie, word) => trie.insert(word)}

}

object Examples {

  import Trie._
  
  def suffixes(s : String) : Vector[String] = {
    val suffs = for (i <- 0 to (s.length - 1)) yield s.drop(i)
    suffs.toVector
  }
  
  val t1 = toTrie(Vector("yes","yaas","yez")) 
  val t2 = toTrie(Vector("yest","yeast","yast"))
  
  
  val ts = TrieMonoidInstance.plus(t1,t2)
  val numYe = ts.numSubstring("ye") // 4

  val s = "yaryastyarz"
  val t = toTrie(suffixes(s))
  val numYar = t.numSubstring("yar") // 2

}

