// Implement a Trie
//
// A trie is a prefix tree
// Paths from the root are all prefixes of a string
// Each node is an Option which indicates if that prefix forms a valid word
// The edges are labeled with the characters.
// This is what it looks like in Haskell
//
// import Data.Map
//  
//  data Trie a = Trie { value    :: Maybe a,
//                       children :: Map Char (Trie a) }
//
// So each node is a Maybe a
// and the children are a Map of type Char -> Trie a.
//
//

import scala.language.higherKinds

object Trie {

  sealed trait Trie {

    private def lookupOption(word : List[Char]) : Option[String] =
      (word, this) match {
        case (Nil, T(None, _)) => None
        case (Nil, T(Some(x),_)) => Some(x)
        case ((x::xs), T(v,m)) => 
          val nextTrie = m.get(x) // the trie at key x
          nextTrie.flatMap(_.lookupOption(xs))
          // Option's flatMap is exactly the thing that lets 
          // us avoid writing "case Some(t) => t.lookup(xs); case _ => None"
      }

    def lookup(word : String) : Option[String] = this.lookupOption(word.toList)

    private def trieToString(n : Int)(t : Trie) : List[String] =
      t match {
        case T(value, map) => 
          val valueString = (" " * n) + value.toString + "\n" 
          // would be nice to make (" " * n) + x.toString + "\n" a generic function
          val mapString = 
            map
            .toList
            .flatMap{case (char, trie) => ((" " * (n + 1)) + char.toString + "\n") :: trieToString(n+2)(trie)}
          valueString :: mapString
      }

    override val toString : String = trieToString(0)(this).foldLeft("")(_ + _)

  }


  case class T(value : Option[String], children : Map[Char, Trie]) extends Trie

  def toTrie(words : List[String]) : Trie = {

    // At each stage of processing a list of words into a trie
    // we'll have a list (word, remains)
    // where we have the whole original word and the part of the word that's left to process.
    // Our makeTrie function will take such a list,
    // split it into the words that still need to be processed and the word (if any) that's done
    // We'll convert the list of (word, remains) where there is still more to process into a Map
    // Map ( remains.head -> List(word, remains.tail))
    // At the current node in our trie, 
    // for the first value, we'll put None if we have not gotten to a finished word or Some(word) if we have
    // for the second value, the Map[(Char,Trie)],
    // we'll take our Map ( remains.head -> List(word, remains.tail)) and recursively map our makeTrie
    // function over the values.    
  
    val wordPairs = words.map(x => (x,x))
  
    def pairsToMap(pairs : List[(String, String)]) : Map[Char, List[(String, String)]] = {
      pairs
        .map{case (word, remains) => (remains.head, (word, remains.tail))}
        .groupBy(_._1) // group all the tuples whose remains start with the same character
        .mapValues(_.map(_._2)) // project the character out of the values its only in the keys 
        // and the values are just a List[(word, remains)] again
    }
  
  
    def makeTrie(wordsAndRemains : List[(String, String)]) : Trie = {
  
      val continuePairs = wordsAndRemains.filter(_._2 != "")  
      val doneWord = wordsAndRemains.filter(_._2 == "").map(_._1).headOption
      // if there is more than one element in this list, it means the same word
      // appeared more than once in the input so it's okay to take the first
  
      continuePairs match {
        case (x::xs) =>
          val moreWordsMap = pairsToMap(continuePairs)
          // : Map[firstLetter, List(wholeWord, whatsLeft)]
          // : Map[Char, List[(String, String)]]
          T(doneWord, moreWordsMap.mapValues(x => makeTrie(x)))
        case Nil => T(doneWord, Map())
      }  
    }
  
    makeTrie(wordPairs)
  
  }

}


import Trie._

val words = List("yes", "yess", "yaas")
val t = toTrie(words)
val yes = t.lookup("yes")
val no = t.lookup("yeess")
val notAWord = t.lookup("ya")



