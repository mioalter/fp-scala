// Use a trie to test to see if a given string q is a substring of s

def makeSuffixes(s : String) : List[String] = {
	val suffixes = for (i <- 0 to (s.length - 1)) yield s.drop(i)
	suffixes.toList
}

def makeSuffixTrie(word : String) : Trie = toTrie(makeSuffixes(word))

val word = "yabba"