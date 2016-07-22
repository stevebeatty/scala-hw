package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = {
    {
      for {
        m <- w.toLowerCase.toList.groupBy { x => x }
      } yield (m._1, m._2.length)
    }.toList.sortBy(e => e._1)
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    if (s.isEmpty) Nil
    else {
      val all = s.map( x => wordOccurrences(x) ).reduce( (a, b) => a.union(b) )
      val gpd = for {
        g <- all.groupBy { c => c._1 }
      } yield {
        (g._1, g._2.reduce( (x, y) => (g._1, x._2 + y._2) )._2)
      }
      gpd.toList.sortBy(e => e._1)
    }
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    val m = for {
      w <- dictionary
    } yield wordOccurrences(w) -> w
    
    val y = for {
      z <- m.groupBy { g => g._1 }
    } yield z._1 -> z._2.map( i => i._2 )

    y withDefault { x => List() }
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)).get

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    val a = for {
      j <- occurrences
      i <- 1 to j._2
    } yield (j._1, i)
    
    def comb(m: List[(Char, List[(Char, Int)])],test: Occurrences): List[Occurrences] = {
     if (m.isEmpty) {
       if (test.isEmpty) Nil
       else List(test.sortWith( _._1 < _._1 ))
     } else {
      val a = for {
        i <- m.head._2
      } yield comb(m.tail, i :: test)

      comb(m.tail, test) ++ a.flatten
     }
    }
    
    List() :: comb(a.groupBy(_._1).toList, Nil)
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val m = y.toMap.withDefaultValue(0)
    val l = for {
      o <- x
    } yield (o._1, o._2 - m(o._1))
    
    l.filter( _._2 > 0 )
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val o = sentenceOccurrences(sentence)
    val c = combinations(o)
    val f = for {
      i <- c
      if dictionaryByOccurrences.contains(i)
    } yield i
    
    def isSubset(sub: Occurrences, sup: Map[Char, Int]): Boolean = {
      sub.forall(x => sup.contains(x._1) && sup(x._1) >= x._2)
    }
    
    def makeList(l: List[List[Word]], acc: List[List[Word]]): List[List[Word]] = {
      if (l.isEmpty) acc
      else if (acc.isEmpty) {
        val n = for (i <- l.head) yield List(i)
        makeList(l.tail, n)
      } else {
        val a = for {
          b <- acc
          s <- l.head
        } yield s :: b
        
        makeList(l.tail, a)
      }
    }
    
    def anagrams(left: Occurrences, pos: List[Occurrences], acc: List[Occurrences]): List[Sentence] = {
      if (left.isEmpty) {
        val l = acc.map(x => dictionaryByOccurrences(x))
        makeList(l, List(Nil))
      }
      else {
        val m = left.toMap
        val r = for {
          i <- pos
          if isSubset(i, m)
        } yield anagrams(subtract(left, i), pos, i :: acc)
        r.flatten
      }
    }
    
    anagrams(o, f, Nil)
  }
}
