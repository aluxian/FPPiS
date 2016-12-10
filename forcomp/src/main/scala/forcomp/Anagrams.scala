package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
    * how often the character appears.
    * This list is sorted alphabetically w.r.t. to the character in each pair.
    * All characters in the occurrence list are lowercase.
    *
    * Any list of pairs of lowercase characters and their frequency which is not sorted
    * is **not** an occurrence list.
    *
    * Note: If the frequency of some character is zero, then that character should not be
    * in the list.
    */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
    * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
    */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
    *
    * Note: the uppercase and lowercase version of the character are treated as the
    * same character, and are represented as a lowercase character in the occurrence list.
    *
    * Note: you must use `groupBy` to implement this method!
    */
  def wordOccurrences(w: Word): Occurrences = {
    w.toLowerCase()
      .groupBy(c => c)
      .map { case (c, chars) => (c, chars.length) }
      .toList
      .sortBy(_._1)
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    if (s.isEmpty) List()
    else s
      .map(wordOccurrences)
      .reduce(_ ++ _)
      .groupBy(_._1)
      .map { case (c, list) => (c, list.map(_._2).sum) }
      .toList
      .sortBy(_._1)
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
    * the words that have that occurrence count.
    * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
    *
    * For example, the word "eat" has the following character occurrence list:
    *
    * `List(('a', 1), ('e', 1), ('t', 1))`
    *
    * Incidentally, so do the words "ate" and "tea".
    *
    * This means that the `dictionaryByOccurrences` map will contain an entry:
    *
    * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
    *
    */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary
      .map(w => (w, wordOccurrences(w)))
      .groupBy(_._2)
      .map { case (occ, words) => (occ, words.map(_._1)) }
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
    * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
    * is a subset of `List(('k', 1), ('o', 1))`.
    * It also include the empty subset `List()`.
    *
    * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
    *
    * List(
    * List(),
    * List(('a', 1)),
    * List(('a', 2)),
    * List(('b', 1)),
    * List(('a', 1), ('b', 1)),
    * List(('a', 2), ('b', 1)),
    * List(('b', 2)),
    * List(('a', 1), ('b', 2)),
    * List(('a', 2), ('b', 2))
    * )
    *
    * Note that the order of the occurrence list subsets does not matter -- the subsets
    * in the example above could have been displayed in some other order.
    */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    val occurrences1 = occurrences.flatMap {
      case (char, count) => for (sz <- 1 to count) yield (char, sz)
    }

    (0 to occurrences1.length)
      .map(occurrences1.combinations)
      .reduce(_ ++ _)
      .toList
      .filter(_.groupBy(_._1).forall(_._2.size <= 1))
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
    *
    * The precondition is that the occurrence list `y` is a subset of
    * the occurrence list `x` -- any character appearing in `y` must
    * appear in `x`, and its frequency in `y` must be smaller or equal
    * than its frequency in `x`.
    *
    * Note: the resulting value is an occurrence - meaning it is sorted
    * and has no zero-entries.
    */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    x
      .map { case (char, count) => (char, count - y.find(_._1 == char).map(_._2).getOrElse(0)) }
      .filter(_._2 > 0)
  }

  /** Returns a list of all anagram sentences of the given sentence.
    *
    * An anagram of a sentence is formed by taking the occurrences of all the characters of
    * all the words in the sentence, and producing all possible combinations of words with those characters,
    * such that the words have to be from the dictionary.
    *
    * The number of words in the sentence and its anagrams does not have to correspond.
    * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
    *
    * Also, two sentences with the same words but in a different order are considered two different anagrams.
    * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
    * `List("I", "love", "you")`.
    *
    * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
    *
    * List(
    * List(en, as, my),
    * List(en, my, as),
    * List(man, yes),
    * List(men, say),
    * List(as, en, my),
    * List(as, my, en),
    * List(sane, my),
    * List(Sean, my),
    * List(my, en, as),
    * List(my, as, en),
    * List(my, sane),
    * List(my, Sean),
    * List(say, men),
    * List(yes, man)
    * )
    *
    * The different sentences do not have to be output in the order shown above - any order is fine as long as
    * all the anagrams are there. Every returned word has to exist in the dictionary.
    *
    * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
    * so it has to be returned in this list.
    *
    * Note: There is only one anagram of an empty sentence.
    */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    if (sentence.isEmpty) List(List())
    else {
      def find(occurrences: Occurrences): List[Sentence] = {
        if (occurrences.isEmpty) List()
        else for {
          occ <- combinations(occurrences) if dictionaryByOccurrences.contains(occ)
          word <- dictionaryByOccurrences(occ)
          sentence <- {
            val subtracted = subtract(occurrences, occ)
            val rest = find(subtracted)
            if (rest.isEmpty) {
              if (subtracted.isEmpty) {
                val sentence = List(word)
                List(sentence)
              } else {
                List()
              }
            } else {
              rest.map(s => word +: s)
            }
          }
        } yield sentence
      }

      find(sentenceOccurrences(sentence))
    }
  }

  // We will transform the sentence into its occurrence list, then try to extract any subset of characters from it
  // to see if we can form any meaningful words. From the remaining characters we will solve the problem recursively
  // and then combine all the meaningful words we have found with the recursive solution.

  //    Let's apply this idea to our example, the sentence `You olive`. Lets represent this sentence as an occurrence
  // list of characters `eiloouvy`. We start by subtracting some subset of the characters, say `i`. We are left with
  // the characters `eloouvy`.
  //
  //      Looking into the dictionary we see that `i` corresponds to word `I` in the English language, so we found
  // one meaningful word. We now solve the problem recursively for the rest of the characters `eloouvy` and obtain a
  // list of solutions `List(List(love, you), List(you, love))`. We can combine`I` with that list to obtain sentences
  // `I love you` and `I you love`, which are both valid anagrams.


  //    def generate(numWords: Int): List[Sentence] = {
  //
  //    }
  //
  //    generate()


  //    List(List())
  //    else {
  //      val occurrences = sentenceOccurrences(sentence)
  //
  //    }


  //    goodCombs
}
