object Anagram {
  def findAnagrams(word: String, words: List[String]): List[String] = {
    val lowerWord = word.toLowerCase
    val sortedWord = lowerWord.sorted

    words.filter { w =>
      val toLower = w.toLowerCase

      toLower != lowerWord && toLower.sorted == sortedWord
    }
  }
}