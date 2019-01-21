object Acronym {
  def abbreviate(phrase: String): String =
    phrase
      .split("[- ]") // split by word (we assume space or '-' defines the beginning of a new word)
      .filterNot(_.isEmpty) // filter out empty words (this can result from multiple spaces between two words in the phrase)
      .map(_.head) // get first letter of each word
      .map(_.toUpper) // uppercase each letter (we could have done .map(_.head.toUpper) in the previous step)
      .mkString
}
