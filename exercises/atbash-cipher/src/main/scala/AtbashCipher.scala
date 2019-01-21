import scala.util.{ Try, Success, Failure }

object AtbashCipher {
  val plain = "abcdefghijklmnopqrstuvwxyz"
  val cipher = "zyxwvutsrqponmlkjihgfedcba"

  def switch(c: Char, fromDict: String, toDict: String): Try[Char] = Try {
    if (c.isDigit) c
    else toDict.charAt(fromDict.indexOf(c.toLower))
  }

  def keepEncodable(word: String): String = word.filter(w => w.isLetter || w.isDigit)

  // effect should have been encoded in the result but tests impose a string type
  def encode(word: String): String = {
    val encoded = keepEncodable(word)
      .map(w => switch(w, plain, cipher)).toList
      .collect { case Success(c) => c }
      .mkString("")
      .grouped(5)
      .mkString(" ")

    encoded
  }

  def decode(word: String): String = {
    keepEncodable(word)
      .map(w => switch(w, cipher, plain)).toList
      .collect { case Success(c) => c }
      .mkString("")
  }
}