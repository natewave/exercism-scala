import scala.util.Try

object Say {

  val teens = IndexedSeq("", "one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen")

  val tens = IndexedSeq("", "", "twenty", "thirty", "forty", "fifty", "sixty",
    "seventy", "eighty", "ninety")
  
  val scales = IndexedSeq("", "thousand", "million",  "billion")

  // numbers 0 through 99
  def basic(n: Int): String = {
    if (n == 0) ""
    else if (n <= 19) teens(n)
    else {
      val s = n.toString

      val decimals = teens(s.charAt(1).asDigit)

      val t =  tens(s.charAt(0).asDigit)
      if (decimals.isEmpty) t
      else t + "-" + decimals
    }
  }

  // handling chunks of 3
  def chunks(n: Int): String = {
    if (n == 0) ""

    if (n <= 99) basic(n)
    else {
      val s = n.toString
      val hundreds = basic(s.charAt(0).asDigit) + " hundred"
      val t = basic(s.drop(1).toInt)


      if (t.isEmpty) hundreds
      else s"$hundreds $t"
    }
  }

  def inEnglish(number: Long): Option[String] = {
    if (number < 0 || number > 999999999999L) None
    else if (number == 0L) Some("zero")
    else {
      val result = Try(number.toString.reverse.grouped(3).zipWithIndex.map { case (group, index) =>
        val chunk = group.reverse
        val hundred = chunks(chunk.toInt)
        val scale = scales(index)

        if (hundred.isEmpty || scale.isEmpty) hundred
        else s"$hundred $scale"
      }.toList.reverse.filterNot(_ == "").mkString(" "))

      result.toOption
    }
  }
}