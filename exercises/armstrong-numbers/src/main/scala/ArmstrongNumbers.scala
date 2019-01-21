object ArmstrongNumbers {
  def isArmstrongNumber(number: Int) = {
    val digits = number.toString.map(_.asDigit)
    val size = digits.size

    number == digits.map(Math.pow(_, size)).sum
  }
}