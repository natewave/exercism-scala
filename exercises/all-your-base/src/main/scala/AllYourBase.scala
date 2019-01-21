object AllYourBase {
  def rebase(inBase: Int, number: List[Int], outBase: Int): Option[List[Int]] = {
    if (inBase < 2 || outBase < 2) None // impossible conversion
    else if (number.isEmpty || number == List(0)) Some(List(0))
    else toDecimal(inBase, number)
      .map(d => toBase(targetBase = outBase, decimal = d))
  }

  // convert number list representation from any base to base 10.
  private def toDecimal(base: Int, number: List[Int]): Option[Int] = {
    number match {
      case Nil => Some(0)
      case head :: tail => {
        if (head >= base || head < 0) None // corrupted representation in specified base
        else for {
          next <- toDecimal(base, tail)
          current = (head * Math.pow(base, tail.size)).toInt
        } yield current + next
      }
    }
  }

  private def toBase(acc: List[Int] = Nil, targetBase: Int, decimal: Int): List[Int] = {
    if (decimal == 0) {
      if (acc.isEmpty) List(0) else acc
    }
    else toBase((decimal % targetBase) :: acc, targetBase, decimal/targetBase)
  }
}