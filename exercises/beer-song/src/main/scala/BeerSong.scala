object BeerSong {
  def recite(bottles: Int, times: Int): String = {
    (0 until times).map { time =>
      howMuch(bottles - time)
    }.mkString("\n")
  }

  private def howMuch(bottles: Int): String =
  bottles match {
    case 0 => "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"
    case 1 => "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall.\n"
    case 2 => "2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottle of beer on the wall.\n"
    case _ => s"$bottles bottles of beer on the wall, $bottles bottles of beer.\nTake one down and pass it around, ${bottles-1} bottles of beer on the wall.\n"
  }
}