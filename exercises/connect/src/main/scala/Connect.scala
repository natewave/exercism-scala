
sealed trait Color
object Color {
  case object Black extends Color
  case object White extends Color
  case object NonAssigned extends Color

  def apply(c: Char): Color = {
    c match {
      case 'X' => Color.Black
      case 'O' => Color.White
      case _ => Color.NonAssigned
    }
  }
}

case class Point(y: Int, x: Int, value: Char)

case class Connect(board: List[String]) {

  // calculates connections from Point to points in next line
  def connections(point: Point, line: List[Point]): List[Point] = {
    val sameValuePoints = line.toList.filter(_.value == point.value)

    // only keep those connected to `point`
    sameValuePoints.filter { p => 
      (p.y == point.y + 1) && (p.x == point.x || p.x == point.x - 1 || p.x == point.x +1 )
    }
  }

  // calculates connections from Point to points in same line
  def neighbors(point: Point, line: List[Point]): List[Point] = {
    // only keep those connected to `point`
    val right = line.drop(point.x + 1).takeWhile(_.value == point.value)

    val left = line.reverse.drop(line.size - point.x + 1).takeWhile(_.value == point.value)

    right ++ left
  }

  def pointsHaveConnection(b: List[String], boardSize: Int, value: Char, previous: List[Point] = Nil): Boolean = {
    b match {
        // no lines in board
        case Nil => {
          if (previous.isEmpty) false else true
        }

        // line found in board
        case line :: tail => {
          // check previous
          previous match {
            // empty previous, either no connections found between lines or start of the calculations
            case Nil => {
              // check if start of calculations
              if (b.size != boardSize) {
                // not the start of calculations, so no connection between lines
                false
              } else {
                // start of the calculations, calculate current line
                val points = line.zipWithIndex.map { case (value, i) =>
                  Point(0, i, value)
                }.filter(_.value == value).toList

                if (points.isEmpty) false
                else pointsHaveConnection(tail, boardSize, value, points)
              }
            }

            // previous found, so far connections have been found between lines
            case _ => {

              val nextPath = previous.flatMap { p =>
                // transform next line into List[Point]
                val points = line.zipWithIndex.map { case (value, i) =>
                  Point(p.y + 1, i, value)
                }

                // filter on value: Char
                val filteredPoints = points.filter(_.value == value).toList

                // get direct connections
                val next = previous.flatMap { previousPoint =>
                  connections(previousPoint, filteredPoints)
                }

                // get connected points to direct connections
                val allConnections = next.flatMap { conn =>
                  conn :: neighbors(conn, points.toList)
                }

                allConnections
              }.distinct

              if (nextPath.isEmpty) false
              else {
                pointsHaveConnection(tail, boardSize, value, nextPath)
              }
            }
          }
        }
    }
  }

  def winner: Option[Color] = {
    // quick known no winner states
    val hasMoves = board.filter(line => line.contains("O") || line.contains("X"))

    if (hasMoves.isEmpty)
     // no moves, no winner
     None
    else {
      val transposed = board.transpose.map(_.mkString.reverse)

      val oWon = pointsHaveConnection(board, board.size, 'O')
      val xWon = pointsHaveConnection(transposed, transposed.size, 'X')

      if (xWon && oWon) None
      else if (xWon) Some(Color.Black)
      else if (oWon) Some(Color.White)
      else None
    }
  }
}