class Accumulate {
  def accumulate[A, B](f: (A) => B, list : List[A]): List[B] = list match {
    // list is not empty, we apply `f` to `head` and continue accumulating tail
    case head :: tail => f(head) +: accumulate(f, tail)
    // list is empty, do nothing
    case Nil          => List.empty[B]
  }

  // solution 2
  def accumulate2[A, B](f: (A) => B, list : List[A]): List[B] = list.headOption match {
    case Some(head) => f(head) +: accumulate(f, list.tail)
    case None => List.empty[B]
  }
}
