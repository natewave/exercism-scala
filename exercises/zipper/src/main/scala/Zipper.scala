// A zipper for a binary tree.
// ??? Zipper[A] ???

case class Zipper[A](
  focusTree: BinTree[A],
  moves: List[Move[A]])

// Moves
trait Move[A]
case class MovedLeft[A](value: A, right: Option[BinTree[A]]) extends Move[A]
case class MovedRight[A](value: A, left: Option[BinTree[A]]) extends Move[A]

object Zipper {
  // Get a zipper focussed on the root node.
  def fromTree[A](bt: BinTree[A]): Zipper[A] = Zipper(bt, Nil)

  // Get the complete tree from a zipper.
  def toTree[A](zipper: Zipper[A]): BinTree[A] = {
    up(zipper) match {
      case Some(parent) => toTree(parent)
      case None => zipper.focusTree
    }
  } 

  // Get the value of the focus node.
  def value[A](zipper: Zipper[A]): A = zipper.focusTree.value

  // Get the left child of the focus node, if any.
  def left[A](zipper: Zipper[A]): Option[Zipper[A]] = {
    val ft = zipper.focusTree
    val m  = zipper.moves


    ft.left match {
      case Some(tree) => Some(zipper.copy(
        focusTree = tree,
        moves = MovedLeft(ft.value, ft.right) :: m
      ))
      case None => None
    }
  }

  // Get the right child of the focus node, if any.
  def right[A](zipper: Zipper[A]): Option[Zipper[A]] = {
    val ft = zipper.focusTree
    val m  = zipper.moves


    ft.right match {
      case Some(tree) => Some(zipper.copy(
        focusTree = tree,
        moves = MovedRight(ft.value, ft.left) :: m
      ))
      case None => None
    }


  }

  // Get the parent of the focus node, if any.
  def up[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper.moves.headOption  match {
    case Some(move) => move match {
      case MovedLeft(v, r) => Some(zipper.copy(
        focusTree = BinTree(v, Some(zipper.focusTree), r),
        moves = zipper.moves.tail
      ))
      case MovedRight(v, l) => Some(zipper.copy(
        focusTree = BinTree(v, l, Some(zipper.focusTree)),
        moves = zipper.moves.tail
      ))
    }
    case None => None
  }

  // Set the value of the focus node.
  def setValue[A](v: A, zipper: Zipper[A]): Zipper[A] = zipper.copy(
    focusTree = zipper.focusTree.copy(value = v)
  )

  // Replace a left child tree.
  def setLeft[A](l: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] = zipper.copy(
    focusTree = zipper.focusTree.copy(left = l)
  )

  // Replace a right child tree.
  def setRight[A](r: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] = zipper.copy(
    focusTree = zipper.focusTree.copy(right = r)
  )
}

// A binary tree.
case class BinTree[A](value: A, left: Option[BinTree[A]], right: Option[BinTree[A]])
