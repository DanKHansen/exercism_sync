import scala.annotation.tailrec
object Zipper:
   case class Zipper[A](focus: BinTree[A], up: Option[Zipper[A]] = None, isLeft: Boolean = false)
   def fromTree[A](bt: BinTree[A]): Zipper[A] = Zipper(bt)
   @tailrec
   def toTree[A](z: Zipper[A]): BinTree[A] = z.up match
      case Some(p) => toTree(p.copy(p.focus.copy(
         left = if(z.isLeft) Some(z.focus) else p.focus.left,
         right = if(!z.isLeft) Some(z.focus) else p.focus.right)))
      case _ => z.focus
   def value[A](z: Zipper[A]): A = z.focus.value
   def left[A](z: Zipper[A]): Option[Zipper[A]] = z.focus.left.map(Zipper(_, Some(z), true))
   def right[A](z: Zipper[A]): Option[Zipper[A]] = z.focus.right.map(Zipper(_, Some(z)))
   def up[A](z: Zipper[A]): Option[Zipper[A]] = z.up
   def setValue[A](v: A, z: Zipper[A]): Zipper[A] = z.copy(z.focus.copy(value = v))
   def setLeft[A](l: Option[BinTree[A]], z: Zipper[A]): Zipper[A] = z.copy(z.focus.copy(left = l))
   def setRight[A](r: Option[BinTree[A]], z: Zipper[A]): Zipper[A] = z.copy(z.focus.copy(right = r))
case class BinTree[A](value: A, left: Option[BinTree[A]], right: Option[BinTree[A]])
