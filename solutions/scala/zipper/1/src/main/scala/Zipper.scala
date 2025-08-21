import scala.annotation.tailrec
object Zipper:
   case class Zipper[A](focus: BinTree[A], up: Option[Zipper[A]] = None, isLeft: Boolean = false)
   def fromTree[A](bt: BinTree[A]): Zipper[A] = Zipper(bt)
   @tailrec
   def toTree[A](zipper: Zipper[A]): BinTree[A] =
      zipper.up match
         case Some(p) if zipper.isLeft  => toTree(p.copy(p.focus.copy(left = Some(zipper.focus))))
         case Some(p) if !zipper.isLeft => toTree(p.copy(p.focus.copy(right = Some(zipper.focus))))
         case _                         => zipper.focus
   def value[A](zipper: Zipper[A]): A = zipper.focus.value
   def left[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper.focus.left.map(ch => Zipper(ch, Some(zipper), true))
   def right[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper.focus.right.map(ch => Zipper(ch, Some(zipper)))
   def up[A](zipper: Zipper[A]): Option[Zipper[A]] = zipper.up
   def setValue[A](v: A, zipper: Zipper[A]): Zipper[A] = zipper.copy(zipper.focus.copy(value = v))
   def setLeft[A](l: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] = zipper.copy(zipper.focus.copy(left = l))
   def setRight[A](r: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] = zipper.copy(zipper.focus.copy(right = r))

case class BinTree[A](value: A, left: Option[BinTree[A]], right: Option[BinTree[A]])
