import scala.annotation.tailrec

case class Bst[T](value: T, left: Option[Bst[T]], right: Option[Bst[T]]):
   def insert(v: T): Bst[T] =
      if v.hashCode <= value.hashCode
      then Bst(value, Some(left.map(_.insert(v)).getOrElse(Bst(v))), right)
      else Bst(value, left, Some(right.map(_.insert(v)).getOrElse(Bst(v))))

   def toList: List[T] =
      left
         .map(_.toList)
         .getOrElse(Nil)
         .concat(List(value))
         .concat(right.map(_.toList).getOrElse(Nil))

object Bst:
   def apply[T](v: T): Bst[T] = Bst(v, None, None)

   def fromList[T](l: List[T]): Bst[T] =
      @tailrec
      def insertAll(bst: Bst[T], l: List[T]): Bst[T] =
         l match
            case Nil            => bst
            case ::(head, tail) => insertAll(bst.insert(head), tail)
      insertAll(Bst(l.head), l.tail)

   def toList[T](bst: Bst[T]): List[T] = bst.toList
