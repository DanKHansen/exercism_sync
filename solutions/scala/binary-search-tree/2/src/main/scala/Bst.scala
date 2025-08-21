case class Bst[T](value: T, left: Option[Bst[T]], right: Option[Bst[T]]):
   def insert(v: T): Bst[T] =
      if v.## <= value.##
      then copy(left = Some(left.map(_.insert(v)).getOrElse(Bst(v))))
      else copy(right = Some(right.map(_.insert(v)).getOrElse(Bst(v))))

object Bst:
   def apply[T](v: T): Bst[T] = Bst(v, None, None)

   def fromList[T](l: List[T]): Bst[T] =
      l.tail.foldLeft(Bst(l.head))((tree, value) => tree.insert(value))

   def toList[T](bst: Bst[T]): List[T] =
      bst.left.toList.flatMap(toList) ::: bst.value :: bst.right.toList.flatMap(toList)
