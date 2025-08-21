enum Sublist:
   case Equal, Unequal, Superlist, Sublist

object Sublist:
   def sublist[T](l1: List[T], l2: List[T]): Sublist =
      (l1, l2) match
         case (Nil, Nil) | _ if l1 == l2 => Equal
         case _ if l1.containsSlice(l2)  => Superlist
         case _ if l2.containsSlice(l1)  => Sublist
         case _                          => Unequal
