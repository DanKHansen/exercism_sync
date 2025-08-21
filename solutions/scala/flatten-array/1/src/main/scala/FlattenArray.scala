object FlattenArray:

   def flatten(l: List[Any]): List[Any] =
      l.filter(_ != null).flatMap {
         case sub: List[?] => flatten(sub)
         case elem         => List(elem)
      }