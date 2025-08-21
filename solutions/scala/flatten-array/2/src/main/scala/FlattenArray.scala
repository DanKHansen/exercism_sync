object FlattenArray:
   def flatten(l: List[Any]): List[Any] =
      l.withFilter(_ != null).flatMap {
         case sub: List[?] => flatten(sub)
         case elem         => List(elem)
      }
