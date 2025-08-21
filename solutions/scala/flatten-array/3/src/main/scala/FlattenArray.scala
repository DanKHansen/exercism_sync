object FlattenArray:
   def flatten(l: List[Any]): List[Any] =
      l.flatMap {
         case sub: List[?] => flatten(sub)
         case elem         => List(elem)
      }.filter(_ != null)
