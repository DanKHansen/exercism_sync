object Strain:
   def keep[A](seq: Seq[A], predicate: A => Boolean): Seq[A] =
      for elem <- seq if predicate(elem) yield elem 

   def discard[A](seq: Seq[A], predicate: A => Boolean): Seq[A] =
      for elem <- seq if !predicate(elem) yield elem