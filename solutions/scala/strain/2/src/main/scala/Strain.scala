object Strain:
   def keep[A](seq: Seq[A], predicate: A => Boolean): Seq[A] =
      seq.filter(predicate)

   def discard[A](seq: Seq[A], predicate: A => Boolean): Seq[A] =
      seq.filterNot(predicate)