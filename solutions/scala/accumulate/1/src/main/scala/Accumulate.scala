class Accumulate {
  def accumulate[A, B](f: A => B, list: List[A]): List[B] = {
    { for (i <- list.indices) yield { f(list(i)) } }.toList
  }
}
