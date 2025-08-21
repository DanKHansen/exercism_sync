class Accumulate {
  def accumulate[A, B](f: A => B, list: List[A]): List[B] = {
    myMap(list, f)
  }

  def myMap[A, B](l: List[A], f: A => B): List[B] = {
    var lm: List[B] = Nil
    for (i <- l.indices.reverse) {
      lm = f(l(i)) :: lm
    }
    lm
  }

}
