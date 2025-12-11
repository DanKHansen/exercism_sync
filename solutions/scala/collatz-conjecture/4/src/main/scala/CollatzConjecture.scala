object CollatzConjecture:
  def steps(n: Int): Option[Int] =
    Option.when(n > 0) {
      Iterator
        .iterate(n)(i => if i % 2 == 0 then i / 2 else 3 * i + 1)
        .takeWhile(_ != 1)
        .size
    }
