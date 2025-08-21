case class Triangle(a: Double, b: Double, c: Double) {
  val l: List[Double] = List(a, b, c)
  val isTriangle: Boolean = l.forall(_ > 0 == true) && l.max < l.sum / 2
  // all sides are equal
  val equilateral: Boolean = isTriangle && l.forall(_ == l.head)
  // at least two sides are equal
  val isosceles: Boolean = isTriangle && (a == b || a == c || c == b)
  // all sides different
  val scalene: Boolean = isTriangle && (a != b && b != c)
}