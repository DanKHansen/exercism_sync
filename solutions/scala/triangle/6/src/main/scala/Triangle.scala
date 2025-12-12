case class Triangle(a: Double, b: Double, c: Double):
  private def isValid: Boolean = a + b >= c && b + c >= a && a + c >= b && List(a, b, c).forall(_ > 0)
  def equilateral: Boolean = isValid && Set(a, b, c).size == 1
  def isosceles: Boolean = isValid && Set(a, b, c).size < 3
  def scalene: Boolean = isValid && Set(a, b, c).size == 3