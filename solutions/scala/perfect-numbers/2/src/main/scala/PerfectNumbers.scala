import NumberType.{Abundant, Deficient, Perfect}
enum NumberType:
   case Perfect, Abundant, Deficient
object PerfectNumbers:
   def classify(n: Int): Either[String, NumberType] =
      if n <= 0 then Left("Classification is only possible for natural numbers.")
      else
         lazy val aliqout = (1 until n).filter(i => n % i == 0).sum
         aliqout match
            case p if p == n => Right(Perfect)
            case a if a > n  => Right(Abundant)
            case d if d < n  => Right(Deficient)
