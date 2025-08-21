import NumberType.{Abundant, Deficient, Perfect}

enum NumberType:
   case Perfect, Abundant, Deficient

object PerfectNumbers:
   def classify(n: Int): Either[String, NumberType] =
      if n <= 0 then Left("Classification is only possible for natural numbers.")
      else if n == 1 then Right(Deficient)
      else
         val sum = (1 to math.sqrt(n).toInt).foldLeft(0) { (s, i) =>
            s + (if n % i == 0 then i + (if i != 1 && i != n / i then n / i else 0) else 0)
         }
         if sum == n then Right(Perfect)
         else if sum > n then Right(Abundant)
         else Right(Deficient)
