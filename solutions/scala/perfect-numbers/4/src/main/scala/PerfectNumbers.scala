object PerfectNumbers:
   def classify(n: Int): Either[String, NumberType] = if n <= 0 then
      Left("Classification is only possible for natural numbers.")
   else
      (1 until n).filter(i => n % i == 0).sum match
         case p if p == n => Right(NumberType.Perfect)
         case a if a > n  => Right(NumberType.Abundant)
         case _           => Right(NumberType.Deficient)

enum NumberType:
   case Perfect, Abundant, Deficient