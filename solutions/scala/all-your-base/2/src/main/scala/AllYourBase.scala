import scala.annotation.tailrec

object AllYourBase:
   def rebase(fromBase: Int, number: List[Int], toBase: Int): Option[List[Int]] =
      if fromBase <= 1 || toBase <= 1 || number.exists(n => n < 0 || n >= fromBase) then None
      else if number.isEmpty then Some(List(0))
      else
         val decimalValue = number.zipWithIndex.map { case (digit, index) =>
            digit * math.pow(fromBase, number.length - 1 - index).toInt
         }.sum

         @tailrec
         def fromDec(n: Int, rem: List[Int]): List[Int] =
            if n == 0 && rem.isEmpty then List(0)
            else if n == 0 then rem
            else fromDec(n / toBase, (n % toBase) :: rem)

         if toBase == 10 then Some(decimalValue.toString.map(_.asDigit).toList)
         else Some(fromDec(decimalValue, Nil))
