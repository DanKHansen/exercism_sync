import scala.annotation.tailrec

object AllYourBase:
   // @tailrec
   def rebase(fromBase: Int, number: List[Int], toBase: Int): Option[List[Int]] =
      def toDec =
         (for c <- (for n <- number.zip(number.indices.reverse)
                   yield n._1 * math.pow(fromBase, n._2).toInt).sum.toString
         yield c.toString.toInt).toList

      @tailrec
      def fromDec(n: List[Int], rem: List[Int]): List[Int] =
         val q = n.mkString.toInt / toBase
         val r = n.mkString.toInt % toBase
         if q == 0 then r :: rem else fromDec(List(q), r :: rem)

      if fromBase <= 1 || toBase <= 1 || number.exists(_ < 0) || number.exists(_ >= fromBase) then None
      else if number.isEmpty then Some(List(0))
      else if toBase == 10 then Some(toDec)
      else Some(fromDec(toDec, Nil))
