import scala.annotation.tailrec
object VariableLengthQuantity:

   def encode(ns: List[Int]): List[Int] =
      def f(n: Int): List[Int] =
         n.toBinaryString.reverse
            .grouped(7)
            .toList
            .reverse
            .map(_.reverse)
            .map(s => "0" * (7 - s.length) + s)
            .reverse
            .zipWithIndex
            .map { case (s, idx) => if idx == 0 then "0" + s else "1" + s }
            .map(Integer.parseUnsignedInt(_, 2))
            .reverse

      ns.flatMap(f)

   def decode(ns: List[Int]): Either[Exception, List[Int]] =
      def f(ns: List[Int]): List[Int] =
         Integer.parseUnsignedInt(
           ns.map(_.toBinaryString.grouped(8).mkString).map(s => "0" * (8 - s.length) + s).map(_.drop(1)).mkString,
           2) :: Nil

      val b = ns.map(_.toBinaryString.grouped(8).toList)
      (b.length, b.head) match
         case (1, ::(h, _)) if h.length > 7 && h(0) == '1' => Left(new Exception())
         case (1, ::(h, t)) if h.length < 8                => Right((h :: t).map(Integer.parseInt(_, 2)))
         case _                                            => Right(splitAtUnder128(ns).flatMap(f))

@tailrec
private def splitAtUnder128(list: List[Int], current: List[Int] = Nil, acc: List[List[Int]] = Nil): List[List[Int]] =
   list match
      case Nil =>
         if current.nonEmpty then (current :: acc).reverse
         else acc.reverse
      case head :: tail if head < 128 =>
         val newSegment = (head :: current).reverse
         splitAtUnder128(tail, Nil, newSegment :: acc)
      case head :: tail =>
         splitAtUnder128(tail, head :: current, acc)
