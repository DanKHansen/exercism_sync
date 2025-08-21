import scala.annotation.tailrec

object Wordy:
   def answer(s: String): Option[Int] =
      val str = s.split("[ ?]").drop(2).filterNot(_ == "by").toList
      @tailrec
      def loop(l: List[String], agg: Int): Option[Int] =
         l match
            case Nil                                       => Some(agg)
            case head :: n :: next if head == "plus"       => loop(next, agg + n.toInt)
            case head :: n :: next if head == "minus"      => loop(next, agg - n.toInt)
            case head :: n :: next if head == "multiplied" => loop(next, agg * n.toInt)
            case head :: n :: next if head == "divided"    => loop(next, agg / n.toInt)
            case _                                         => None

      loop(str.tail, str.head.toIntOption.getOrElse(0))
