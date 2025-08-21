import scala.annotation.tailrec

object Dominoes:
   def chain(l: List[(Int, Int)]): Option[List[(Int, Int)]] =
      if l.isEmpty then Some(Nil)
      else if l.size.eq(1) then if l.head.swap.equals(l.head) then Some(l) else None
      else

         @tailrec
         def loop(xs: List[(Int, Int)], acc: List[(Int, Int)] = Nil): List[(Int, Int)] =
            (xs.headOption, acc.lastOption) match
               case (Some((a, _)), Some((_, b))) if a.eq(b) => loop(xs.tail, acc.appended(xs.head))
               case (Some((_, a)), Some((_, b))) if a.eq(b) => loop(xs.tail, acc.appended(xs.head.swap))
               case (None, _)                               =>
                  (acc.headOption, acc.lastOption) match
                     case (Some((a, _)), Some((_, b))) if a.eq(b) => acc
                     case _                                       => Nil
               case (_, None)                               => loop(xs.tail, acc.appended(xs.head))
               case _                                       => Nil
         l.permutations.map(loop(_)).find(_.nonEmpty)
