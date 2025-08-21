import scala.annotation.tailrec

case class Bowling(rolls: List[Int] = Nil):
   def roll(pins: Int): Bowling = Bowling(pins +: rolls)

   def score(): Either[String, Int] =
      @tailrec
      def loop(rs: List[Int], f: Int, acc: Int): Either[String, Int] =
         (f, rs) match
            case (11, Nil)                                          => Right(acc)
            case (10, 10 :: 10 :: br3 :: tail) if br3 <= 10         => loop(tail, f + 1, 10 + 10 + br3 + acc)
            case (10, 10 :: br2 :: br3 :: tail) if br2 + br3 <= 10  => loop(tail, f + 1, 10 + br2 + br3 + acc)
            case (10, br1 :: br2 :: br3 :: tail) if br1 + br2 == 10 => loop(tail, f + 1, br1 + br2 + br3 + acc)
            case (_, 10 :: 10 :: r1 :: tail)                        => loop(10 :: r1 :: tail, f + 1, 10 + 10 + r1 + acc)
            case (_, 10 :: r1 :: r2 :: tail)                        => loop(r1 :: r2 :: tail, f + 1, 10 + r1 + r2 + acc)
            case (_, r1 :: r2 :: r3 :: tail) if r1 + r2 == 10       => loop(r3 :: tail, f + 1, r1 + r2 + r3 + acc)
            case (_, r1 :: r2 :: tail) if r1 + r2 < 10              => loop(tail, f + 1, r1 + r2 + acc)
            case _                                                  => Left("TheEnd!")
      loop(rolls.reverse, 1, 0)
