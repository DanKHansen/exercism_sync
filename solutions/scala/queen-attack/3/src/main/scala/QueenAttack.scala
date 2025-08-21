case class Queen(x: Int, y: Int)

object Queen:
   def create(x: Int, y: Int): Option[Queen] =
      Option.when(Seq(x, y).forall(n => (0 to 7).contains(n)))(Queen(x, y))

object QueenAttack:
   def canAttack(q1: Queen, q2: Queen): Boolean =
      q1.x == q2.x || q1.y == q2.y || (q1.x - q2.x).abs == (q1.y - q2.y).abs
