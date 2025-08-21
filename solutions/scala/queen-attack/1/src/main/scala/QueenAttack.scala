case class Queen(x: Int, y: Int)

object Queen:
   def create(row: Int, col: Int): Option[Queen] =
      if row < 0 || col < 0 || row > 7 || col > 7 then None
      else Some(Queen(row, col))

object QueenAttack:
   def canAttack(q1: Queen, q2: Queen): Boolean =
      q1.x == q2.x || q1.y == q2.y || (q1.x - q2.x).abs == (q1.y - q2.y).abs
