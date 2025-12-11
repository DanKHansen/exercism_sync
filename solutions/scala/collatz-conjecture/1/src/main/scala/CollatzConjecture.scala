import scala.annotation.tailrec

object CollatzConjecture:
  @tailrec
  def steps(n: Int, acc: Int = 0): Option[Int] =
    n match
      case n if n <= 0 => None
      case 1           => Some(acc)
      case _           =>
        if n % 2 == 0 then steps(n / 2, acc + 1) else steps(3 * n + 1, acc + 1)
