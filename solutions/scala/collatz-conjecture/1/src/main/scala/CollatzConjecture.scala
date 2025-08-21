import scala.annotation.tailrec

object CollatzConjecture {
  def steps(x: Int): Option[Int] = {
    @tailrec
    def loop(n: Int, acc: Int): Option[Int] = n match {
      case n if n <= 0 => None
      case 1 => Some(acc)
      case _ => if (n % 2 == 0) loop(n/2, acc + 1) else loop(3*n+1, acc +1)
    }
    loop(x, 0)
  }
}