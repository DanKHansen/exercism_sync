import scala.annotation.tailrec

object SecretHandshake {
  val wordList: List[String] =
    List("wink", "double blink", "close your eyes", "jump")

  def commands(i: Int): List[String] = {
    if (i > 31) Nil
    else
      loop(i.toBinaryString.reverse, Nil, 0)
  }

  @tailrec
  private def loop(
      binStr: String,
      buffer: List[String],
      idx: Int
  ): List[String] = {
    val result = {
      if (idx == 4) buffer.reverse
      else
        binStr.head match {
          case '1' => buffer :+ wordList(idx)
          case _   => buffer
        }
    }
    if (binStr.tail.nonEmpty) {
      loop(binStr.tail, result, idx + 1)
    } else {
      result
    }

  }
}
