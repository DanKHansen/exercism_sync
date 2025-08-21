import scala.math.pow

object ArmstrongNumbers {
  def isArmstrongNumber(i: Int): Boolean = {
    val p: Int = i.toString.length
    val l: List[Int] = i.toString.toList.map(_.toString.toInt)
    i == l.map(pow(_, p)).sum
  }
}