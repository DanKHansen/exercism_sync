import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

class Robot {
  private val sizes: (Int, Int) = (2, 3)
  var name: String = newName

  def reset(): Unit = {
    name = newName
  }

  @tailrec
  final def newName: String = {
    val n: String = makePrefix + makePostfix
    if (userNames.stored.contains(n)) {
      newName
    } else {
      userNames.add(n)
      n
    }
  }

  private def makePrefix: String = Random.alphanumeric.filter(_.isLetter).take(sizes._1).mkString.toUpperCase

  private def makePostfix: String = Random.alphanumeric.filter(_.isDigit).take(sizes._2).mkString
}

object userNames {
  val stored: mutable.HashSet[String] = mutable.HashSet.empty[String]

  def add(n: String): Unit = {
    stored += n
  }

}