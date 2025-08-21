import scala.util.Random

class Robot {
  private val sizes: (Int, Int) = (2, 3)
  var name: String = newName

  def reset(): Unit = {
    name = newName
  }

  private def newName: String = {
    makePrefix + makePostfix
  }

  private def makePrefix: String = Random.alphanumeric.filter(_.isLetter).take(sizes._1).mkString.toUpperCase

  private def makePostfix: String = Random.alphanumeric.filter(_.isDigit).take(sizes._2).mkString
}

