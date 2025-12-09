import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object RobotName:
  private val reservedNames: mutable.Set[String] = mutable.Set.empty

  private def generateRandomName: String =
    def randomLetter = ('A' + Random.nextInt(26)).toChar
    def randomDigit = Random.nextInt(10)
    s"$randomLetter$randomLetter$randomDigit$randomDigit$randomDigit"

  @tailrec
  def allocateUniqueName: String =
    val candidate = generateRandomName
    if reservedNames.contains(candidate) then
      allocateUniqueName
    else
      reservedNames += candidate
      candidate

class Robot:
  private var currentName: String = RobotName.allocateUniqueName
  def name: String = currentName
  def reset(): Unit = currentName = RobotName.allocateUniqueName
