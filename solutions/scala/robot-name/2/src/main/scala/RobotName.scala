import scala.util.Random
object Robot:
  private def makeName = {
    LazyList
      .continually(('A' to 'Z')(Random.nextInt(26)))
      .take(2)
      .map(_.toUpper) ++ LazyList
      .continually(Random.nextInt(10))
      .take(3)
  }.mkString

class Robot:
  private var robotName: String = Robot.makeName
  def name: String = this.robotName
  def reset(): Unit = robotName = Robot.makeName
