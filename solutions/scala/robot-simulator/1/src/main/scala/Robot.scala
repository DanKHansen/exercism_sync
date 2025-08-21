case class Robot(bearing: Bearing.Value, coordinates: (Int, Int)) {
  private val p = coordinates
  private val b = bearing

  def simulate(cmd: String): Robot = RobotSimulator.simulate(this, cmd)

  def advance: Robot = Robot(b, move)

  private def move: (Int, Int) = b match {
    case Bearing.North => (p._1, p._2 + 1)
    case Bearing.West => (p._1 - 1, p._2)
    case Bearing.South => (p._1, p._2 - 1)
    case Bearing.East => (p._1 + 1, p._2)
    case _ => (0, 0)
  }

  def turnRight: Robot = Robot(Bearing + b, p)

  def turnLeft: Robot = Robot(Bearing - b, p)

}

object Bearing extends Enumeration {
  type Face = Value
  val North, East, South, West = Value

  def +(b: Face): Value = if (b.id < 3) Bearing(b.id + 1) else North

  def -(b: Face): Value = if (b.id > 0) Bearing(b.id - 1) else West
}