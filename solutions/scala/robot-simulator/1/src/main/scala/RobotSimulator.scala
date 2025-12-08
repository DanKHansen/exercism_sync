enum Bearing:
  case North, East, South, West

case class Robot(bearing: Bearing, coordinates: (Int, Int)):
  def turnRight: Robot =
    Robot(Bearing.fromOrdinal((bearing.ordinal + 1) % 4), coordinates)

  def turnLeft: Robot =
    Robot(Bearing.fromOrdinal((bearing.ordinal - 1 + 4) % 4), coordinates)

  def advance: Robot =
    bearing match
      case Bearing.North => Robot(bearing, (coordinates._1, coordinates._2 + 1))
      case Bearing.East  => Robot(bearing, (coordinates._1 + 1, coordinates._2))
      case Bearing.South => Robot(bearing, (coordinates._1, coordinates._2 - 1))
      case Bearing.West  => Robot(bearing, (coordinates._1 - 1, coordinates._2))

  def simulate(instructions: String): Robot =
    instructions.foldLeft(this)(_ `act` _)

  private def act(c: Char): Robot =
    c match
      case 'L' => turnLeft
      case 'R' => turnRight
      case 'A' => advance
      case _   => this