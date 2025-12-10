enum Bearing:
  case North, East, South, West

case class Robot(bearing: Bearing, coordinates: (Int, Int)):
  def turnRight: Robot = copy(bearing = turn(1))

  def turnLeft: Robot = copy(bearing = turn(3))

  def advance: Robot =
    val (x, y) = coordinates
    val (dx, dy) =
      bearing match
        case Bearing.North => (0, 1)
        case Bearing.East  => (1, 0)
        case Bearing.South => (0, -1)
        case Bearing.West  => (-1, 0)
    copy(coordinates = (x + dx, y + dy))

  def simulate(instructions: String): Robot =
    instructions.foldLeft(this)(_ act _)

  infix private def act(c: Char): Robot =
    c match
      case 'L' => turnLeft
      case 'R' => turnRight
      case 'A' => advance
      case _   => this

  private def turn(i: Int): Bearing =
    Bearing.fromOrdinal((bearing.ordinal + i) % 4)
