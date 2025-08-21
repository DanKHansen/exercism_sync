object RobotSimulator {
  def simulate(r: Robot, s: String): Robot = s.foldLeft(r)(parser)

  private def parser(r: Robot, c: Char): Robot = c match {
    case 'A' => r.advance
    case 'L' => r.turnLeft
    case 'R' => r.turnRight
    case _ => r
  }
}