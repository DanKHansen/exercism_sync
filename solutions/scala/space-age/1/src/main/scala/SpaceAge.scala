object SpaceAge {
  val ey: Double = 60 * 60 * 24 * 365.25

  def onMercury(s: Double): Double = s / (ey * 0.2408467)

  def onVenus(s: Double): Double = s / (ey * 0.61519726)

  def onEarth(s: Double): Double = s / ey

  def onMars(s: Double): Double = s / (ey * 1.8808158)

  def onJupiter(s: Double): Double = s / (ey * 11.862615)

  def onSaturn(s: Double): Double = s / (ey * 29.447498)

  def onUranus(s: Double): Double = s / (ey * 84.016846)

  def onNeptune(s: Double): Double = s / (ey * 164.79132)
}
