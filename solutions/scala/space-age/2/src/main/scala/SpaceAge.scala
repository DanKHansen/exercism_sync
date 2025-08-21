object SpaceAge {

  def onMercury(s: Double): Double = onEarth(s) / 0.2408467

  def onVenus(s: Double): Double = onEarth(s) / 0.61519726

  def onMars(s: Double): Double = onEarth(s) / 1.8808158

  def onJupiter(s: Double): Double = onEarth(s) / 11.862615

  def onSaturn(s: Double): Double = onEarth(s) / 29.447498

  def onUranus(s: Double): Double = onEarth(s) / 84.016846

  def onNeptune(s: Double): Double = onEarth(s) / 164.79132

  def onEarth(s: Double): Double = s / (60 * 60 * 24 * 365.25)
}
