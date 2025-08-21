case class clck(min: Int = 0) {
  def +(c: clck): clck = clck(Clock.ms(min + c.min))
  def -(c: clck): clck = clck(Clock.ms(min - c.min))
}

object Clock {
  private val mpd = 24 * 60
  def ms(x: Int): Int = (x % mpd + mpd) % mpd
  def apply(h: Int, m: Int): clck = clck(ms(h * 60 + m))
  def apply(m: Int): clck = clck(m % (60 * mpd))
}
