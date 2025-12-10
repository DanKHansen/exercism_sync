object SecretHandshake:
  private val signs =
    Map(1 -> "wink", 2 -> "double blink", 4 -> "close your eyes", 8 -> "jump")

  def commands(i: Int): List[String] =
    val s = signs.collect { case (k, v) if (i & k) != 0 => v }.toList
    if (i & 16) != 0 then s.reverse else s
