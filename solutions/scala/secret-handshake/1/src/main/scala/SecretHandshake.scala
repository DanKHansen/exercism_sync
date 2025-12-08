object SecretHandshake:
  def commands(i: Int): List[String] =
    val as =
      List("wink", "double blink", "close your eyes", "jump", "reverse")
    val l = i.toBinaryString.reverse
      .zip(as)
      .collect { case ('1', a) => a }
      .toList
    if l.contains("reverse") then l.reverse.tail else l
