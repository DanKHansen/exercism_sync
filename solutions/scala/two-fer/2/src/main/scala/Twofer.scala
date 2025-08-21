object Twofer {
  def twofer(name: String = ""): String = name match {
    case "" => "One for you, one for me."
    case _ => "One for " + name + ", one for me."
  }
}
