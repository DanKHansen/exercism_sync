object BinarySearch {
  def find(l: List[Int], i: Int): Option[AnyVal] = if (l.contains(i)) Some(l.indexOf(i)) else None
}


