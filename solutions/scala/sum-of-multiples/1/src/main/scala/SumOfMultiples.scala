object SumOfMultiples {
  def sum(factors: Set[Int], limit: Int): Int = {
    factors.map(i => (1 until limit).map(j => j * i)).flatMap(v => v.filter(p => p < limit)).sum
  }
}
