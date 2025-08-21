object Series:
   def largestProduct(span: Int, digits: String): Option[Int] =
      if span > digits.length || !digits.forall(_.isDigit) || span < 0 then None
      else digits.sliding(span).map(value => value.toList.map(_.toString.toInt).product).maxOption
