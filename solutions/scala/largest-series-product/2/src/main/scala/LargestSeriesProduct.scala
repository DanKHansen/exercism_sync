object Series:
   def largestProduct(span: Int, digits: String): Option[Int] =
      if span <= 0 || span > digits.length || !digits.forall(_.isDigit) then None
      else digits.sliding(span).map(_.map(_.asDigit).product).maxOption