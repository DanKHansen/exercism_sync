object AllYourBase:
   def rebase(fromBase: Int, number: List[Int], toBase: Int): Option[List[Int]] =
      if fromBase < 2 || toBase < 2 || number.exists(x => x < 0 || x >= fromBase) then None
      else
         val decimal =
            number.reverse.zipWithIndex.map { case (digit, index) => digit * math.pow(fromBase, index) }.sum.toInt
         def convertToBase(value: Int, base: Int): List[Int] =
            if value <= 0 then Nil
            else (value % base) :: convertToBase(value / base, base)
         Some(convertToBase(decimal, toBase).reverse).filter(_.nonEmpty).orElse(Some(List(0)))
