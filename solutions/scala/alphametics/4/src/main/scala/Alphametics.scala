object Alphametics:

   def solve(equation: String): Option[Map[Char, Int]] =
      val (addends, result) = parse(equation)
      val uniques = equation.filter(_.isLetter).distinct
      val notZeroes = addends.map(_.head).appended(result.head).distinct
      val wMap = weights(addends, result)

      (0 to 9)
         .combinations(uniques.length)
         .flatMap(_.permutations)
         .collectFirst {
            case comb
                if comb.zip(wMap).forall { case (i, t) => i != 0 || !notZeroes.contains(t._1) } &&
                   comb.zip(wMap).map { case (i, t) => i * t._2 }.sum == 0 =>
               comb.zip(wMap).map { case (i, t) => (t._1, i) }.toMap
         }

   private def parse(s: String): (Seq[String], String) =
      val parts = s.filterNot(_.isSpaceChar).split("==", 2)
      (parts.head.split("\\+").toSeq, parts.last)

   private def weights(as: Seq[String], r: String): Map[Char, Int] =
      def weight(s: String, sign: Int): Map[Char, Int] =
         s.zipWithIndex
            .groupMap(_._1) { case (_, idx) => Math.pow(10, s.length - 1 - idx).toInt }
            .view
            .mapValues(_.sum * sign)
            .toMap

      (as.map(weight(_, 1)) :+ weight(r, -1)).flatten
         .groupMap(_._1)(_._2)
         .view
         .mapValues(_.sum)
         .toMap
