object Luhn:
   def valid(s: String): Boolean =
      val trimmed = s.filterNot(_.isSpaceChar)
      trimmed.forall(_.isDigit) && trimmed.length > 1 && {
         val everySecond = trimmed.reverse.zipWithIndex.filter(_._2 % 2 != 0).map(_._1)
         val doubled = everySecond.map(_.asDigit).map(n => if n * 2 > 9 then n * 2 - 9 else n * 2).mkString
         trimmed.diff(everySecond).concat(doubled).map(_.asDigit).sum % 10 == 0
      }
