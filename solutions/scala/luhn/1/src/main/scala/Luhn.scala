object Luhn:
   def valid(s: String): Boolean =
      val trimmed = s.filterNot(_.isSpaceChar)
      val okToValidate = trimmed.forall(_.isDigit) && trimmed.length > 1
      okToValidate && {
         val everySecond = trimmed.zip(trimmed.indices.reverse).filter(t => t._2 % 2 != 0).map(_._1).mkString
         val doubled = everySecond.map(_.asDigit).map(n => if n * 2 > 9 then n * 2 - 9 else n * 2).mkString
         trimmed.diff(everySecond).concat(doubled).map(_.asDigit).sum % 10 == 0
      }
