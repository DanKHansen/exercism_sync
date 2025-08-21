object Alphametics:
   def solve(equation: String): Option[Map[Char, Int]] =
      val (left, right) = parseEquation(equation)
      val uniqueLetters = (left.flatMap(_.toSet) ++ right.toSet).toSet.mkString
      backtrack(uniqueLetters, left, right, Map.empty, Seq.empty)

   private def parseEquation(equation: String): (Seq[String], String) =
      val parts = equation.filterNot(_.isSpaceChar).split("==").toSeq
      val left = parts.head.filterNot(_.isSpaceChar).split("\\+").toSeq
      (left, parts(1))

   private def backtrack(
       letters: String,
       left: Seq[String],
       right: String,
       currentMapping: Map[Char, Int],
       usedDigits: Seq[Int]): Option[Map[Char, Int]] =
      if letters.isEmpty then
         if isValidMapping(currentMapping, left, right) then Some(currentMapping)
         else None
      else
         val letter = letters.head
         (0 to 9)
            .to(LazyList)
            .flatMap { digit =>
               if isLeadingZero(letter, left, right, digit) || usedDigits.contains(digit) then None
               else backtrack(letters.tail, left, right, currentMapping + (letter -> digit), usedDigits.appended(digit))
            }
            .headOption

   private def isValidMapping(mapping: Map[Char, Int], left: Seq[String], right: String): Boolean =
      left.foldLeft(0L) { (sum, w) =>
         sum + w.map(mapping).mkString.toLong
      } == right.map(mapping).mkString.toLong

   private def isLeadingZero(letter: Char, left: Seq[String], right: String, digit: Int): Boolean =
      digit == 0 && ( right.headOption.contains(letter) || left.exists(w => w.head == letter))
