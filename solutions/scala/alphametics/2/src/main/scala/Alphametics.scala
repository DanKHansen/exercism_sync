object Alphametics:
   def solve(equation: String): Option[Map[Char, Long]] =
      val (left, right) = parseEquation(equation)
      val uniqueLetters = (left.flatMap(_.toSet) ++ right.toSet).toSet.toList
      backtrack(uniqueLetters, left, right, Map.empty, Set.empty)

   private def parseEquation(equation: String): (Array[String], String) =
      val parts = equation.split("==").map(_.trim)
      val left = parts(0).split("\\+").map(_.trim)
      (left, parts(1))

   private def backtrack(
       letters: List[Char],
       left: Array[String],
       right: String,
       currentMapping: Map[Char, Long],
       usedDigits: Set[Long]): Option[Map[Char, Long]] =
      if letters.isEmpty then if isValidMapping(currentMapping, left, right) then Some(currentMapping) else None
      else
         val letter = letters.head
         (0 to 9)
            .to(LazyList)
            .flatMap { digit =>
               val digitLong = digit.toLong
               if isLeadingZero(letter, left, right, digit) || usedDigits.contains(digitLong) then None
               else backtrack(letters.tail, left, right, currentMapping + (letter -> digitLong), usedDigits + digitLong)
            }
            .headOption

   private def isValidMapping(mapping: Map[Char, Long], left: Array[String], right: String): Boolean =
      val leftSum = left.map(word => word.map(mapping).mkString("").toLong).sum
      val rightValue = right.map(mapping).mkString("").toLong
      leftSum == rightValue

   private def isLeadingZero(letter: Char, left: Array[String], right: String, digit: Int): Boolean =
      val isFirstLetter = (word: String) => word.head == letter
      digit == 0 && (left.exists(isFirstLetter) || right.headOption.contains(letter))
