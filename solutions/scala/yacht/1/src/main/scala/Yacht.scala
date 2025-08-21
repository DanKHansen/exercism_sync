object Yacht:
   def score(dice: List[Int], category: String): Int =
      val ds = List("ones", "twos", "threes", "fours", "fives", "sixes")
      category match
         case "yacht"                 => if dice.forall(_ == dice.head) then 50 else 0
         case cat if ds.contains(cat) => dice.filter(_ == ds.indexOf(cat) + 1).sum
         case "full house"            => if dice.groupBy(identity).values.map(_.size).toSeq.sorted == Seq(2, 3) then dice.sum else 0
         case "four of a kind"        => dice.groupBy(identity).collect { case (_, xs) if xs.size >= 4 => xs.take(4).sum }.maxOption.getOrElse(0)
         case "little straight"       => if dice.sorted == List(1, 2, 3, 4, 5) then 30 else 0
         case "big straight"          => if dice.sorted == List(2, 3, 4, 5, 6) then 30 else 0
         case "choice"                => dice.sum
         case _                       => 0
