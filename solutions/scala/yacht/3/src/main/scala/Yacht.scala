object Yacht:
   def score(dice: List[Int], category: String): Int =
      val (dv, ds) = (dice.groupBy(identity).values, List("ones", "twos", "threes", "fours", "fives", "sixes"))
      category match
         case "yacht" if dice.distinct.size == 1                => 50
         case c if ds.contains(c)                               => dice.filter(_ == ds.indexOf(c) + 1).sum
         case "full house" if dv.map(_.size).toSet == Set(2, 3) => dice.sum
         case "four of a kind"                                  => dv.filter(_.size >= 4).flatten.take(4).sum
         case "little straight" if dice.sorted == (1 to 5)      => 30
         case "big straight" if dice.sorted == (2 to 6)         => 30
         case "choice"                                          => dice.sum
         case _                                                 => 0
