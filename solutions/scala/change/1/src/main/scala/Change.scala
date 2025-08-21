object Change:
   def findFewestCoins(target: Int, coins: List[Int]): Option[List[Int]] =
      if target >= 0 then
         val dp = (0 to target).foldLeft(Vector.fill(target + 1)(None: Option[List[Int]])) { (acc, amt) =>
            if amt == 0 then acc.updated(amt, Some(List.empty[Int])) else
               val candidates = coins.flatMap { coin => if amt - coin >= 0 then acc(amt - coin).map(coin :: _) else None}
               acc.updated(amt, if candidates.isEmpty then None else Some(candidates.minBy(_.length)))
         }
         dp(target)
      else None