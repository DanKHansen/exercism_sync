object BookStore:
   private val discounts = Seq(800, 760, 720, 640, 600)

   def total(books: Seq[Int]): Int =
      val counts = books.groupBy(identity).view.mapValues(_.size).toMap
      val bookCounts = counts.keys.toSeq.sorted.map(counts).toArray
      def calc(counts: Seq[Int], memo: Map[Seq[Int], Int]): (Int, Map[Seq[Int], Int]) =
         if counts.forall(_ == 0) then (0, memo)
         else
            memo.get(counts).map((_, memo)).getOrElse {
               (1 until 1 << counts.size).foldLeft((Int.MaxValue, memo)) { case ((minP, m), mask) =>
                  val subset = counts.indices.filter(i => (mask & (1 << i)) != 0)
                  if subset.forall(counts(_) > 0) then
                     val price = discounts(subset.size - 1) * subset.size
                     val newCounts = counts.zipWithIndex.map { (c, i) => if subset.contains(i) then c - 1 else c }
                     val (p, m2) = calc(newCounts, m)
                     val newMin = price + p
                     val newMemo = m2.updated(counts, Math.min(minP, newMin))
                     (Math.min(minP, newMin), newMemo)
                  else (minP, m)
               }
            }
      calc(bookCounts.toSeq, Map.empty)._1
