object Frequency:
   def frequency(numWorkers: Int = 1, texts: Seq[String]): Map[Char, Int] =
      val t =
         if texts.isEmpty then ""
         else texts.reduce((s, s2) => s.concat(s2))

      "[a-z]+".r
         .findAllIn(t.toLowerCase)
         .mkString
         .toSeq
         .groupBy(identity)
         .map((k, v) => k -> v.size)
