class WordCount(s: String):
   def countWords: Map[String, Int] =
      "\\w+('?\\w+)*".r.findAllIn(s.toLowerCase).toSeq.groupBy(identity).map((k,v) => k -> v.size)
