import scala.collection.parallel.CollectionConverters.*

object Frequency:
   def frequency(numWorkers: Int, texts: Seq[String]): Map[Char, Int] =
      texts.par
         .map(_.toLowerCase.filter(_.isLetter).groupMapReduce(identity)(_ => 1)(_ + _))
         .reduceOption((m1, m2) => m1 ++ m2.map(t => (t._1, t._2 + m1.getOrElse(t._1, 0))))
         .getOrElse(Map.empty)

