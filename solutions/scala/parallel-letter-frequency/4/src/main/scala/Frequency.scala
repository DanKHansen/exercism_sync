import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future}
import scala.concurrent.duration.Duration

object Frequency:
   def frequency(numWorkers: Int, texts: Seq[String]): Map[Char, Int] =
      implicit val exCtx: ExecutionContextExecutorService =
         ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(numWorkers))

      Await.result(
        Future(
          texts
             .map(_.toLowerCase.filter(_.isLetter).groupMapReduce(identity)(_ => 1)(_ + _))
             .reduceOption((m1, m2) => m1 ++ m2.map(t => (t._1, t._2 + m1.getOrElse(t._1, 0))))
             .getOrElse(Map.empty)),
        Duration(1000L, "ms")
      )
