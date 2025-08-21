import scala.util.{Success, Try}

object Minesweeper:
   def annotate(grid: List[String]): List[String] =
      if grid.isEmpty || grid.head.isEmpty then return grid
      def check(c: (Int, Int)): String =
         val amount = List(
           Try(grid(c._1 - 1)(c._2 - 1)),
           Try(grid(c._1 - 1)(c._2)),
           Try(grid(c._1 - 1)(c._2 + 1)),
           Try(grid(c._1)(c._2 - 1)),
           Try(grid(c._1)(c._2 + 1)),
           Try(grid(c._1 + 1)(c._2 - 1)),
           Try(grid(c._1 + 1)(c._2)),
           Try(grid(c._1 + 1)(c._2 + 1))
         ).count(_ == Success('*'))
         if amount > 0 then amount.toString else " "

      val cells = for
         row <- grid.indices
         col <- grid.head.indices
         cell = (row, col)
      yield cell
      val r = cells.map(cell => if grid(cell._1)(cell._2) == '*' then "*" else check(cell))
      if grid.head.length > 1 then r.mkString.sliding(grid.head.length, grid.size).toList
      else r.toList
