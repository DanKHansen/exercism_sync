import Direction.*
import scala.annotation.tailrec

enum Color:
   case Black, White
enum Direction:
   case N, NE, E, S, SW, W

case class Pos(row: Int, col: Int)
extension (p: Pos)
   private def vicinity(d: Direction): Pos = d match
      case Direction.N  => Pos(p.row - 1, p.col)
      case Direction.NE => Pos(p.row - 1, p.col + 1)
      case Direction.E  => Pos(p.row, p.col + 1)
      case Direction.S  => Pos(p.row + 1, p.col)
      case Direction.SW => Pos(p.row + 1, p.col - 1)
      case Direction.W  => Pos(p.row, p.col - 1)

class Connect(board: List[String]):
   private val (maxRowIdx, maxColIdx) = (board.indices.last, board.head.indices.last)
   private val startRowBlack: List[Int] = findIndices(board.map(_.head), _ == 'X')
   private val startColWhite: List[Int] = findIndices(board.head.toList, _ == 'O')
   private val startPosBlack: List[Pos] = if startRowBlack.isEmpty then Nil else startRowBlack.map(Pos(_, 0))
   private val startPosWhite: List[Pos] = if startColWhite.isEmpty then Nil else startColWhite.map(Pos(0, _))

   private def findIndices[T](list: List[T], predicate: T => Boolean): List[Int] =
      list.zipWithIndex.collect {
         case (element, index) if predicate(element) => index
      }

   private def charAtPos(p: Pos): Char = board(p.row)(p.col)

   @tailrec
   private def checkPath(c: Char, ps: List[Pos], visited: List[Pos]): Boolean =
      if ps.isEmpty then false
      else if visited.contains(ps.head) then checkPath(c, ps.tail, visited)
      else
         (c, ps) match
            case ('X', ::(head, _)) if head.col == maxColIdx => true
            case ('O', ::(head, _)) if head.row == maxRowIdx => true
            case (_, ::(head, tail))                         =>
               val nextPos = (for d <- Direction.values yield head.vicinity(d))
                  .filterNot(_.row < 0)
                  .filterNot(_.col < 0)
                  .filterNot(_ => visited.contains(tail))
                  .filterNot(_.row > maxRowIdx)
                  .filterNot(_.col > maxColIdx)
                  .filter(p => charAtPos(p) == c)

               if nextPos.isEmpty then false
               else checkPath(c, nextPos.toList ++ tail, head :: visited)
            case (_, Nil)                                    => false

   def winner: Option[Color] =
      val midBordEmpty = board.nonEmpty && board.tail.nonEmpty && board.tail.init.forall(_.tail.init.forall(_ == '.'))
      val checkWhite =
         if startPosWhite.isEmpty || midBordEmpty then false
         else checkPath('O', startPosWhite, Nil)
      val checkBlack =
         if startPosBlack.isEmpty || midBordEmpty then false
         else checkPath('X', startPosBlack, Nil)
      (checkBlack, checkWhite) match
         case (false, true) => Some(Color.White)
         case (true, false) => Some(Color.Black)
         case _             => None
