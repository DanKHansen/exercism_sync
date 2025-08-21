enum Plant:
   case Grass, Clover, Radishes, Violets

case class Garden(s: String):
   def plants(name: String): List[Plant] =
      val Array(firstRow, secondRow) = s.split("\n").map(_.sliding(2, 2).toList)
      (firstRow(name.head - 'A') ++ secondRow(name.head - 'A'))
         .flatMap(c => Plant.values.find(_.toString.head == c))
         .toList

object Garden:
   def defaultGarden(encoding: String): Garden = Garden(encoding)
