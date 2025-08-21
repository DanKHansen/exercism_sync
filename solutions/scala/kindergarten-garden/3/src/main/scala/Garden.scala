enum Plant:
   case Grass, Clover, Radishes, Violets

case class Garden(s: String):
   def plants(name: String): Seq[Plant] =
      val rows = s.split("\n").map(_.sliding(2, 2).toSeq)
      (rows(0)(name.head - 'A') ++ rows(1)(name.head - 'A'))
         .flatMap(c => Plant.values.find(_.toString.head == c))

object Garden:
   def defaultGarden(encoding: String): Garden = Garden(encoding)
