object RailFenceCipher:
   private enum Mode:
      case E, D
   def encode(s: String, i: Int): String = rfc(s, i, Mode.E)
   def decode(s: String, i: Int): String = rfc(s, i, Mode.D)
   private def p(k: Int): Seq[Int] = (0 until k) ++ (k - 2 until 0 by -1)
   private def rfc(s: String, i: Int, m: Mode): String =
      m match
         case Mode.E => s.sliding(p(i).size, p(i).size).flatMap(_.zip(p(i))).toSeq.groupBy(_._2).values
           .flatten.map(_._1).mkString
         case Mode.D => (0 until i).flatMap(r => (0 until s.length).zip(LazyList.continually(p(i))
               .flatten.take(s.length)).filter(_._2 == r).map(_._1)).zip(s).sortBy(_._1).map(_._2).mkString
