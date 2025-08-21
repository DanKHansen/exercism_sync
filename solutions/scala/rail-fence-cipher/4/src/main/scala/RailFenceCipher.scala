object RailFenceCipher:
   private enum Mode:
      case E, D
   def encode(s: String, i: Int): String = rfc(s, i, Mode.E)
   def decode(s: String, i: Int): String = rfc(s, i, Mode.D)
   private def pat(k: Int): Seq[Int] = (0 until k) ++ (k - 2 until 0 by -1)
   private def rfc(s: String, i: Int, m: Mode): String =
      lazy val (p, pSize, sLen) = (pat(i), pat(i).size, s.length)
      lazy val rep = (0 until sLen).flatMap(_ => p).sliding(sLen, sLen).next()
      m match
         case Mode.E => s.sliding(pSize, pSize).flatMap(_.zip(p)).toSeq.groupBy(_._2).values.flatten.map(_._1).mkString
         case Mode.D => (0 until i).flatMap(j => (0 until sLen).zip(rep).filter(_._2 == j).map(_._1)).zip(s).sorted.map(_._2).mkString
