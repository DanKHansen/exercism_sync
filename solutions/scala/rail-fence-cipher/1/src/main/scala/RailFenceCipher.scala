object RailFenceCipher:
   private enum MODE:
      case enc, dec
   def encode(text: String, key: Int): String =
      rfc(text, key, MODE.enc)

   def decode(text: String, key: Int): String =
      rfc(text, key, MODE.dec)

   private def rfc(t: String, k: Int, m: MODE): String =
      val p = (0 until k) ++ (k - 2 until 0 by -1)
      m match
         case MODE.enc => t.sliding(p.size, p.size).flatMap(_.zip(p)).toSeq.groupBy(_._2).values.flatten.map(_._1).mkString
         case MODE.dec => (0 until k).flatMap(i =>
                  (0 until t.length).zip(LazyList.continually(p).flatten.take(t.length)).collect
                     { case (idx, `i`) => idx }).zip(t).sorted.map(_._2).mkString
