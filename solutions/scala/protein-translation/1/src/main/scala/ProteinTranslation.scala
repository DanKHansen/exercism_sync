import scala.annotation.tailrec

object ProteinTranslation:
   private val pMap: Map[Seq[String], String] = Map(
     Seq("AUG") -> "Methionine",
     Seq("UUU", "UUC") -> "Phenylalanine",
     Seq("UUA", "UUG") -> "Leucine",
     Seq("UCU", "UCC", "UCA", "UCG") -> "Serine",
     Seq("UAU", "UAC") -> "Tyrosine",
     Seq("UGU", "UGC") -> "Cysteine",
     Seq("UGG") -> "Tryptophan",
     Seq("UAA", "UAG", "UGA") -> "STOP"
   )
   def proteins(s: String): Seq[String] =
      @tailrec
      def loop(cs: Seq[String], proteins: Seq[String]): Seq[String] =
         cs match
            case Nil                                                    => proteins.reverse
            case ::(head, _) if Seq("UAA", "UAG", "UGA").contains(head) => proteins.reverse
            case ::(head, tail)                                         =>
               loop(tail, proteins.prepended(pMap.find(_._1.contains(head)).map(_._2).getOrElse("")))

      loop(s.grouped(3).toSeq, Nil)
