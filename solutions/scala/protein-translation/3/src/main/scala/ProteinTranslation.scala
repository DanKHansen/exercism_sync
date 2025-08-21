object ProteinTranslation:
   private def codonMatch(codon: String): String =
      codon match
         case "AUG"                         => "Methionine"
         case "UUU" | "UUC"                 => "Phenylalanine"
         case "UUA" | "UUG"                 => "Leucine"
         case "UCU" | "UCC" | "UCA" | "UCG" => "Serine"
         case "UAU" | "UAC"                 => "Tyrosine"
         case "UGU" | "UGC"                 => "Cysteine"
         case "UGG"                         => "Tryptophan"
         case "UAA" | "UAG" | "UGA"         => "STOP"

   def proteins(s: String): Seq[String] =
      s.grouped(3).map(codonMatch).takeWhile(_ != "STOP").toSeq
