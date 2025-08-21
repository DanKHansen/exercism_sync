object ProteinTranslation:
   def proteins(s: String): Seq[String] =
      s.grouped(3)
         .map {
            case "AUG"                         => "Methionine"
            case "UUU" | "UUC"                 => "Phenylalanine"
            case "UUA" | "UUG"                 => "Leucine"
            case "UCU" | "UCC" | "UCA" | "UCG" => "Serine"
            case "UAU" | "UAC"                 => "Tyrosine"
            case "UGU" | "UGC"                 => "Cysteine"
            case "UGG"                         => "Tryptophan"
            case _                             => "STOP"
         }
         .takeWhile(_ != "STOP")
         .toSeq
