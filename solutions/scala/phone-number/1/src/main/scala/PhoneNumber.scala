object PhoneNumber:
   def clean(s: String): Option[String] = {
      val onlyDigits = s.filter(_.isDigit)
      if (s.length < 10 || onlyDigits.length < 10 || onlyDigits.length > 11) None
      else onlyDigits.length match {
         case 10 if List('0', '1').contains(onlyDigits.head) => None
         case 10 if List('0', '1').contains(onlyDigits(3)) => None
         case 10 => Some(onlyDigits)
         case 11 if onlyDigits.head == '1' => Some(onlyDigits.tail)
         case _ => None
      }
   }