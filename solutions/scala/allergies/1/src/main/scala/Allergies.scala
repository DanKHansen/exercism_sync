enum Allergen(val score: Int):
   case Eggs extends Allergen(1)
   case Peanuts extends Allergen(2)
   case Shellfish extends Allergen(4)
   case Strawberries extends Allergen(8)
   case Tomatoes extends Allergen(16)
   case Chocolate extends Allergen(32)
   case Pollen extends Allergen(64)
   case Cats extends Allergen(128)

object Allergies:
   def list(n: Int): List[Allergen] =
      Allergen.values.filter(a => (n & a.score) != 0).toList

   def allergicTo(a: Allergen, n: Int): Boolean =
      (n & a.score) != 0