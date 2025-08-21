enum Allergen:
   case Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats
object Allergies:
   def allergicTo(a: Allergen, n: Int): Boolean = (1 << a.ordinal & n) > 0
   def list(n: Int): List[Allergen] = Allergen.values.filter(allergicTo(_, n)).toList