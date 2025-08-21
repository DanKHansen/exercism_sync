object ZebraPuzzle:

   sealed trait Resident
   case object Englishman extends Resident
   case object Spaniard extends Resident
   case object Ukrainian extends Resident
   case object Norwegian extends Resident
   case object Japanese extends Resident

   case class Solution(waterDrinker: Resident, zebraOwner: Resident)

   lazy val solve: Solution = {
      for
         residents <- List(Englishman, Spaniard, Ukrainian, Norwegian, Japanese).permutations
         if residents.head == Norwegian
         pets <- List("Dog", "Snail", "Fox", "Horse", "Zebra").permutations
         if pets.indexOf("Dog") == residents.indexOf(Spaniard)
         hobbies <- List("Dancing", "Reading", "Painter", "Chess", "Football").permutations
         if hobbies.indexOf("Chess") == residents.indexOf(Japanese) &&
            hobbies.indexOf("Dancing") == pets.indexOf("Snail") &&
            (hobbies.indexOf("Reading") == pets.indexOf("Fox") - 1 || hobbies.indexOf("Reading") == pets.indexOf(
              "Fox") + 1) &&
            (hobbies.indexOf("Painter") == pets.indexOf("Horse") - 1 || hobbies.indexOf("Painter") == pets.indexOf(
              "Horse") + 1)
         colors <- List("Red", "Green", "Ivory", "Yellow", "Blue").permutations
         if colors(1) == "Blue" &&
            colors.indexOf("Red") == residents.indexOf(Englishman) &&
            colors.indexOf("Green") == colors.indexOf("Ivory") + 1 &&
            colors.indexOf("Yellow") == hobbies.indexOf("Painter")
         beverages <- List("Coffee", "Tea", "Milk", "Orange juice", "Water").permutations
         if beverages(2) == "Milk" &&
            beverages.indexOf("Coffee") == colors.indexOf("Green") &&
            beverages.indexOf("Tea") == residents.indexOf(Ukrainian) &&
            beverages.indexOf("Orange juice") == hobbies.indexOf("Football")
      yield Solution(residents(beverages.indexOf("Water")), residents(pets.indexOf("Zebra")))
   }.next()
