object ZebraPuzzle:

   sealed trait Resident
   case object Englishman extends Resident
   case object Spaniard extends Resident
   case object Ukrainian extends Resident
   case object Norwegian extends Resident
   case object Japanese extends Resident

   case class Solution(waterDrinker: Resident, zebraOwner: Resident)

   lazy val solve: Solution =
      val residents = List(Englishman, Spaniard, Ukrainian, Norwegian, Japanese)
      val colors = List("Red", "Green", "Ivory", "Yellow", "Blue")
      val pets = List("Dog", "Snail", "Fox", "Horse", "Zebra")
      val beverages = List("Coffee", "Tea", "Milk", "Orange juice", "Water")
      val hobbies = List("Dancing", "Reading", "Painter", "Chess", "Football")
      val sols = for
         resident <- residents.permutations
         if resident.indexOf(Norwegian) == 0 // statement 10
         pet <- pets.permutations
         if pet.indexOf("Dog") == resident.indexOf(Spaniard) // statement 3
         hobby <- hobbies.permutations
         if hobby.indexOf("Chess") == resident.indexOf(Japanese) // statement 14
            && hobby.indexOf("Dancing") == pet.indexOf("Snail") // statement 7
            && (hobby.indexOf("Reading") == pet.indexOf("Fox") - 1
               || hobby.indexOf("Reading") == pet.indexOf("Fox") + 1) // statement 11
            && (hobby.indexOf("Painter") == pet.indexOf("Horse") - 1
               || hobby.indexOf("Painter") == pet.indexOf("Horse") + 1) // statement 12
         color <- colors.permutations
         if color.indexOf("Blue") == 1 // statement 15
            && color.indexOf("Red") == resident.indexOf(Englishman) // statement 2
            && color.indexOf("Green") == color.indexOf("Ivory") + 1 // statement 6
            && color.indexOf("Yellow") == hobby.indexOf("Painter") // statement 8
         beverage <- beverages.permutations
         if beverage.indexOf("Milk") == 2 // statement 9
            && beverage.indexOf("Coffee") == color.indexOf("Green") // statement 4
            && beverage.indexOf("Tea") == resident.indexOf(Ukrainian) // Statement 5
            && beverage.indexOf("Orange juice") == hobby.indexOf("Football") // statement 13
      yield Solution(resident(beverage.indexOf("Water")), resident(pet.indexOf("Zebra"))) // Question

      sols.toSeq.head
