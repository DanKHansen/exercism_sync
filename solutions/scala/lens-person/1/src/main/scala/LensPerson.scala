import java.time.LocalDate
import monocle.{Lens, PLens}
import monocle.macros.GenLens

object LensPerson:
   case class Person(_name: Name, _born: Born, _address: Address)

   case class Name(_foreNames: String /*Space separated*/, _surName: String)

   // Value of java.time.LocalDate.toEpochDay
   private type EpochDay = Long

   case class Born(_bornAt: Address, _bornOn: EpochDay)

   case class Address(_street: String, _houseNumber: Int, _place: String /*Village / city*/, _country: String)

   // Valid values of Gregorian are those for which 'java.time.LocalDate.of'
   // returns a valid LocalDate.
   case class Gregorian(_year: Int, _month: Int, _dayOfMonth: Int)

   // Implement these.
   private val personBorn: Lens[Person, Born] = GenLens[Person](_._born)
   private val personAddress: Lens[Person, Address] = GenLens[Person](_._address)
   private val addressStreet: Lens[Address, String] = GenLens[Address](_._street)
   private val bornAt: Lens[Born, Address] = GenLens[Born](_._bornAt)
   private val bornOn: Lens[Born, EpochDay] = GenLens[Born](_._bornOn)

   val bornStreet: Born => String =
      bornAt.andThen(addressStreet).asGetter.get

   val setCurrentStreet: String => Person => Person =
      personAddress.andThen(addressStreet).asSetter.replace

   val setBirthMonth: Int => Person => Person =
      (month: Int) =>
         val f = (d: EpochDay) => LocalDate.ofEpochDay(d).withMonth(month).toEpochDay
         personBorn.andThen(bornOn).modify(f)

   // Transform both birth and current street names.
   val renameStreets: (String => String) => Person => Person =
      (f: String => String) =>
         personAddress
            .andThen(addressStreet)
            .modify(f)
            .andThen(personBorn.andThen(bornAt.andThen(addressStreet)).modify(f))
