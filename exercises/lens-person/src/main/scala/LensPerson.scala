import java.time.LocalDate

import monocle.{Lens, PIso}
import monocle.macros.GenLens

object LensPerson {

  //---! models
  case class Person(_name: Name, _born: Born, _address: Address)
  case class Name(_foreNames: String /*Space separated*/ , _surName: String)
  // Value of java.time.LocalDate.toEpochDay
  type EpochDay = Long
  case class Born(_bornAt: Address, _bornOn: EpochDay)
  case class Address(_street: String, _houseNumber: Int,
                     _place: String /*Village / city*/ , _country: String)

  // Valid values of Gregorian are those for which 'java.time.LocalDate.of'
  // returns a valid LocalDate.
  case class Gregorian(_year: Int, _month: Int, _dayOfMonth: Int)


  //---! Lens
  val name      : Lens[Person, Name] = GenLens[Person](_._name)
  val born      : Lens[Person, Born] = GenLens[Person](_._born)
  val address   : Lens[Person , Address]  = GenLens[Person](_._address)
  val streetName: Lens[Address  , String]  = GenLens[Address](_._street)
  val bornAt: Lens[Born, Address] = GenLens[Born](_._bornAt)
  val bornOn: Lens[Born, EpochDay] = GenLens[Born](_._bornOn)
  val month = GenLens[Gregorian](_._month)

  // Iso
  val isoEpocheDayGregorian: PIso[EpochDay, EpochDay, Gregorian, Gregorian] = {
    def epochDayToGregorian(ed: EpochDay): Gregorian = {
      val ld = LocalDate.ofEpochDay(ed)
      Gregorian(ld.getYear, ld.getMonth.getValue, ld.getDayOfMonth)
    }
    def gregorianToEpochDay(g: Gregorian): EpochDay =
      LocalDate.of(g._year, g._month, g._dayOfMonth).toEpochDay

    PIso(epochDayToGregorian)(gregorianToEpochDay)
  }


  // Implement these.
  val bornStreet: Born => String = (born: Born) => (bornAt composeLens streetName).get(born)

  val setCurrentStreet: String => Person => Person = (street: String) =>
    (address composeLens streetName).set(street)

  val setBirthMonth: Int => Person => Person = (m: Int) =>
      (born composeLens bornOn composeIso isoEpocheDayGregorian composeLens month).set(m)

  // Transform both birth and current street names.
  val renameStreets: (String => String) => Person => Person = (f: String => String) => {
    val bornStreet = born composeLens bornAt composeLens streetName
    val addressStreet = address composeLens streetName

    bornStreet.modify(f).compose(addressStreet.modify(f))
  }
}
