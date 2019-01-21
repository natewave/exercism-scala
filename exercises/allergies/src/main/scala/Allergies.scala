object Allergies {
  def allergicTo(allergen: Allergen, score: Int): Boolean = {
    (Allergen.score(allergen) & score) != 0
  }

  def list(score: Int): List[Allergen] = Allergen.all.filter(allergicTo(_, score))
}

sealed abstract class Allergen extends Product with Serializable

object Allergen {
  final case object Eggs extends Allergen
  final case object Peanuts extends Allergen
  final case object Shellfish extends Allergen
  final case object Strawberries extends Allergen
  final case object Tomatoes extends Allergen
  final case object Chocolate extends Allergen
  final case object Pollen extends Allergen
  final case object Cats extends Allergen

  def score(allergen: Allergen): Int = allergen match {
    case Eggs => 1
    case Peanuts => 2
    case Shellfish => 4
    case Strawberries => 8
    case Tomatoes => 16
    case Chocolate => 32
    case Pollen => 64
    case Cats => 128
  }

  def all: List[Allergen] = List(Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats)
}