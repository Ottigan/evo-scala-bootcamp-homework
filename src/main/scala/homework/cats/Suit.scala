package homework.cats

sealed trait Suit {
  val letter: String
}
object Suit {
  case object Diamond extends Suit {
    val letter = "d"
  }
  case object Spade extends Suit {
    val letter = "s"
  }
  case object Heart extends Suit {
    val letter = "h"
  }
  case object Club extends Suit {
    val letter = "c"
  }

  def create(letter: String): Suit = letter match {
    case "c" => Club
    case "d" => Diamond
    case "h" => Heart
    case "s" => Spade
  }
}
