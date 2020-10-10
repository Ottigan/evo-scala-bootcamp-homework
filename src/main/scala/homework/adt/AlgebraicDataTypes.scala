package homework.adt

object AlgebraicDataTypes extends App {
  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //

  // 1. Suit
  sealed trait Suit
  object Suit {
    case object Club extends Suit
    case object Diamond extends Suit
    case object Heart extends Suit
    case object Spade extends Suit
  }

  // Alternative
  case class SuitAnyVal private (suit: String) extends AnyVal
  object SuitAnyVal {
    def create(suit: String): Option[SuitAnyVal] = {
      val lowerSuit = suit.toLowerCase

      lowerSuit match {
        case x @ "c" => Some(SuitAnyVal(x))
        case x @ "d" => Some(SuitAnyVal(x))
        case x @ "h" => Some(SuitAnyVal(x))
        case x @ "s" => Some(SuitAnyVal(x))
        case _       => None
      }
    }
  }

  // 2. Rank
  sealed trait Rank
  object Rank {
    case object Two extends Rank
    case object Three extends Rank
    case object Four extends Rank
    case object Five extends Rank
    case object Six extends Rank
    case object Seven extends Rank
    case object Eight extends Rank
    case object Nine extends Rank
    case object Ten extends Rank
    case object Jack extends Rank
    case object Queen extends Rank
    case object King extends Rank
    case object Ace extends Rank
  }

  // Alternative
  case class RankAnyVal private (rank: Int) extends AnyVal
  object RankAnyVal {
    def create(rank: Int): Option[RankAnyVal] = {
      if (rank > 1 && rank < 15) Some(RankAnyVal(rank))
      else None
    }
  }

  // 3. Card
  case class Card(rank: Rank, suit: Suit)

  import Rank._
  import Suit._

  val aceOfSpades: Card = Card(Ace, Spade)
  val fiveOfDiamonds: Card = Card(Five, Diamond)
  val tenOfClubs: Card = Card(Ten, Club)
  val jackOfSpades: Card = Card(Jack, Spade)
  val twoOfHearts: Card = Card(Two, Heart)

  // 4. Hand (Texas or Omaha)
  case class Hand private (cards: Set[Card], isOmaha: Boolean = False)
  object Hand {
    def create(cards: Set[Card], isOmaha: Boolean = False): Option[Hand] = cards.size match {
      case 2 if !isOmaha => Some(Hand(cards))
      case 4 if isOmaha  => Some(Hand(cards))
      case _             => None
    }
  }

  // 5. Board
  case class Board private (cards: Set[Card])
  object Board {
    def create(cards: Set[Card]): Option[Board] = {
      if (cards.size == 5) Some(Board(cards))
      else None
    }
  }

  // 6. Poker Combination (High Card, Pair, etc.)
  sealed trait Combination
  object Combination {
    case class StraightFlush private (cards: Set[Card]) extends Combination
    object StraightFlush {
      def create(cards: Set[Card]): Option[StraightFlush] = ???
    }

    case class FourOfaKind private (cards: Set[Card]) extends Combination
    object FourOfaKind {
      def create(cards: Set[Card]): Option[FourOfaKind] = ???
    }

    case class FullHouse private (cards: Set[Card]) extends Combination
    object FullHouse {
      def create(cards: Set[Card]): Option[FullHouse] = ???
    }

    case class Flush private (cards: Set[Card]) extends Combination
    object Flush {
      def create(cards: Set[Card]): Option[Flush] = ???
    }

    case class Straight private (cards: Set[Card]) extends Combination
    object Straight {
      def create(cards: Set[Card]): Option[Straight] = ???
    }

    case class ThreeOfaKind private (cards: Set[Card]) extends Combination
    object ThreeOfaKind {
      def create(cards: Set[Card]): Option[ThreeOfaKind] = ???
    }

    case class TwoPair private (cards: Set[Card]) extends Combination
    object TwoPair {
      def create(cards: Set[Card]): Option[TwoPair] = ???
    }

    case class Pair private (cards: Set[Card]) extends Combination
    object Pair {
      def create(cards: Set[Card]): Option[Pair] = ???
    }

    case class HighCard private (cards: Set[Card])
    object HighCard {
      def create(cards: Set[Card]): Option[HighCard] = ???
    }
  }

  // 7. Test Case (Board & Hands to rank)
  case class TestInput private (board: Board, hand: Set[TexasHand])
  object TestInput {
    def create(board: Board, hand: Set[TexasHand]): Option[TestInput] = ???
  }

  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  case class TestOutput private (hand: TexasHand*)
  object TestOutput {
    def create(hand: TexasHand*): Option[TestOutput] = ???
  }
}
