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

  // 2. Rank
  sealed trait Rank {
    val rank: Int
  }
  object Rank {
    case object Two extends Rank {
      val rank = 2
    }
    case object Three extends Rank {
      val rank = 3
    }
    case object Four extends Rank {
      val rank = 4
    }
    case object Five extends Rank {
      val rank = 5
    }
    case object Six extends Rank {
      val rank = 6
    }
    case object Seven extends Rank {
      val rank = 7
    }
    case object Eight extends Rank {
      val rank = 8
    }
    case object Nine extends Rank {
      val rank = 9
    }
    case object Ten extends Rank {
      val rank = 10
    }
    case object Jack extends Rank {
      val rank = 11
    }
    case object Queen extends Rank {
      val rank = 12
    }
    case object King extends Rank {
      val rank = 13
    }
    case object Ace extends Rank {
      val rank = 14
    }
  }

  // 3. Card
  final case class Card(rank: Rank, suit: Suit)

  // Alternative
  // Potentially could assist with the logic of creating valid Omaha combinations
  final case class CardOmaha(rank: Rank, suit: Suit, isFromHand: Boolean)

  // 4. Hand (Texas or Omaha)
  final case class Hand private (cards: Set[Card], isOmaha: Boolean = false)
  object Hand {
    def create(cards: Set[Card], isOmaha: Boolean = false): Option[Hand] = cards.size match {
      case 2 if !isOmaha => Some(Hand(cards))
      case 4 if isOmaha  => Some(Hand(cards))
      case _             => None
    }
  }

  // 5. Board
  final case class Board private (cards: Set[Card])
  object Board {
    def create(cards: Set[Card]): Option[Board] = {
      if (cards.size == 5) Some(Board(cards))
      else None
    }
  }

  // 6. Test Case (Board & Hands to rank)
  final case class TestInput private (board: Board, hands: Set[Hand], isOmaha: Boolean = false)
  object TestInput {
    def create(board: Board, hands: Set[Hand], isOmaha: Boolean = false): Option[TestInput] = hands.size match {
      case x if x > 0 && x < 24 && !isOmaha => Some(TestInput(board, hands))
      case x if x > 0 && x < 12 && isOmaha  => Some(TestInput(board, hands, true))
      case _                                => None
    }
  }

  // 7. Poker Combination (High Card, Pair, etc.)
  sealed trait Combination extends Any {
    def cards: Set[Card]
  }
  object Combination {
    case class StraightFlush private (cards: Set[Card]) extends AnyVal with Combination
    case class FourOfAKind private (cards: Set[Card]) extends AnyVal with Combination
    case class FullHouse private (cards: Set[Card]) extends AnyVal with Combination
    case class Flush private (cards: Set[Card]) extends AnyVal with Combination
    case class Straight private (cards: Set[Card]) extends AnyVal with Combination
    case class ThreeOfAKind private (cards: Set[Card]) extends AnyVal with Combination
    case class TwoPair private (cards: Set[Card]) extends AnyVal with Combination
    case class Pair private (cards: Set[Card]) extends AnyVal with Combination
    case class HighCard private (cards: Set[Card]) extends AnyVal with Combination
  }

  // 8. Hand plus it's best combination for comparisons
  sealed abstract case class HandWithCombination private (hand: Hand, combination: Combination)
  object HandWithCombination {
    def create(hand: Hand, combination: Combination): Option[HandWithCombination] =
      (hand.cards.size, combination.cards.size) match {
        case (2, 5) if !hand.isOmaha => Some(new HandWithCombination(hand, combination) {})
        case (4, 5) if hand.isOmaha  => Some(new HandWithCombination(hand, combination) {})
        case _                       => None

      }
  }

  // 9. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  // If the nested Set contains more than one element then you would .mkString("=")
  sealed abstract case class TestOutput private (rankedHands: List[List[Hand]])
  object TestOutput {
    def create(rankedHands: List[List[Hand]]): Option[TestOutput] = {
      if (rankedHands.forall(_.nonEmpty)) Some(new TestOutput(rankedHands) {})
      else None
    }
  }
}
