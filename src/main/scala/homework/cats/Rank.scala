package homework.cats

sealed trait Rank {
  val rank: Int
  val rankString: String
}
object Rank {
  case object Two extends Rank {
    val rank = 2
    val rankString = "2"
  }
  case object Three extends Rank {
    val rank = 3
    val rankString = "3"
  }
  case object Four extends Rank {
    val rank = 4
    val rankString = "4"
  }
  case object Five extends Rank {
    val rank = 5
    val rankString = "5"
  }
  case object Six extends Rank {
    val rank = 6
    val rankString = "6"
  }
  case object Seven extends Rank {
    val rank = 7
    val rankString = "7"
  }
  case object Eight extends Rank {
    val rank = 8
    val rankString = "8"
  }
  case object Nine extends Rank {
    val rank = 9
    val rankString = "9"
  }
  case object Ten extends Rank {
    val rank = 10
    val rankString = "10"
  }
  case object Jack extends Rank {
    val rank = 11
    val rankString = "J"
  }
  case object Queen extends Rank {
    val rank = 12
    val rankString = "Q"
  }

  case object King extends Rank {
    val rank = 13
    val rankString = "K"
  }
  case object Ace extends Rank {
    val rank = 14
    val rankString = "A"
  }

  def create(rank: String): Rank = rank match {
    case "2"  => Two
    case "3"  => Three
    case "4"  => Four
    case "5"  => Five
    case "6"  => Six
    case "7"  => Seven
    case "8"  => Eight
    case "9"  => Nine
    case "10" => Ten
    case "11" => Jack
    case "12" => Queen
    case "13" => King
    case "14" => Ace
  }

}
