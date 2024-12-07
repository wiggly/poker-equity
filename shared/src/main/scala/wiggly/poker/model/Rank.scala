package wiggly.poker.model

import cats.Order
import cats.Show

enum Rank {
  case Two
  case Three
  case Four
  case Five
  case Six
  case Seven
  case Eight
  case Nine
  case Ten
  case Jack
  case Queen
  case King
  case Ace
}

object Rank {
  given showRank: Show[Rank] = new Show[Rank] {
    override def show(t: Rank): String = t match
      case Two   => "2"
      case Three => "3"
      case Four  => "4"
      case Five  => "5"
      case Six   => "6"
      case Seven => "7"
      case Eight => "8"
      case Nine  => "9"
      case Ten   => "T"
      case Jack  => "J"
      case Queen => "Q"
      case King  => "K"
      case Ace   => "A"
  }

  given orderRank: Order[Rank] =
    Order.by(_.ordinal)
}
