package wiggly.poker.model

import cats.Order
import cats.Show

enum Suit {
  case Spade
  case Heart
  case Diamond
  case Club
}

object Suit {
  given showSuit: Show[Suit] = new Show[Suit] {
    override def show(t: Suit): String = t match
      case Spade   => "s"
      case Heart   => "h"
      case Diamond => "d"
      case Club    => "c"
  }

  given orderSuit: Order[Suit] =
    Order.by(_.ordinal)
}
