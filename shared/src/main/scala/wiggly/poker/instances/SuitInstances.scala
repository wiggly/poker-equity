package wiggly.poker.instances

import cats.Show
import wiggly.poker.model.Suit
import wiggly.poker.model.Suit.{Club, Diamond, Heart, Spade}

/** Other typeclass instances for Suit
  */
object SuitInstances {
  given showSuitSymbol: Show[Suit] = new Show[Suit] {
    override def show(t: Suit): String = t match
      case Spade   => "♠ "
      case Heart   => "♥ "
      case Diamond => "♦ "
      case Club    => "♣ "
  }
}
