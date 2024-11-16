package wiggly.poker

import cats.implicits.*
import cats.Show

final case class Card(suit: Suit, rank: Rank)

object Card {

  given showCard: Show[Card] = new Show[Card] {
    override def show(t: Card): String = s"${t.rank.show}${t.suit.show}"
  }

}