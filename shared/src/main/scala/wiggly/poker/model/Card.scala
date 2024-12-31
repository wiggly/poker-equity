package wiggly.poker.model

import cats.Order
import cats.Show
import cats.implicits.*
import wiggly.poker.instances.SuitInstances

final case class Card(suit: Suit, rank: Rank)

object Card {
  import SuitInstances.showSuitSymbol

  // Spades
  val cAs: Card = Card(Suit.Spade, Rank.Ace)
  val cKs: Card = Card(Suit.Spade, Rank.King)
  val cQs: Card = Card(Suit.Spade, Rank.Queen)
  val cJs: Card = Card(Suit.Spade, Rank.Jack)
  val cTs: Card = Card(Suit.Spade, Rank.Ten)
  val c9s: Card = Card(Suit.Spade, Rank.Nine)
  val c8s: Card = Card(Suit.Spade, Rank.Eight)
  val c7s: Card = Card(Suit.Spade, Rank.Seven)
  val c6s: Card = Card(Suit.Spade, Rank.Six)
  val c5s: Card = Card(Suit.Spade, Rank.Five)
  val c4s: Card = Card(Suit.Spade, Rank.Four)
  val c3s: Card = Card(Suit.Spade, Rank.Three)
  val c2s: Card = Card(Suit.Spade, Rank.Two)

  // Hearts
  val cAh: Card = Card(Suit.Heart, Rank.Ace)
  val cKh: Card = Card(Suit.Heart, Rank.King)
  val cQh: Card = Card(Suit.Heart, Rank.Queen)
  val cJh: Card = Card(Suit.Heart, Rank.Jack)
  val cTh: Card = Card(Suit.Heart, Rank.Ten)
  val c9h: Card = Card(Suit.Heart, Rank.Nine)
  val c8h: Card = Card(Suit.Heart, Rank.Eight)
  val c7h: Card = Card(Suit.Heart, Rank.Seven)
  val c6h: Card = Card(Suit.Heart, Rank.Six)
  val c5h: Card = Card(Suit.Heart, Rank.Five)
  val c4h: Card = Card(Suit.Heart, Rank.Four)
  val c3h: Card = Card(Suit.Heart, Rank.Three)
  val c2h: Card = Card(Suit.Heart, Rank.Two)

  // Diamonds
  val cAd: Card = Card(Suit.Diamond, Rank.Ace)
  val cKd: Card = Card(Suit.Diamond, Rank.King)
  val cQd: Card = Card(Suit.Diamond, Rank.Queen)
  val cJd: Card = Card(Suit.Diamond, Rank.Jack)
  val cTd: Card = Card(Suit.Diamond, Rank.Ten)
  val c9d: Card = Card(Suit.Diamond, Rank.Nine)
  val c8d: Card = Card(Suit.Diamond, Rank.Eight)
  val c7d: Card = Card(Suit.Diamond, Rank.Seven)
  val c6d: Card = Card(Suit.Diamond, Rank.Six)
  val c5d: Card = Card(Suit.Diamond, Rank.Five)
  val c4d: Card = Card(Suit.Diamond, Rank.Four)
  val c3d: Card = Card(Suit.Diamond, Rank.Three)
  val c2d: Card = Card(Suit.Diamond, Rank.Two)

  // Clubs
  val cAc: Card = Card(Suit.Club, Rank.Ace)
  val cKc: Card = Card(Suit.Club, Rank.King)
  val cQc: Card = Card(Suit.Club, Rank.Queen)
  val cJc: Card = Card(Suit.Club, Rank.Jack)
  val cTc: Card = Card(Suit.Club, Rank.Ten)
  val c9c: Card = Card(Suit.Club, Rank.Nine)
  val c8c: Card = Card(Suit.Club, Rank.Eight)
  val c7c: Card = Card(Suit.Club, Rank.Seven)
  val c6c: Card = Card(Suit.Club, Rank.Six)
  val c5c: Card = Card(Suit.Club, Rank.Five)
  val c4c: Card = Card(Suit.Club, Rank.Four)
  val c3c: Card = Card(Suit.Club, Rank.Three)
  val c2c: Card = Card(Suit.Club, Rank.Two)

  def parse(card: String): Either[String, Card] = {
    card match {
      case "As" => cAs.asRight[String]
      case "Ks" => cKs.asRight[String]
      case "Qs" => cQs.asRight[String]
      case "Js" => cJs.asRight[String]
      case "Ts" => cTs.asRight[String]
      case "9s" => c9s.asRight[String]
      case "8s" => c8s.asRight[String]
      case "7s" => c7s.asRight[String]
      case "6s" => c6s.asRight[String]
      case "5s" => c5s.asRight[String]
      case "4s" => c4s.asRight[String]
      case "3s" => c3s.asRight[String]
      case "2s" => c2s.asRight[String]

      case "Ah" => cAh.asRight[String]
      case "Kh" => cKh.asRight[String]
      case "Qh" => cQh.asRight[String]
      case "Jh" => cJh.asRight[String]
      case "Th" => cTh.asRight[String]
      case "9h" => c9h.asRight[String]
      case "8h" => c8h.asRight[String]
      case "7h" => c7h.asRight[String]
      case "6h" => c6h.asRight[String]
      case "5h" => c5h.asRight[String]
      case "4h" => c4h.asRight[String]
      case "3h" => c3h.asRight[String]
      case "2h" => c2h.asRight[String]

      case "Ad" => cAd.asRight[String]
      case "Kd" => cKd.asRight[String]
      case "Qd" => cQd.asRight[String]
      case "Jd" => cJd.asRight[String]
      case "Td" => cTd.asRight[String]
      case "9d" => c9d.asRight[String]
      case "8d" => c8d.asRight[String]
      case "7d" => c7d.asRight[String]
      case "6d" => c6d.asRight[String]
      case "5d" => c5d.asRight[String]
      case "4d" => c4d.asRight[String]
      case "3d" => c3d.asRight[String]
      case "2d" => c2d.asRight[String]

      case "Ac" => cAc.asRight[String]
      case "Kc" => cKc.asRight[String]
      case "Qc" => cQc.asRight[String]
      case "Jc" => cJc.asRight[String]
      case "Tc" => cTc.asRight[String]
      case "9c" => c9c.asRight[String]
      case "8c" => c8c.asRight[String]
      case "7c" => c7c.asRight[String]
      case "6c" => c6c.asRight[String]
      case "5c" => c5c.asRight[String]
      case "4c" => c4c.asRight[String]
      case "3c" => c3c.asRight[String]
      case "2c" => c2c.asRight[String]

      case str => s"Cannot parse '$str' as a Card".asLeft[Card]
    }
  }

  given showCard: Show[Card] = new Show[Card] {
    override def show(t: Card): String = s"${t.rank.show}${t.suit.show}"
  }

  given orderCard: Order[Card] = new Order[Card] {

    override def compare(x: Card, y: Card): Int = {
      val rankOrder = Order.compare(x.rank, y.rank)
      if (rankOrder == 0) {
        Order.compare(x.suit, y.suit)
      } else {
        rankOrder
      }
    }
  }

}
