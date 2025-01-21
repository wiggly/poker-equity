package wiggly.poker

import cats.implicits.*
import cats.Show

import scala.collection.immutable.List

package object model {

  /** A deck of cards
    */
  opaque type Deck = Set[Card]

  object Deck {
    val complete: Set[Card] = {
      (for {
        s <- Suit.values
        r <- Rank.values
      } yield model.Card(s, r)).toSet
    }

    def create: Deck = complete

    extension (d: Deck) {
      def toList: List[Card] = d.toList

      def remove(c: Card): Deck = d - c

      def removeAll(cs: Set[Card]): Deck = d.removedAll(cs)

      def size: Int = d.size
    }
  }

  /** HoleCards is exactly 2 distinct cards
    */
  opaque type HoleCards = Set[Card]

  object HoleCards {
    val size: Int = 2

    extension (hc: HoleCards) def cards: Set[Card] = hc

    def create(a: Card, b: Card): Either[String, HoleCards] = {
      val cards = Set(a, b)
      Either.cond(
        cards.size == 2,
        cards,
        "HoldEm requires two different hole cards"
      )
    }

    def fromIterable(cards: Iterable[Card]): Iterator[HoleCards] = {
      if (cards.size >= size) {
        cards.toList
          .combinations(size)
          .collect({ case a :: b :: Nil =>
            Set(a, b)
          })
      } else {
        Iterator.empty[HoleCards]
      }
    }
  }

  /** PokerHand is exactly 5 distinct cards
    */
  opaque type PokerHand = (Card, Card, Card, Card, Card)

  object PokerHand {
    val size: Int = 5

    extension (hand: PokerHand) {
      def toBits: Long = {
        1L << (hand._1.rank.ordinal + (hand._1.suit.ordinal * 13)) |
          1L << (hand._2.rank.ordinal + (hand._2.suit.ordinal * 13)) |
          1L << (hand._3.rank.ordinal + (hand._3.suit.ordinal * 13)) |
          1L << (hand._4.rank.ordinal + (hand._4.suit.ordinal * 13)) |
          1L << (hand._5.rank.ordinal + (hand._5.suit.ordinal * 13))
      }

      def toSet: Set[Card] = Set(hand._1, hand._2, hand._3, hand._4, hand._5)

      def toList: List[Card] = List(hand._1, hand._2, hand._3, hand._4, hand._5)

      def suits: List[Suit] = List(
        hand._1.suit,
        hand._2.suit,
        hand._3.suit,
        hand._4.suit,
        hand._5.suit
      )

      def ranks: List[Rank] = List(
        hand._1.rank,
        hand._2.rank,
        hand._3.rank,
        hand._4.rank,
        hand._5.rank
      )

      def first: Card = hand._1
      def second: Card = hand._2
      def third: Card = hand._3
      def fourth: Card = hand._4
      def fifth: Card = hand._5
    }

    def fromCards(
        a: Card,
        b: Card,
        c: Card,
        d: Card,
        e: Card
    ): Option[PokerHand] = {
      Option.when(Set(a, b, c, d, e).size == 5)((a, b, c, d, e): PokerHand)
    }

    def fromIterable(cards: Iterable[Card]): Iterator[PokerHand] = {
      if (cards.size >= size) {
        cards.toList
          .combinations(size)
          .collect({ case a :: b :: c :: d :: e :: Nil =>
            (a, b, c, d, e): PokerHand
          })
      } else {
        Iterator.empty[PokerHand]
      }
    }

    given showPokerHand: Show[PokerHand] = new Show[PokerHand] {
      override def show(t: (Card, Card, Card, Card, Card)): String = {
        s"${t._1.show} ${t._2.show} ${t._3.show} ${t._4.show} ${t._5.show}"
      }
    }
  }
}
