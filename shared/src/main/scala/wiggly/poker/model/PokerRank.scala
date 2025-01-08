package wiggly.poker.model

import cats.Order
import cats.Show
import cats.implicits.*
import wiggly.poker.model.{PokerHand, Rank}

import wiggly.poker.model.*

sealed trait PokerRank {
  def rank: Byte
}

object PokerRank {

  /**
   * Bitset containing the Rank ordinals of the cards present
   */
  type Kickers = Int

  final case class StraightFlush(highRank: Rank) extends PokerRank {
    def rank = 8
  }

  final case class FourOfAKind(quadsRank: Rank, kicker: Rank)
      extends PokerRank {
    def rank = 7
  }

  final case class FullHouse(tripsRank: Rank, pairRank: Rank)
      extends PokerRank {
    def rank = 6
  }

  final case class Flush(kickers: Kickers) extends PokerRank {
    def rank = 5
  }

  final case class Straight(highRank: Rank) extends PokerRank {
    def rank = 4
  }

  final case class ThreeOfAKind(tripsRank: Rank, kickers: Kickers)
      extends PokerRank {
    def rank = 3
  }

  final case class TwoPair(pairsRanks: Kickers, kicker: Rank)
      extends PokerRank {
    def rank = 2
  }

  final case class Pair(pairRank: Rank, kickers: Kickers) extends PokerRank {
    def rank = 1
  }

  final case class HighCard(kickers: Kickers) extends PokerRank {
    def rank = 0
  }

  given orderPokerRank: Order[PokerRank] = new Order[PokerRank] {
    override def compare(a: PokerRank, b: PokerRank): Int =
      val result = Order.compare(a.rank, b.rank)
      if (result == 0) {
        (a, b) match {
          case (StraightFlush(aHighRank), StraightFlush(bHighRank)) =>
            Order.compare(aHighRank, bHighRank)

          case (
                FourOfAKind(aQuadsRank: Rank, aKicker: Rank),
                FourOfAKind(bQuadsRank: Rank, bKicker: Rank)
              ) =>
            val result = Order.compare(aQuadsRank, bQuadsRank)
            if (result == 0) {
              Order.compare(aKicker, bKicker)
            } else {
              result
            }

          case (
                FullHouse(aTripsRank: Rank, aPairRank: Rank),
                FullHouse(bTripsRank: Rank, bPairRank: Rank)
              ) =>
            val result = Order.compare(aTripsRank, bTripsRank)
            if (result == 0) {
              Order.compare(aPairRank, bPairRank)
            } else {
              result
            }

          case (Flush(aKickers), Flush(bKickers)) =>
            Order.compare(aKickers, bKickers)

          case (Straight(aHighRank), Straight(bHighRank)) =>
            Order.compare(aHighRank, bHighRank)

          case (
                ThreeOfAKind(aTripsRank, aKickers),
                ThreeOfAKind(bTripsRank, bKickers)
              ) =>
            val result = Order.compare(aTripsRank, bTripsRank)
            if (result == 0) {
              Order.compare(aKickers, bKickers)
            } else {
              result
            }

          case (TwoPair(aPairsRanks, aKicker), TwoPair(bPairsRanks, bKicker)) =>
            val result = Order.compare(aPairsRanks, bPairsRanks)
            if (result == 0) {
              Order.compare(aKicker, bKicker)
            } else {
              result
            }

          case (Pair(aPairRank, aKickers), Pair(bPairRank, bKickers)) =>
            val result = Order.compare(aPairRank, bPairRank)
            if (result == 0) {
              Order.compare(aKickers, bKickers)
            } else {
              result
            }

          case (HighCard(aKickers), HighCard(bKickers)) =>
            Order.compare(aKickers, bKickers)
        }
      } else {
        result
      }
  }

  given orderPokerHand: Order[PokerHand] = new Order[PokerHand] {
    override def compare(a: PokerHand, b: PokerHand): Int = {
      val pra = rankHand(a)
      val prb = rankHand(b)
      Order.compare(pra, prb)
    }
  }

  def rankHand(pokerHand: PokerHand): PokerRank = {
    val ranks = pokerHand.ranks

    val kickers = ranksKickers(ranks)

    val flush: Boolean =
      (pokerHand.first.suit == pokerHand.second.suit) &&
        (pokerHand.second.suit == pokerHand.third.suit) &&
        (pokerHand.third.suit == pokerHand.fourth.suit) &&
        (pokerHand.fourth.suit == pokerHand.fifth.suit)

    if(straightKickers.contains(kickers)) {
      // calculate high straight rank - ignore Ace if wheel
      val straightHighrank = if (pokerHand.ranks.contains(Rank.Ace) && pokerHand.ranks.contains(Rank.Two)) {
        Rank.Five
      } else {
        pokerHand.ranks.max
      }

      // straight or straight flush
      if(flush) {
        StraightFlush(straightHighrank)
      } else {
        Straight(straightHighrank)
      }
    } else if(flush) {
      // flush
      Flush(kickers)
    } else {
      val rankCounts = countRank(pokerHand)

      if(rankCounts.contains(4)) {
        // quads
        FourOfAKind(Rank.fromOrdinal(rankCounts.indexOf(4)), Rank.fromOrdinal(rankCounts.indexOf(1)))
      } else if(rankCounts.contains(3) && rankCounts.contains(2)) {
        // boat
        FullHouse(Rank.fromOrdinal(rankCounts.indexOf(3)), Rank.fromOrdinal(rankCounts.indexOf(2)))
      } else if(rankCounts.contains(3)) {
        // trips
        ThreeOfAKind(Rank.fromOrdinal(rankCounts.indexOf(3)), rankCountKickers(rankCounts))
      } else if (rankCounts.contains(2)) {
        if(rankCounts.count(n => n != 0) == 3) {
          // 2P
          TwoPair(
            rankCountKickers(rankCounts.map(n => if(n>0) { n-1} else { n })),
            Rank.fromOrdinal(rankCounts.indexOf(1))
          )
        } else {
          // P
          Pair(Rank.fromOrdinal(rankCounts.indexOf(2)), rankCountKickers(rankCounts))
        }
      } else {
        HighCard(kickers)
      }
    }
  }

  /**
   * Generate Kickers from PokerHand
   */
  private def pokerHandKickers(pokerHand: PokerHand): Kickers =
    ranksKickers(pokerHand.ranks)

  /**
   * Generate Kickers from ranks
   */
  private def ranksKickers(ranks: IterableOnce[Rank]): Kickers = {
    ranks.iterator.foldLeft[Kickers](0)((acc, bit) => acc | (1 << bit.ordinal))
  }

  /**
   * Generate Kickers from rank counts - only count singles
   */
  private def rankCountKickers(counts: IterableOnce[Int]): Kickers = {
    counts.iterator.zipWithIndex.foldLeft[Kickers](0)((acc, countIndex) =>
      if(countIndex._1 == 1) {
        acc | (1 << countIndex._2)
      } else {
        acc
      }
    )
  }

  /**
   * Construct all Kickers that represent straights
   */
  val straightKickers: Array[Kickers] = {
    val values = Rank.values.reverse
    val ace = values.head
    val xxx: List[Array[Rank]] = values.sliding(5).toList
    val yyy: Array[Rank] = values.takeRight(4).appended(ace)
    val zzz: List[Array[Rank]] = xxx :+ yyy
    val ddd = zzz
      .map(ranksKickers(_))
      .toArray

    ddd
  }

  private def countRank(pokerHand: PokerHand): Array[Int] = {
    val result = Array.ofDim[Int](13)
    var c = 0
    c = pokerHand.first.rank.ordinal
    result.update(c, 1)
    c = pokerHand.second.rank.ordinal
    result.update(c, result(c) + 1)
    c = pokerHand.third.rank.ordinal
    result.update(c, result(c) + 1)
    c = pokerHand.fourth.rank.ordinal
    result.update(c, result(c) + 1)
    c = pokerHand.fifth.rank.ordinal
    result.update(c, result(c) + 1)
    result
  }
}
