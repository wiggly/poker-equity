package wiggly.poker

import cats.implicits.*
import wiggly.poker.equity.ImprovedSimpleEquityCalculator.straightCards
import wiggly.poker.model.{PokerHand, Rank}

import scala.util.control.Breaks.*
package object equity {

  // rank count  | possible hand
  // 5           | high card abcde
  // 4           | pair XXabc
  // 3           | two-pair XXYYa OR 3oak XXXab
  // 2           | quads XXXXa OR full-house XXXYY
  // 1           | NA

  private val straightCards: List[Set[Rank]] = List(
    Set(Rank.Ace, Rank.Two, Rank.Three, Rank.Four, Rank.Five),
    Set(Rank.Two, Rank.Three, Rank.Four, Rank.Five, Rank.Six),
    Set(Rank.Three, Rank.Four, Rank.Five, Rank.Six, Rank.Seven),
    Set(Rank.Four, Rank.Five, Rank.Six, Rank.Seven, Rank.Eight),
    Set(Rank.Five, Rank.Six, Rank.Seven, Rank.Eight, Rank.Nine),
    Set(Rank.Six, Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten),
    Set(Rank.Seven, Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack),
    Set(Rank.Eight, Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen),
    Set(Rank.Nine, Rank.Ten, Rank.Jack, Rank.Queen, Rank.King),
    Set(Rank.Ten, Rank.Jack, Rank.Queen, Rank.King, Rank.Ace)
  )

  final case class QuickPokerHand(pokerHand: PokerHand) {
    val allRankOrd: Array[Int] = {
      val xxx = Array.ofDim[Int](13)
      var c = 0
      c = pokerHand.first.rank.ordinal
      xxx.update(c, 1)
      c = pokerHand.second.rank.ordinal
      xxx.update(c, xxx(c) + 1)
      c = pokerHand.third.rank.ordinal
      xxx.update(c, xxx(c) + 1)
      c = pokerHand.fourth.rank.ordinal
      xxx.update(c, xxx(c) + 1)
      c = pokerHand.fifth.rank.ordinal
      xxx.update(c, xxx(c) + 1)

      //println(s"${pokerHand.show} : ${xxx.mkString(",")}")
      xxx
    }

    private val distinctRankCount: Int = allRankOrd.count(_ != 0)
    
    lazy val isHighCard: Boolean = {
//      println(s"isHighCard ${pokerHand.show} - distinct rank count: $distinctRankCount")

      // 5 distinct ranks
      // NOT flush
      // NOT straight
      (distinctRankCount == 5) && !(isFlush) && (!isStraight)
    }

    lazy val isPair: Boolean = distinctRankCount == 4

    lazy val isTwoPair: Boolean = (distinctRankCount == 3) && !isThreeOfAKind

    lazy val isThreeOfAKind: Boolean =
      (distinctRankCount == 3) && allRankOrd.contains(3)

    lazy val isFlush: Boolean = {
      (pokerHand.first.suit == pokerHand.second.suit) &&
        (pokerHand.second.suit == pokerHand.third.suit) &&
        (pokerHand.third.suit == pokerHand.fourth.suit) &&
        (pokerHand.fourth.suit == pokerHand.fifth.suit)
    }
    
    lazy val isStraight: Boolean = 
      straightCards.contains(pokerHand.ranks.toSet)

    lazy val isFullHouse: Boolean =
      distinctRankCount == 2 && allRankOrd.contains(3)

    lazy val isFourOfAKind: Boolean = distinctRankCount == 2 && !isFullHouse
    
    def pairRank: Int = {
      //println(s"pairRank: ${allRankOrd.mkString(",")}")
      allRankOrd.indexOf(2)
    }
  }

  // straight flush

  // ranks bitset
  // - compare to straight bitsets for straights
  // suits bitset
  // - compare to single suit bitsets for flush

  // can we make a number that represents the ordinal for the hand?
  // i.e. straight flush ace high
  // top bits can be used for poker rank in order so any straight flush beats any 4oak
  // further down bits can encode discriminators for other poker hand ranks
  // 4oak -

  object QuickPokerHand {

    def compareCountRanks(
        a: QuickPokerHand,
        b: QuickPokerHand,
        count: Int
    ): Int = {
      //     println(s"compare ranks:\n\t${a.allRankOrd.mkString(",")}\n\t${b.allRankOrd.mkString(",")}")

      val ca = a.allRankOrd
      val cb = b.allRankOrd

      var idx = ca.length - 1

        while (idx >= 0) {
          breakable {
            val cav = ca(idx)
          if (cav != 0 && cav != count) break();

          val cbv = cb(idx)
          if (cbv != 0 && cbv != count) break();

          // terminate early if one is bigger
          if (cav > cbv) {
            return 1
          } else if (cbv > cav) {
            return -1
          }
          }
          idx = idx - 1
      }

      // default equality at the end
      return 0
    }

    def compareRanks(a: QuickPokerHand, b: QuickPokerHand): Int = {
      //     println(s"compare ranks:\n\t${a.allRankOrd.mkString(",")}\n\t${b.allRankOrd.mkString(",")}")

      var idx = a.allRankOrd.length - 1

      while (idx >= 0) {
        // terminate early if one is bigger
        if (a.allRankOrd(idx) > b.allRankOrd(idx)) {
          return 1
        } else if (b.allRankOrd(idx) > a.allRankOrd(idx)) {
          return -1
        }
        idx = idx - 1
      }

      // default equality at the end
      return 0

      // hand-unrolled loop
      // TODO: do we need to do this with arrays or not?
      /*
      if (a.rankOrd(4) > b.rankOrd(4)) {
        1
      } else if (a.rankOrd(4) < b.rankOrd(4)) {
        -1
      } else if (a.rankOrd(3) > b.rankOrd(3)) {
        1
      } else if (a.rankOrd(3) < b.rankOrd(3)) {
        -1
      } else if (a.rankOrd(2) > b.rankOrd(2)) {
        1
      } else if (a.rankOrd(2) < b.rankOrd(2)) {
        -1
      } else if (a.rankOrd(1) > b.rankOrd(1)) {
        1
      } else if (a.rankOrd(1) < b.rankOrd(1)) {
        -1
      } else if (a.rankOrd(0) > b.rankOrd(0)) {
        1
      } else if (a.rankOrd(0) < b.rankOrd(0)) {
        -1
      } else {
        0
      }
       */
    }

  }

}
