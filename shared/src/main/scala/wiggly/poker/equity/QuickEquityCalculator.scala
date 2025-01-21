package wiggly.poker.equity

import cats.implicits.*
import cats.Order
import wiggly.poker.model.{
  Card,
  Deck,
  HoleCards,
  PokerHand,
  PokerRankCategory,
  Rank
}
import wiggly.poker.MathUtil
import wiggly.poker.equity.EquityCalculator.{Equity, EquityResult}
import wiggly.poker.equity.*
import wiggly.poker.model.PokerRankCategory.{
  Flush,
  FourOfAKind,
  FullHouse,
  HighCard,
  Pair,
  Straight,
  StraightFlush,
  ThreeOfAKind,
  TwoPair
}

import math.BigDecimal.int2bigDecimal

class QuickEquityCalculator extends EquityCalculator {

  override def calculate(
      a: HoleCards,
      b: HoleCards,
      board: Set[Card],
      dead: Set[Card],
      coverage: Option[Float]
  ): Either[String, EquityCalculator.EquityResult] = {
    val cardCount = (HoleCards.size * 2) + board.size + dead.size
    val usedCards = a.cards ++ b.cards ++ board ++ dead
    val stub = Deck.create.removeAll(usedCards)

    if (usedCards.size != cardCount) {
      "A single card cannot be used by more than one hand or the board or dead"
        .asLeft[EquityResult]
    } else {
      generateEquityResult(a, b, board, stub, coverage).asRight[String]
    }
  }

  private def generateEquityResult(
      a: HoleCards,
      b: HoleCards,
      board: Set[Card],
      stub: Deck,
      coverage: Option[Float]
  ): EquityResult = {
    val cardsRequired = 5 - board.size

    // maximum number of distinct boards we need to evaluate with the given hole cards to exhaustively generate equity
    val maxBoards = MathUtil.nCombK(stub.size.toBigInt, cardsRequired).toInt

    // val count = EquityCalculator.defaultRunSize
    val count = EquityCalculator.boardCountForPercentage(maxBoards, coverage)
    println(
      s"maxBoards: $maxBoards - coverage: ${coverage} - boards to examine: $count"
    )

    // generate permutations of stub to generate boards and generate an equity result for the hole cards for that board
    val xxx: (Equity, Equity) = stub.toList.sorted
      .combinations(cardsRequired)
      .take(count)
      .map(extraBoard => {
        generateBoardEquityResult(a, b, board ++ extraBoard)
      })
      .toList
      .combineAll

    EquityResult(maxBoards, count, (a, xxx._1), (b, xxx._2))
  }

  private def generateBoardEquityResult(
      a: HoleCards,
      b: HoleCards,
      board: Set[Card]
  ): (Equity, Equity) = {
    import QuickEquityCalculator.orderPokerHand

    val bestA = PokerHand.fromIterable(a.cards ++ board).max

    val bestB = PokerHand.fromIterable(b.cards ++ board).max

    val result = Order.compare(bestA, bestB)

    if (result < 0) {
      (Equity(0, 1, 0), Equity(1, 0, 0))
    } else if (result > 0) {
      (Equity(1, 0, 0), Equity(0, 1, 0))
    } else {
      (Equity(0, 0, 1), Equity(0, 0, 1))
    }
  }
}

object QuickEquityCalculator {
  given orderPokerHand: Order[PokerHand] = new Order[PokerHand] {
    override def compare(a: PokerHand, b: PokerHand): Int = {
      // println(s"COMPARE\n\t${a.show}\n\t${b.show}")

      val qa = QuickPokerHand(a)

      val qb = QuickPokerHand(b)

      fallbackCompare(qa, qb)

      /*
            if (qa.isHighCard) {
              if (qb.isHighCard) {
                // QA and QB are high card - compare ranks
                QuickPokerHand.compareRanks(qa, qb)
              } else {
                // QA is high card, QB is something else, QB must win, whatever it is
                -1
              }
            } else if (qb.isHighCard) {
              // QB is high card, QA is something else, QA must win, whatever it is
              1
            } else {
              // fallback to improved logic
              fallbackCompare(qa, qb)
            }
       */

    }

    private def fallbackCompare(a: QuickPokerHand, b: QuickPokerHand): Int = {
      //    println("--------------------------------------------------------------------------")
      val prca = pokerHandCategory(a)
      val prcb = pokerHandCategory(b)

      val phrc = PokerRankCategory.orderPokerRankCategory.compare(prca, prcb)

      if (phrc == 0) {
        val xxx = prca match {
          case StraightFlush => compareStraightFlush(a.pokerHand, b.pokerHand)
          case FourOfAKind   => compareFourOfAKind(a.pokerHand, b.pokerHand)
          case FullHouse     => compareFullHouse(a.pokerHand, b.pokerHand)
          case Flush         => compareFlush(a.pokerHand, b.pokerHand)
          case Straight      => compareStraight(a.pokerHand, b.pokerHand)
          case ThreeOfAKind  => compareThreeOfAKind(a.pokerHand, b.pokerHand)
          case TwoPair       => compareTwoPair(a.pokerHand, b.pokerHand)
          case Pair          => comparePair(a, b)
          case HighCard      => compareHighCard(a.pokerHand, b.pokerHand)
        }
        /*
                if(xxx == 0) {
                  println(s"EXACT SAME HAND RANK $prca ($xxx)\n\t${a.pokerHand.show}\n\t${b.pokerHand.show}")
                } else {
                  println(s"SAME HAND RANK $prca ($xxx)\n\t${a.pokerHand.show}\n\t${b.pokerHand.show}")
                }
         */
        xxx
      } else {
//      println(s"DIFF HAND RANK ($prca vs $prcb)\n\t${a.pokerHand.show}\n\t${b.pokerHand.show}")
        phrc
      }
    }
  }

  private def pokerHandCategory(hand: QuickPokerHand): PokerRankCategory = {
    if (hand.isHighCard) {
      HighCard
    } else if (hand.isPair) {
      Pair
    } else if (hand.isTwoPair) {
      TwoPair
    } else if (hand.isThreeOfAKind) {
      ThreeOfAKind
    } else if (hand.isFullHouse) {
      FullHouse
    } else if (hand.isFourOfAKind) {
      FourOfAKind
    } else if (hand.isStraight) {
      if (hand.isFlush) {
        StraightFlush
      } else {
        Straight
      }
    } else if (hand.isFlush) {
      Flush
    } else {
      HighCard
    }
  }

  private def compareStraightFlush(a: PokerHand, b: PokerHand): Int = {
    Order.compare(
      straightCards.indexOf(a.toSet.map(_.rank)),
      straightCards.indexOf(b.toSet.map(_.rank))
    )
  }

  private def compareFourOfAKind(a: PokerHand, b: PokerHand): Int = {
    val ga = a.toList
      .groupBy(_.rank)
      .map(item => (item._1, item._2.size))

    val gb = b.toList
      .groupBy(_.rank)
      .map(item => (item._1, item._2.size))

    val result = for {
      a4 <- ga.find(item => item._2 == 4).map(_._1)
      a1 <- ga.find(item => item._2 == 1).map(_._1)

      b4 <- gb.find(item => item._2 == 4).map(_._1)
      b1 <- gb.find(item => item._2 == 1).map(_._1)
      result =
        if (a4 == b4) {
          Order.compare(a1, b1)
        } else {
          Order.compare(a4, b4)
        }
    } yield result

    result.getOrElse(
      throw new Exception(
        "Cannot happen unless this function is fed a non Quads hand"
      )
    )
  }

  private def compareFullHouse(a: PokerHand, b: PokerHand): Int = {
    val tallyA = a.toList
      .groupBy(_.rank)
      .map(item => (item._1, item._2.size))

    val tallyB = b.toList
      .groupBy(_.rank)
      .map(item => (item._1, item._2.size))

    val tripCmp: Option[Int] = for {
      tripRankA <- tallyA
        .find((_, count) => count == 3)
        .map(_._1.ordinal) // TODO: remove ordinal from this
      tripRankB <- tallyB.find((_, count) => count == 3).map(_._1.ordinal)
    } yield tripRankA.compare(tripRankB)

    // trips are the same
    if (tripCmp.contains(0)) {
      val pa = tallyA.filter(_._2 == 2).map(x => x._1.ordinal)
      val pb = tallyB.filter(_._2 == 2).map(x => x._1.ordinal)
      pa
        .zip(pb)
        .map((oa, ob) => oa - ob)
        .find(n => n != 0)
        .getOrElse(0)
    } else {
      tripCmp.get
    }
  }

  private def compareFlush(a: PokerHand, b: PokerHand): Int =
    compareRankList(
      a.toList.map(_.rank),
      b.toList.map(_.rank)
    )

  private def compareStraight(a: PokerHand, b: PokerHand): Int = {
    Order.compare(
      straightCards.indexOf(a.toSet.map(_.rank)),
      straightCards.indexOf(b.toSet.map(_.rank))
    )
  }

  private def compareThreeOfAKind(a: PokerHand, b: PokerHand): Int = {
    val maybeTripA = rankForCount(a, 3)
    val maybeTripB = rankForCount(b, 3)

    val tripCmp = (for {
      tripA <- maybeTripA
      tripB <- maybeTripB
      result =
        if (Order.eqv(tripA, tripB)) {
          compareRankList(
            a.toList.map(_.rank).filterNot(_ == tripA),
            b.toList.map(_.rank).filterNot(_ == tripB)
          )
        } else {
          Order.compare(tripA, tripB)
        }
    } yield result).getOrElse(
      throw new Exception("Three of a kind does not contain three of a kind")
    )

    tripCmp
  }

  private def compareTwoPair(a: PokerHand, b: PokerHand): Int = {
    val pairsA = ranksForCount(a, 2)
    val pairsB = ranksForCount(b, 2)

    val pairsCmp = compareRankList(pairsA, pairsB)

    if (pairsCmp == 0) {
      compareRankList(
        a.toList.map(_.rank).filterNot(pairsA.contains),
        b.toList.map(_.rank).filterNot(pairsB.contains)
      )
    } else {
      pairsCmp
    }
  }

  private def comparePair(a: QuickPokerHand, b: QuickPokerHand): Int = {
    val pr = Order.compare(a.pairRank, b.pairRank)
    if (pr == 0) {
      QuickPokerHand.compareCountRanks(a, b, 1)
    } else {
      pr
    }
  }

  private def compareHighCard(a: PokerHand, b: PokerHand): Int =
    compareRankList(a.toList.map(_.rank), b.toList.map(_.rank))

  private def rankTally(x: PokerHand): Map[Rank, Int] =
    x.toList
      .groupBy(_.rank)
      .map(item => (item._1, item._2.size))

  private def ranksForCount(x: PokerHand, count: Int): List[Rank] =
    rankTally(x).filter((_, actualCount) => actualCount == count).keys.toList

  private def rankForCount(x: PokerHand, count: Int): Option[Rank] =
    ranksForCount(x, count).ensuring(_.length < 2).headOption

  private def compareRankList(a: List[Rank], b: List[Rank]): Int = {
    val ra = a.map(_.ordinal).sorted.reverse
    val rb = b.map(_.ordinal).sorted.reverse
    ra
      .zip(rb)
      .map((oa, ob) => oa - ob)
      .find(n => n != 0)
      .getOrElse(0)
  }

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

}
