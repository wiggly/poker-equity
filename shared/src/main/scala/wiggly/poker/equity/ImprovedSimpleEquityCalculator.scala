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

class ImprovedSimpleEquityCalculator extends EquityCalculator {

  override def calculate(
      a: HoleCards,
      b: HoleCards,
      board: Set[Card],
      dead: Set[Card]
  ): Either[String, EquityCalculator.EquityResult] = {
    val cardCount = (HoleCards.size * 2) + board.size + dead.size
    val usedCards = a.cards ++ b.cards ++ board ++ dead
    val stub = Deck.create.removeAll(usedCards)

    if (usedCards.size != cardCount) {
      "A single card cannot be used by more than one hand or the board or dead"
        .asLeft[EquityResult]
    } else {
      generateEquityResult(a, b, board, stub).asRight[String]
    }
  }

  private def generateEquityResult(
      a: HoleCards,
      b: HoleCards,
      board: Set[Card],
      stub: Deck
  ): EquityResult = {
    val cardsRequired = 5 - board.size

    // maximum number of distinct boards we need to evaluate with the given hole cards to exhaustively generate equity
    val maxBoards = MathUtil.nCombK(stub.size.toBigInt, cardsRequired).toInt

    // number of boards we intend to evaluate - this should be a parameter?
    //    val count = maxBoards / 3
    //    val count = maxBoards / 5
    // val count = 5000
    val count = EquityCalculator.defaultRunSize
    // val count = 15000

    println(s"stub length: ${stub.toList.size}")

    // generate permutations of stub to generate boards and generate an equity result for the hole cards for that board
    val xxx: (Equity, Equity) = stub.toList.sorted
      .combinations(cardsRequired)
      .take(count)
      .map(extraBoard => {
        // System.err.println(extraBoard.map(_.show).mkString);
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
    import ImprovedSimpleEquityCalculator.orderPokerHand

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

object ImprovedSimpleEquityCalculator {

  given orderPokerHand: Order[PokerHand] = new Order[PokerHand] {
    override def compare(a: PokerHand, b: PokerHand): Int = {
      //    println("--------------------------------------------------------------------------")
      val rta = rankTally(a)
      val prca = pokerHandCategory(rta)(a)
      val rtb = rankTally(b)
      val prcb = pokerHandCategory(rtb)(b)

      val phrc = PokerRankCategory.orderPokerRankCategory.compare(prca, prcb)

      if (phrc == 0) {
        val xxx = prca match {
          case StraightFlush => compareStraightFlush(a, b)
          case FourOfAKind   => compareFourOfAKind(a, b)
          case FullHouse     => compareFullHouse(a, b)
          case Flush         => compareFlush(a, b)
          case Straight      => compareStraight(a, b)
          case ThreeOfAKind  => compareThreeOfAKind(a, b)
          case TwoPair       => compareTwoPair(rta, rtb)(a, b)
          case Pair          => comparePair(a, b)
          case HighCard      => compareHighCard(a, b)
        }
        /*
                if(xxx == 0) {
                  println(s"EXACT SAME HAND RANK $prca ($xxx)\n\t${a.show}\n\t${b.show}")
                } else {
                  println(s"SAME HAND RANK $prca ($xxx)\n\t${a.show}\n\t${b.show}")
                }
         */
        xxx
      } else {
        //           println(s"DIFF HAND RANK ($prca vs $prcb)\n\t${a.show}\n\t${b.show}")
        phrc
      }
    }
  }

  private def pokerHandCategory(
      rt: Map[Rank, Int]
  )(hand: PokerHand): PokerRankCategory = {
    if (rt.size == 5) {
      if (isPokerHandFlush(hand)) {
        if (isPokerHandStraight(hand)) {
          StraightFlush
        } else {
          Flush
        }
      } else if (isPokerHandStraight(hand)) {
        Straight
      } else {
        HighCard
      }
    } else {
      if (isPokerHandFourOfAKind(rt)(hand)) {
        FourOfAKind
      } else if (isPokerHandFullHouse(rt)(hand)) {
        FullHouse
      } else if (isPokerHandThreeOfAKind(rt)(hand)) {
        ThreeOfAKind
      } else if (isPokerHandTwoPair(rt)(hand)) {
        TwoPair
      } else if (isPokerHandPair(rt)(hand)) {
        Pair
      } else {
        HighCard
      }
    }
  }
  /*
  private def isPokerHandStraightFlush(hand: PokerHand): Boolean = {
    isPokerHandFlush(hand) && isPokerHandStraight(hand)
  }
   */
  private def isPokerHandFourOfAKind(rt: Map[Rank, Int])(
      @annotation.unused hand: PokerHand
  ): Boolean =
    rt.values.exists(_ == 4)

  private def isPokerHandFullHouse(
      rt: Map[Rank, Int]
  )(@annotation.unused hand: PokerHand): Boolean = {
    rt.values.exists(_ == 3) && rt.values.exists(_ == 2)
  }

  private def isPokerHandFlush(hand: PokerHand): Boolean =
    hand.suits.distinct.size == 1

  private def isPokerHandStraight(hand: PokerHand): Boolean = {
    straightCards.contains(hand.ranks.toSet)
  }

  private def isPokerHandThreeOfAKind(rt: Map[Rank, Int])(
      @annotation.unused hand: PokerHand
  ): Boolean =
    rt.values.exists(_ == 3)

  private def isPokerHandTwoPair(rt: Map[Rank, Int])(
      @annotation.unused hand: PokerHand
  ): Boolean =
    rt.count((_, actualCount) => actualCount == 2) == 2

  private def isPokerHandPair(rt: Map[Rank, Int])(
      @annotation.unused hand: PokerHand
  ): Boolean =
    rt.count((_, actualCount) => actualCount == 2) == 1

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
    //    println(s"compare straight indexA: ${straightCards.indexOf(a.toSet.map(_.rank))} - indexB: ${straightCards.indexOf(b.toSet.map(_.rank))}")

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

  private def compareTwoPair(
      rta: Map[Rank, Int],
      rtb: Map[Rank, Int]
  )(a: PokerHand, b: PokerHand): Int = {
    val pairsA = rta.filter((_, actualCount) => actualCount == 2).keys.toList
    val pairsB = rtb.filter((_, actualCount) => actualCount == 2).keys.toList

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

  private def comparePair(a: PokerHand, b: PokerHand): Int = {
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

  private def compareHighCard(a: PokerHand, b: PokerHand): Int =
    compareRankList(a.toList.map(_.rank), b.toList.map(_.rank))

  private def rankTally(x: PokerHand): Map[Rank, Int] =
    x.ranks
      .groupMap(identity)(identity)
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
}
