package wiggly.poker.equity

import cats.effect.{IO, Resource, Sync}
import cats.implicits.*
import cats.{Applicative, Order}
import wiggly.poker.model.{Card, Deck, HoleCards, PokerHand, PokerRank, PokerRankCategory, Rank}
import wiggly.poker.MathUtil
import wiggly.poker.equity.EquityCalculator.{Equity, EquityResult, defaultRunSize}
import wiggly.poker.equity.*
import wiggly.poker.equity.PrecomputedPokerRankLoader.Entry
import wiggly.poker.model.PokerRankCategory.{Flush, FourOfAKind, FullHouse, HighCard, Pair, Straight, StraightFlush, ThreeOfAKind, TwoPair}

import java.io.{FileInputStream, ObjectInputStream}
import math.BigDecimal.int2bigDecimal
import scala.annotation.tailrec


class PokerRankOrdEquityCalculator(cache: Array[PokerRankOrdEquityCalculator.CacheEntry]) extends EquityCalculator {

  assert(!cache.isEmpty)

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
    //    import PokerRank.orderPokerRank

    val bestA = PokerHand.fromIterable(a.cards ++ board).map(rankHand).max

    val bestB = PokerHand.fromIterable(b.cards ++ board).map(rankHand).max

    val result = Order.compare(bestA, bestB)

    if (result < 0) {
      (Equity(0, 1, 0), Equity(1, 0, 0))
    } else if (result > 0) {
      (Equity(1, 0, 0), Equity(0, 1, 0))
    } else {
      (Equity(0, 0, 1), Equity(0, 0, 1))
    }
  }

  private def rankHand(pokerHand: PokerHand): Int = {
    val key = pokerHand.toBits

    newLookupCache(key, 0, cache.length)
      .getOrElse[Int](throw new Exception(s"PokerRankOrdEquityCalculator cannot function without a complete lookup table - key: ${key.toBinaryString} - hand: ${pokerHand.show}"))
  }

  @tailrec
  private def newLookupCache(key: Long, left: Int, right: Int): Option[Int] = {
    //println(s"LOOKUP $start $finish")
    if (left > right) {
      //        println(s"CACHE MISS for key ${key.toBinaryString}")
      None
    } else {
      val mid = (left + right) / 2
      val entry = cache(mid)

      if (entry._1 == key) {
        //          println(s"CACHE HIT for key ${key.toBinaryString}")
        Some(entry._2)
      } else if (entry._1 > key) {
        newLookupCache(key, left, mid - 1)
      } else {
        newLookupCache(key, mid + 1, right)
      }
    }
  }

  @tailrec
  private def lookupCache(key: Long, start: Int, finish: Int): Option[Int] = {
    if (finish == 0) return None
    //println(s"LOOKUP $start $finish")

    if (finish >= start) {
      val mid = ((finish - start) / 2) + start
      val entry = cache(mid)

      if (entry._1 == key) {
        Some(entry._2)
      } else if (entry._1 > key) {
        lookupCache(key, start, mid - 1)
      } else {
        lookupCache(key, mid + 1, finish)
      }
    } else {
      println(s"CACHE MISS for key ${key.toBinaryString}")
      None
    }
  }
}

object PokerRankOrdEquityCalculator {

  type CacheEntry = (Long, Int)

  def load[F[_]](filename: String)(using F: Sync[F]): F[PokerRankOrdEquityCalculator] = {
    createObjectInputStream(filename)
      .use(input =>
        for {
          cache <- F.delay(input.readObject().asInstanceOf[Array[CacheEntry]])
          _ <- F.delay(println(s"Loaded ${cache.length} entries"))
          sortedCache = cache.sortBy(_._1)
          _ <- F.delay(println(s"Sorted cache"))
        } yield new PokerRankOrdEquityCalculator(sortedCache)
      )
  }

  private def createObjectInputStream[F[_]](
                                             name: String
                                           )(using F: Sync[F]): Resource[F, ObjectInputStream] = {
    Resource
      .make(F.delay(new FileInputStream(name)))(fileIn => F.delay(fileIn.close()))
      .evalMap(fileIn => F.delay(new ObjectInputStream(fileIn)))
  }
}