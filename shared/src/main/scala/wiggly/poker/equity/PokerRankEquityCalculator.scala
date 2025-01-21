package wiggly.poker.equity

import cats.effect.{Resource, Sync}
import cats.implicits.*
import cats.Order
import wiggly.poker.model.{Card, Deck, HoleCards, PokerHand, PokerRank}
import wiggly.poker.MathUtil
import wiggly.poker.equity.EquityCalculator.{Equity, EquityResult}
import wiggly.poker.equity.*

import java.io.{FileInputStream, ObjectInputStream}
import math.BigDecimal.int2bigDecimal

class PokerRankEquityCalculator(
    cache: Array[PokerRankEquityCalculator.CacheEntry]
) extends EquityCalculator {

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
    import PokerRank.orderPokerRank

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

  private def rankHand(pokerHand: PokerHand): PokerRank = {
    val key = pokerHand.toBits

    lookupCache(key, 0, cache.length)
      .getOrElse(PokerRank.rankHand(pokerHand))
  }

  private def lookupCache(
      key: Long,
      start: Int,
      finish: Int
  ): Option[PokerRank] = {
    if (finish == 0) return None
    //    println(s"LOOKUP $start $finish")

    if (finish >= start) {
      val mid = ((finish - start) / 2) + start
      val entry = cache(mid)

      if (entry._1 == key) {
//        println(s"CACHE HIT")
        Some(entry._2)
      } else if (entry._1 > key) {
        lookupCache(key, start, mid - 1)
      } else {
        lookupCache(key, mid + 1, finish)
      }
    } else {
      println("CACHE MISS")
      None
    }
  }
}

object PokerRankEquityCalculator {

  type CacheEntry = (Long, PokerRank)

  def apply(): PokerRankEquityCalculator = {
    new PokerRankEquityCalculator(Array.empty[CacheEntry])
  }

  def load[F[_]](
      filename: String
  )(using F: Sync[F]): F[PokerRankEquityCalculator] = {
    createObjectInputStream(filename)
      .use(input =>
        for {
          cache <- F.delay(input.readObject().asInstanceOf[Array[CacheEntry]])
          _ <- F.delay(println(s"Loaded ${cache.length} entries"))
          sortedCache = cache.sortBy(_._1)
          _ <- F.delay(println("Sorted cache"))
        } yield new PokerRankEquityCalculator(sortedCache)
      )
  }

  private def createObjectInputStream[F[_]](
      name: String
  )(using F: Sync[F]): Resource[F, ObjectInputStream] = {
    Resource
      .make(F.delay(new FileInputStream(name)))(fileIn =>
        F.delay(fileIn.close())
      )
      .evalMap(fileIn => F.delay(new ObjectInputStream(fileIn)))
  }
}
