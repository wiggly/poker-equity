package wiggly.poker.equity

import cats.effect.{Resource, Sync}
import cats.implicits.*
import cats.Order
import wiggly.poker.model.{Card, Deck, HoleCards, PokerHand}
import wiggly.poker.MathUtil
import wiggly.poker.equity.EquityCalculator.{
  Equity,
  EquityResult,
  defaultRunSize
}
import wiggly.poker.equity.*

import java.io.{
  DataInputStream,
  FileInputStream,
  IOException,
  ObjectInputStream
}
import math.BigDecimal.int2bigDecimal
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.boundary
import scala.util.boundary.break

class PokerRankOrdEquityCalculator(
    cache: Array[PokerRankOrdEquityCalculator.CacheEntry]
) extends EquityCalculator {

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
    val count = EquityCalculator.defaultRunSize

    println(s"stub length: ${stub.toList.size}")

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

    lookupScore(key, 0, cache.length)
      .getOrElse[Int](
        throw new Exception(
          s"PokerRankOrdEquityCalculator cannot function without a complete lookup table - key: ${key.toBinaryString} - hand: ${pokerHand.show}"
        )
      )
  }

  /** Binary search over poker hand to rank cache
    */
  @tailrec
  private def lookupScore(key: Long, left: Int, right: Int): Option[Int] = {
    if (left > right) {
      None
    } else {
      val mid = (left + right) / 2
      val entry = cache(mid)

      if (entry._1 == key) {
        Some(entry._2)
      } else if (entry._1 > key) {
        lookupScore(key, left, mid - 1)
      } else {
        lookupScore(key, mid + 1, right)
      }
    }
  }
}

object PokerRankOrdEquityCalculator {

  type CacheEntry = (Long, Int)

  def load[F[_]](filename: String)(using F: Sync[F]): F[EquityCalculator] = {
    createDataInputStream(filename)
      .use(input =>
        for {
          cache <- loadCache[F](input)
          _ <- F.delay(println(s"Loaded ${cache.length} entries"))
        } yield new PokerRankOrdEquityCalculator(cache)
      )
  }

  private def loadCache[F[_]](
      in: DataInputStream
  )(using F: Sync[F]): F[Array[CacheEntry]] = F.delay {
    println(s"load cache")
    val size: Int = in.readInt()
    val data = Array.ofDim[CacheEntry](size)

    println(s"loading $size items")

    try {
      var i = 0
      var bits: Long = 0
      var score: Int = 0

      while (i < size) {
        bits = in.readLong()
        score = in.readInt()
        data.update(i, (bits, score))
        i = i + 1
      }
    } catch {
      case _: IOException => {
        println(s"exception whilst reading in cache data, found end of file?")
      }
    } finally {
      in.close()
    }

    data
  }

  private def createDataInputStream[F[_]](
      name: String
  )(using F: Sync[F]): Resource[F, DataInputStream] = {
    Resource
      .make(F.delay(new FileInputStream(name)))(fileIn =>
        F.delay(fileIn.close())
      )
      .evalMap(fileIn => F.delay(new DataInputStream(fileIn)))
  }

}
