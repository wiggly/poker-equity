package wiggly.poker.equity

import cats.implicits.*
import cats.{Monoid, Show}
import wiggly.poker.equity.EquityCalculator.EquityResult
import wiggly.poker.model.{Card, HoleCards}

trait EquityCalculator {

  /** *
    * @param a
    *   Hand A
    * @param b
    *   Hand B
    * @param board
    *   current set of board cards already on table
    * @param dead
    *   dead cards not available to the board
    * @param coverage
    *   Percentage of board-space to cover. None == entire space. values will be
    *   clamped between 1.0 and 0.0
    */
  def calculate(
      a: HoleCards,
      b: HoleCards,
      board: Set[Card],
      dead: Set[Card],
      coverage: Option[Float]
  ): Either[String, EquityResult]
}

object EquityCalculator {

  // val defaultRunSize = 5000
  val defaultRunSize = 50000
  // val defaultRunSize = 150000
  // val defaultRunSize = 350000
  // val defaultRunSize = 1712304

  /** @param win
    *   number of wins for this hand
    * @param lose
    *   number of losses for this hand
    * @param tie
    *   number of ties for this hand
    */
  final case class Equity(win: Int, lose: Int, tie: Int)

  object Equity {
    given showEquity: Show[Equity] = new Show[Equity] {
      override def show(t: Equity): String = {
        val total = (t.win + t.lose + t.tie).toDouble
        val winPercent = (t.win.toDouble / total) * 100.0
        val losePercent = (t.lose.toDouble / total) * 100.0
        val tiePercent = (t.tie.toDouble / total) * 100.0
        s"win: ${winPercent}% (${t.win}) - lose: ${losePercent}% (${t.lose}) - tie: ${tiePercent}% (${t.tie})"
      }
    }

    given monoidEquity: Monoid[Equity] = new Monoid[Equity] {
      override def empty: Equity = Equity(0, 0, 0)
      override def combine(x: Equity, y: Equity): Equity =
        Equity(x.win + y.win, x.lose + y.lose, x.tie + y.tie)
    }
  }

  /** @param possibleBoards
    *   number of boards posssible given card distribution
    * @param boardsInspected
    *   number of boards compared
    */
  final case class EquityResult(
      possibleBoards: Int,
      boardsInspected: Int,
      a: (HoleCards, Equity),
      b: (HoleCards, Equity)
  )

  object EquityResult {
    given showEquityResult: Show[EquityResult] = new Show[EquityResult] {
      override def show(t: EquityResult): String = {
        val inspectedPercent: Double =
          (t.boardsInspected.toDouble / t.possibleBoards.toDouble) * 100.0
        val cardsa = t.a._1.cards.map(_.show).mkString
        val cardsb = t.b._1.cards.map(_.show).mkString

        s"inspected ${t.boardsInspected} / ${t.possibleBoards} - ${inspectedPercent}% - \n\tA: $cardsa - ${t.a._2.show}\n\tB: $cardsb - ${t.b._2.show}"
      }
    }
  }

  def boardCountForPercentage(maxCount: Int, percentage: Option[Float]): Int = {
    val factor = percentage
      .map(p => {
        Math.min(Math.max(0.0f, p), 1.0f)
      })
      .getOrElse(1.0f)

    (maxCount.toFloat * factor).toInt
  }
}
