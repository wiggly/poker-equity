package wiggly.poker.equity

import cats.implicits.*
import wiggly.poker.equity.EquityCalculator.{Equity, EquityResult}
import wiggly.poker.model.{Card, HoleCards}

class DummyEquityCalculator extends EquityCalculator {
  override def calculate(
      a: HoleCards,
      b: HoleCards,
      board: Set[Card],
      dead: Set[Card]
  ): Either[String, EquityCalculator.EquityResult] = {
    EquityResult(0, 0, (a, Equity(0, 0, 0)), (b, Equity(0, 0, 0)))
      .asRight[String]
  }
}
