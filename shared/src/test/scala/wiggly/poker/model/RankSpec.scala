package wiggly.poker.model

import cats.kernel.Order
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class RankSpec
    extends AnyFlatSpec
    with TableDrivenPropertyChecks
    with Matchers {

  val greaterThanExamples = TableFor2(
    ("a", "b"),
    (Rank.Ace, Rank.King),
    (Rank.King, Rank.Queen),
    (Rank.Queen, Rank.Jack),
    (Rank.Jack, Rank.Ten),
    (Rank.Ten, Rank.Nine),
    (Rank.Eight, Rank.Seven),
    (Rank.Seven, Rank.Six),
    (Rank.Six, Rank.Five),
    (Rank.Five, Rank.Four),
    (Rank.Four, Rank.Three),
    (Rank.Three, Rank.Two)
  )

  it should "correctly identify greater than examples" in {
    forAll(greaterThanExamples) { (a: Rank, b: Rank) =>
      assertResult(1) {
        Order.compare(a, b).sign
      }
    }
  }

  it should "correctly identify less than examples" in {
    forAll(greaterThanExamples) { (b: Rank, a: Rank) =>
      assertResult(-1) {
        Order.compare(a, b).sign
      }
    }
  }
}
