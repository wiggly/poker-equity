package wiggly.poker.equity

import cats.kernel.Order
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import wiggly.poker.model.PokerHand

class SimplePokerHandOrderSpec
    extends AnyFlatSpec
    with TableDrivenPropertyChecks
    with Matchers
    with PokerHandOrderExamples {

  val order: Order[PokerHand] = SimpleEquityCalculator.orderPokerHand

  it should "correctly identify greater than examples" in {
    forAll(greaterThanExamples) { (a: String, b: String) =>
      testOrder(order, (a, b), _ > 0)
    }
  }

  it should "correctly identify inverted greater than examples" in {
    forAll(greaterThanExamples) { (a: String, b: String) =>
      testOrder(order, (b, a), _ < 0)
    }
  }

  it should "correctly identify equal examples" in {
    forAll(equalExamples) { (a: String, b: String) =>
      testOrder(order, (a, b), _ == 0)
    }
  }

  it should "correctly identify inverted equal examples" in {
    forAll(equalExamples) { (a: String, b: String) =>
      testOrder(order, (b, a), _ == 0)
    }
  }

}
