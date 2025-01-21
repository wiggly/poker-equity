package wiggly.poker.equity

import cats.kernel.Order
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import wiggly.poker.model.PokerHand
import wiggly.poker.model.Deck

class QuickVsImprovedSimpleParitySpec
    extends AnyFlatSpec
    with ScalaCheckPropertyChecks
    with Matchers {

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(
    minSuccessful = 100
  )

  val quickOrder: Order[PokerHand] = QuickEquityCalculator.orderPokerHand
  val improvedOrder: Order[PokerHand] =
    ImprovedSimpleEquityCalculator.orderPokerHand

  implicit val pokerHandGen: Gen[PokerHand] = Gen
    .pick(5, Deck.create.toList)
    .map(cs => PokerHand.fromIterable(cs).next)

  implicit val pokerHandArb: Arbitrary[PokerHand] = Arbitrary(pokerHandGen)

  it should "produce the same results for all inputs" in {
    forAll { (a: PokerHand, b: PokerHand) =>
      assert(quickOrder.compare(a, b).sign == improvedOrder.compare(a, b).sign)
    }
  }

}
