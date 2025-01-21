package wiggly.poker.equity

import cats.kernel.Order
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import wiggly.poker.model.PokerHand
import wiggly.poker.model.PokerRank
import wiggly.poker.model.Deck

class PokerRankVsSimpleParitySpec
    extends AnyFlatSpec
    with ScalaCheckPropertyChecks
    with Matchers {

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(
    minSuccessful = 100
  )

  val pokerRankOrder: Order[PokerHand] = PokerRank.orderPokerHand
  val simpleOrder: Order[PokerHand] = SimpleEquityCalculator.orderPokerHand

  implicit val pokerHandGen: Gen[PokerHand] = Gen
    .pick(5, Deck.create.toList)
    .map(cs => PokerHand.fromIterable(cs).next)

  implicit val pokerHandArb: Arbitrary[PokerHand] = Arbitrary(pokerHandGen)

  it should "produce the same results for all inputs" in {
    forAll { (a: PokerHand, b: PokerHand) =>
      assert(
        simpleOrder.compare(a, b).sign == pokerRankOrder.compare(a, b).sign
      )
    }
  }

}
