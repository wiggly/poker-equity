package wiggly.poker.model

import cats.implicits.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1}
import wiggly.poker.model.Card.*

import scala.util.Random

class CardSpec
    extends AnyFlatSpec
    with TableDrivenPropertyChecks
    with Matchers {

  val cards = List(
    Card.c2h,
    Card.c2d,
    Card.c3s,
    Card.c4s,
    Card.c4h,
    Card.c4d,
    Card.cJs,
    Card.cJh,
    Card.cJd,
    Card.cAs,
    Card.cAh,
    Card.cAd
  )

  val input = Random.shuffle(cards)

  val randomisedExamples: TableFor1[List[Card]] = Table(
    "input",
    input.permutations.take(100).toList: _*
  )

  it should "correctly sort cards" in {
    forAll(randomisedExamples) { (unsorted: List[Card]) =>
      assert(unsorted.sorted == cards)
    }
  }
}
