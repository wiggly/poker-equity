package wiggly.search

import cats.implicits.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import wiggly.search.*

class LinearSortedSearchSpec extends AnyFlatSpec
  with ScalaCheckPropertyChecks
  with Matchers {

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(
    minSuccessful = 10
  )

  type Entry = Short
  type Container = IndexedSeq[Entry]

  given containerGen: Gen[Container] =
    Gen
      .listOfN[Entry](1000, Gen.choose[Entry](Short.MinValue, Short.MaxValue))
      .map(_.toIndexedSeq)

  given containerArb: Arbitrary[Container] = Arbitrary(containerGen)

  it should "determine containement" in {
    forAll { (container: Container, target: Entry) =>
      val sorted = setupContainer(container, target)
      assert(subject.contains(sorted, target))
    }
  }

  private def subject: SortedSearch = new LinearSortedSearch()

  private def setupContainer(container: Container, target: Entry): Container = {
    (container.filterNot(_ == target) :+ target).sorted
  }
}
