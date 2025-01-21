package wiggly.search

import cats.implicits.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import wiggly.search.*

class BinarySortedSearchSpec
    extends AnyFlatSpec
    with ScalaCheckPropertyChecks
    with Matchers {

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(
    minSuccessful = 10
  )

//  type X = Vector

  type Entry = Long
  type Container = IndexedSeq[Entry]

  given containerGen: Gen[Container] =
    Gen
      .listOfN[Entry](10000, Gen.choose[Entry](0L, Long.MaxValue))
      .map(_.toIndexedSeq)

  given containerArb: Arbitrary[Container] = Arbitrary(containerGen)

  it should "determine containement when present" in {
    forAll { (container: Container, target: Entry) =>
      // println(s"container size: ${container.size} : ${container.take(3).mkString(", ")}")
      val distinctContainer = setupContainer(container, target)
      val sortedContainer = distinctContainer.sorted
      // println(s"sortedContainer size: ${sortedContainer.size} : ${sortedContainer.take(3).mkString(", ")}")
      assert(subject.contains(sortedContainer, target))
    }
  }

  private def subject: SortedSearch = new BinarySortedSearch()

  private def setupContainer(container: Container, target: Entry): Container = {
    (container :+ target).distinct
  }
}
