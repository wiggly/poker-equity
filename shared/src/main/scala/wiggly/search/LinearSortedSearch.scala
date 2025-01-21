package wiggly.search

import cats.Order
import scala.util.boundary
import scala.util.boundary.break

class LinearSortedSearch extends SortedSearch {
  override def containsBy[A, B](
      container: IndexedSeq[A],
      convert: A => B,
      target: B
  )(using Order[B]): Boolean =
    findBy[A, B](container, convert, target).isDefined

  override def findBy[A, B](
      container: IndexedSeq[A],
      convert: A => B,
      target: B
  )(using Order[B]): Option[A] = {
    var found: Option[A] = None

    boundary {
      container.foreach(a =>
        if (Order.eqv(convert(a), target)) {
          found = Option(a)
          break()
        }
      )
    }

    found
  }
}
