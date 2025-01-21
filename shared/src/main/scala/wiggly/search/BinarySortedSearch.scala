package wiggly.search

import cats.Order

import scala.annotation.tailrec

class BinarySortedSearch extends SortedSearch {
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
  )(using Order[B]): Option[A] =
    newLookupCache[A, B](container, target, convert, 0, container.length)

  @tailrec
  private def newLookupCache[A, B](
      cache: IndexedSeq[A],
      key: B,
      convert: A => B,
      left: Int,
      right: Int
  )(using Order[B]): Option[A] = {

    if (left > right) {
      None
    } else {
      val mid = (left + right) / 2
      val entry = cache(mid)

      if (Order.eqv(convert(entry), key)) {
        Some(entry)
      } else if (Order.gt(convert(entry), key)) {
        newLookupCache(cache, key, convert, left, mid - 1)
      } else {
        newLookupCache(cache, key, convert, mid + 1, right)
      }
    }

  }

}
