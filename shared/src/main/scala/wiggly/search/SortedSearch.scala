package wiggly.search

import cats.Order

trait SortedSearch {

  /** Determine if a container sorted by the value A contains a particular value
    */
  def contains[A](container: IndexedSeq[A], target: A)(using
      Order[A]
  ): Boolean = containsBy[A, A](container, identity, target)

  /** Determine if a container sorted by the value B contains a particular value
    */
  def containsBy[A, B](container: IndexedSeq[A], convert: A => B, target: B)(
      using Order[B]
  ): Boolean

  def find[A](container: IndexedSeq[A], target: A)(using Order[A]): Option[A] =
    findBy[A, A](container, identity, target)

  def findBy[A, B](container: IndexedSeq[A], convert: A => B, target: B)(using
      Order[B]
  ): Option[A]
}
