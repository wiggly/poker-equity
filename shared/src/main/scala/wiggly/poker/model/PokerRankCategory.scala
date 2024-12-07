package wiggly.poker.model

import cats.Order

/** The ranks that a PokerHand may have
  */
enum PokerRankCategory(val value: Int) {
  case StraightFlush extends PokerRankCategory(8)
  case FourOfAKind extends PokerRankCategory(7)
  case FullHouse extends PokerRankCategory(6)
  case Flush extends PokerRankCategory(5)
  case Straight extends PokerRankCategory(4)
  case ThreeOfAKind extends PokerRankCategory(3)
  case TwoPair extends PokerRankCategory(2)
  case Pair extends PokerRankCategory(1)
  case HighCard extends PokerRankCategory(0)
}

object PokerRankCategory {
  given orderPokerRankCategory: Order[PokerRankCategory] = Order.by(_.value)

  given orderingHankRank: Ordering[PokerRankCategory] =
    orderPokerRankCategory.toOrdering
}
