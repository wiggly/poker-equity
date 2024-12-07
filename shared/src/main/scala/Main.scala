import cats.implicits.*
import wiggly.poker.{model, *}
import wiggly.poker.equity.*
import wiggly.poker.model.{Card, HoleCards, Rank, Suit}

object Main extends App {
  //val calc = new DummyEquityCalculator()
  val calc = new SimpleEquityCalculator()

  val result = for {
    a <- HoleCards.create(
      Card(Suit.Spade, Rank.Ten),
      model.Card(Suit.Spade, Rank.Jack)
    )
    b <- HoleCards.create(
      model.Card(Suit.Heart, Rank.Ace),
      model.Card(Suit.Diamond, Rank.Ace)
    )
    result <- calc.calculate(a, b, Set.empty, Set.empty)
  } yield result

  println(result.fold(identity, _.show))
}
