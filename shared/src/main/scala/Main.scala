import cats.implicits.*
import cats.effect.{IO, IOApp}
import wiggly.poker.{model, *}
import wiggly.poker.equity.*
import wiggly.poker.model.{Card, HoleCards, Rank, Suit}
import cats.effect.unsafe.implicits.global

object Main extends IOApp.Simple {
  val equityCalculator: IO[EquityCalculator] = IO.delay(new SimpleEquityCalculator())
  // val equityCalculator: IO[EquityCalculator] = IO.delay(new ImprovedSimpleEquityCalculator())
  // val equityCalculator: IO[EquityCalculator] = PokerRankEquityCalculator.load[IO]("precomputed.dat")
  //val equityCalculator: IO[EquityCalculator] = PokerRankOrdEquityCalculator.load[IO]("poker-rank-ord.dat")

  override def run: IO[Unit] = equityCalculator
    .map(calculator => {
      val result = for {
        a <- HoleCards.create(
          Card(Suit.Spade, Rank.Ten),
          model.Card(Suit.Spade, Rank.Jack)
        )
        b <- HoleCards.create(
          model.Card(Suit.Heart, Rank.Ace),
          model.Card(Suit.Diamond, Rank.Ace)
        )
        result <- calculator.calculate(a, b, Set.empty, Set.empty)
      } yield result

      println(result.fold(identity, _.show))
    })
}
