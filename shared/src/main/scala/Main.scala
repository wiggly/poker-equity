import cats.Show
import cats.implicits.*
import cats.effect.{Async, Clock, IO, IOApp}
import wiggly.poker.{model, *}
import wiggly.poker.equity.*
import wiggly.poker.model.{Card, HoleCards, Rank, Suit}
import wiggly.poker.equity.EquityCalculator.EquityResult

import scala.concurrent.duration.Duration

object Main extends IOApp.Simple {

  val coverage: Option[Float] = None
//  val coverage: Option[Float] = Option(0.2f)

  given showDuration: Show[Duration] = new Show[Duration] {
    override def show(t: Duration): String = {
      if (t == Duration.Undefined) {
        "N/A"
      } else {
        val averageDurationMillis = t.toMillis.toDouble / 1000d
        f"$averageDurationMillis%.5f seconds"
      }
    }
  }

  // val equityCalculator: IO[EquityCalculator] = IO.delay(new SimpleEquityCalculator())
  // val equityCalculator: IO[EquityCalculator] = IO.delay(new ImprovedSimpleEquityCalculator())
  // val equityCalculator: IO[EquityCalculator] = PokerRankEquityCalculator.load[IO]("precomputed.dat")
  val equityCalculator: IO[EquityCalculator] =
    PokerRankOrdEquityCalculator.load[IO]("poker-rank-ord.dat")

  override def run: IO[Unit] = equityCalculator
    .flatMap(calculator =>
      timedRun[IO, Either[String, EquityResult]](IO(runCalc(calculator)))
        .flatMap(result => IO(println(result.fold(identity, _.show))))
    )

  def runCalc(calculator: EquityCalculator): Either[String, EquityResult] = {
    for {
      a <- HoleCards.create(
        Card(Suit.Spade, Rank.Ten),
        model.Card(Suit.Spade, Rank.Jack)
      )
      b <- HoleCards.create(
        model.Card(Suit.Heart, Rank.Ace),
        model.Card(Suit.Diamond, Rank.Ace)
      )
      result <- calculator.calculate(a, b, Set.empty, Set.empty, coverage)
    } yield result
  }

  def timedRun[F[_], T](
      action: F[T]
  )(using F: Async[F], clock: Clock[F]): F[T] = {
    for {
      start <- clock.realTime
      result <- action
      finish <- clock.realTime
      runtime: Duration = finish - start
      _ <- F.delay(println(s"RUNTIME ${runtime.show}"))
    } yield result
  }

}
