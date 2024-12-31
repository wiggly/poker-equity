import cats.implicits.*
import cats.Show
import cats.effect.*
import wiggly.poker.{model, *}
import wiggly.poker.equity.*
import wiggly.poker.equity.EquityCalculator.EquityResult
import wiggly.poker.model.{Card, HoleCards, Rank, Suit}

import scala.concurrent.duration.FiniteDuration

object Benchmark extends IOApp {

  val runs = 2

  case class BenchmarkResults(results: Seq[BenchmarkResult])

  case class BenchmarkResult(
      runtime: FiniteDuration,
      result: Either[String, EquityResult]
  )

  given showBenchmarkResults: Show[BenchmarkResults] =
    new Show[BenchmarkResults] {
      override def show(t: BenchmarkResults): String = {
        val results = t.results//.drop(2)
        val totalDuration = results.map(_.runtime).reduce(_ + _)
        val averageDuration = (totalDuration / results.size.toDouble).toSeconds
        val sample = results.last.result.fold(identity, _.show)
        s"""runs: ${results.size}
         |average duration: $averageDuration seconds
         |sample output:
         |$sample
         |""".stripMargin
      }
    }

  def benchmarkImplementation(
      calculator: EquityCalculator
  ): IO[BenchmarkResults] =
    List
      .fill(runs)(runImplementation(calculator))
      .sequence
      .map(BenchmarkResults.apply)

  def runImplementation(calculator: EquityCalculator): IO[BenchmarkResult] = {
    for {
      start <- IO.realTime
      result <- IO(runPure(calculator))
      finish <- IO.realTime
      runtime = finish - start
    } yield BenchmarkResult(runtime, result)
  }

  def runPure(calculator: EquityCalculator): Either[String, EquityResult] =
    for {
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

  // val calc = new DummyEquityCalculator()
  val calc = new SimpleEquityCalculator()
  // val calc = new ImprovedSimpleEquityCalculator()

  override def run(args: List[String]): IO[ExitCode] = {
    val subjects = List(
//      ("simple", new SimpleEquityCalculator()),
      ("quick", new QuickEquityCalculator()),
//      ("improved", new ImprovedSimpleEquityCalculator()),
    )

    for {
      results <- subjects.traverse((name, calc) =>
        IO.println(s"benchmarking: $name") >> benchmarkImplementation(calc).map(
          result => (name, result)
        )
      )
      _ <- results.traverse((name, result) =>
        IO.println(s"$name results:\n${result.show}")
      )
    } yield ExitCode.Success
  }
}
