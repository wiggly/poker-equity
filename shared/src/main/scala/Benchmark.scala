import cats.implicits.*
import cats.{Parallel, Show}
import cats.effect.*
import wiggly.TimingHarness
import wiggly.poker.{model, *}
import wiggly.poker.equity.*
import wiggly.poker.equity.EquityCalculator.EquityResult
import wiggly.poker.model.{Card, HoleCards, Rank, Suit}

import scala.concurrent.duration.*
import scala.language.postfixOps

object Benchmark extends IOApp {

  val runs: Int = 5
  val timeout: Duration = 1 minute

  type Output = Either[String, EquityResult]

  def runPure(calculator: EquityCalculator, coverage: Float): Output =
    for {
      a <- HoleCards.create(
        Card(Suit.Spade, Rank.Ten),
        model.Card(Suit.Spade, Rank.Jack)
      )
      b <- HoleCards.create(
        model.Card(Suit.Heart, Rank.Ace),
        model.Card(Suit.Diamond, Rank.Ace)
      )
      result <- calculator.calculate(
        a,
        b,
        Set.empty,
        Set.empty,
        Option(coverage)
      )
    } yield result

  given showDuration: Show[Duration] = new Show[Duration] {
    override def show(t: Duration): String = {
      if (t == Duration.Undefined) {
        "N/A"
      } else {
        val averageDurationMillis = t.toMillis.toDouble / 1000d
        f"$averageDurationMillis%.5f"
      }
    }
  }

  def benchmarkImplementations[F[_]: Parallel](
      implementations: List[(String, EquityCalculator)],
      coverageRange: List[Float]
  )(using
      F: Async[F]
  ): F[Map[(String, Float), TimingHarness.Result[Output]]] = {
    val matrix = for {
      implementation <- implementations
      coverage <- coverageRange
    } yield (implementation._1, implementation._2, coverage)

    matrix
      .traverse(input =>
        F.delay(println(s"${input._1} ${input._3}")) >>
          TimingHarness
            .timeAction(F.delay(runPure(input._2, input._3)), runs, timeout)
            .map(result => (input._1, input._3) -> result)
      )
      .map(_.toMap)
  }

  def displayResults[F[_]](
      results: Map[(String, Float), TimingHarness.Result[Output]]
  )(using F: Async[F]): F[Unit] = {
    val names = results.keys.map(_._1).toList.distinct.sorted
    val coverageRange = results.keys.map(_._2).toList.distinct.sorted

    val headings = List(("coverage" +: names).mkString(","))

    val xxx = coverageRange
      .foldLeft[List[String]](headings)((cacc, coverage) => {

        val yyy = names
          .foldLeft[List[String]](List(coverage.toString))((nacc, name) =>
            results
              .get((name, coverage))
              .map(_.averageDuration.show)
              .getOrElse("Err") :: nacc
          )
          .reverse
          .mkString(", ")

        yyy :: cacc
      })
      .reverse
      .mkString("\n")

    F.delay(println(xxx))
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- IO.println("BENCHMARK")
//      _ <- IO.println("creating PokerRankEquityCalculator")
//      precomputedPokerRank <- PokerRankEquityCalculator
//        .load[IO]("precomputed.dat")

      _ <- IO.println("creating PokerRankOrdEquityCalculator")
      precomputedPokerRankOrd <- PokerRankOrdEquityCalculator
        .load[IO]("poker-rank-ord.dat")

      implementations = List(
//        ("simple", new SimpleEquityCalculator()),
//        ("improved", new ImprovedSimpleEquityCalculator()),
        ("quick", new QuickEquityCalculator()),
        ("poker-rank", PokerRankEquityCalculator()),
//        ("precomputed-poker-rank", precomputedPokerRank),
        ("precomputed-poker-rank-ord", precomputedPokerRankOrd)
      )

//      coverageRange = List(0.001f, 0.005f, 0.01f, 0.03f)
//      coverageRange = List(0.01f, 0.02f, 0.03f, 0.04f, 0.05f, 0.06f, 0.07f, 0.08f, 0.09f, 0.1f, 0.11f)
      coverageRange = List(0.01f, 0.03f, 0.05f, 0.07f, 0.09f, 0.11f, 0.13f,
        0.15f, 0.17f, 0.19f)
      results <- benchmarkImplementations[IO](implementations, coverageRange)

      _ <- displayResults[IO](results)

    } yield ExitCode.Success

  }
}
