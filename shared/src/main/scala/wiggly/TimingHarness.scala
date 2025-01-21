package wiggly

import cats.implicits.*
import cats.Parallel
import cats.effect.{Async, Clock}

import scala.concurrent.duration.Duration

object TimingHarness {
  final case class SingleResult[T](duration: Duration, output: Option[T])

  final case class Result[T](
      averageDuration: Duration,
      runs: List[SingleResult[T]]
  )

  def timeAction[F[_]: Parallel, T](
      action: F[T],
      runCount: Int,
      cutoff: Duration
  )(using F: Async[F]): F[Result[T]] = {
    val warmUpRuns = 2

    val warmUp = List.fill(warmUpRuns)(singleRun(action, cutoff)).sequence.void

    val hotRuns: F[List[SingleResult[T]]] =
      List.fill(runCount)(singleRun(action, cutoff)).sequence

    val results: F[List[SingleResult[T]]] =
      F.delay(println("warmup")) >> warmUp >> F.delay(
        println("hot runs")
      ) >> hotRuns

    results.map(runs =>
      if (runs.exists(_.output.isEmpty)) {
        Result(Duration.Undefined, runs)
      } else {
        val average = runs.map(_.duration).reduce(_ + _) / runs.size
        Result(average, runs)
      }
    )
  }

  def singleRun[F[_], T](action: F[T], cutoff: Duration)(using
      F: Async[F],
      clock: Clock[F]
  ): F[SingleResult[T]] = {
    for {
      start <- clock.realTime
      result <- timeoutAction[F, T](action, cutoff)
      finish <- clock.realTime
      runtime = result.map(_ => finish - start).getOrElse(Duration.Undefined)
    } yield SingleResult(runtime, result)
  }

  def timeoutAction[F[_], T](action: F[T], cutoff: Duration)(using
      F: Async[F]
  ): F[Option[T]] =
    F.race(F.sleep(cutoff), action).map(_.toOption)
}
