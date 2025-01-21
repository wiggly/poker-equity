package wiggly.poker

import cats.implicits.*
import cats.effect.*
import cats.Order
import wiggly.poker.model.*

import java.io.{FileOutputStream, *}

/** NOTE: This was using ObjectOutputStream which deals with the serialization
  * but those classes are not available in Scala native so I had to write it
  * myself.
  *
  * It does generate a file that is less than half the size.
  *
  * TODO: readability
  */
object PrecomputePokerRankRank extends IOApp {

  type RankEntry = (Long, PokerRank)
  type OrdEntry = (Long, Int)

  override def run(args: List[String]): IO[ExitCode] =
    serializePokerRanks().as(ExitCode.Success)

  private def serializePokerRanks(): IO[Unit] = {
    createDataOutputStream[IO]("poker-rank-ord.dat")
      .use(out =>
        for {
          cache <- IO(createCache())
          _ <- writeCache[IO](out, cache)
          _ <- IO(println("wrote entries to file"))
        } yield ()
      )
  }

  private def writeCache[F[_]](
      out: DataOutputStream,
      cache: List[OrdEntry]
  )(using F: Sync[F]): F[Unit] = for {
    _ <- F.delay(out.writeInt(cache.length))
    _ <- cache
      .traverse(entry =>
        F.delay(
          {
            out.writeLong(entry._1)
            out.writeInt(entry._2)
          }
        )
      )
  } yield ()

  private def createDataOutputStream[F[_]](
      name: String
  )(using F: Sync[F]): Resource[F, DataOutputStream] = {
    Resource
      .make(F.delay(new FileOutputStream(name)))(fileOut =>
        F.delay(fileOut.close())
      )
      .evalMap(fileOut => F.delay(new DataOutputStream(fileOut)))
  }

  def createCache(): List[OrdEntry] = {
    val sortedRankEntries: Array[(Long, PokerRank)] = PokerHand
      .fromIterable(Deck.create.toList)
      .map(hand => (hand.toBits, PokerRank.rankHand(hand)))
      .toArray
      .sortBy(_._2)

    val sortedOrdEntries: List[OrdEntry] = sortedRankEntries
      .sliding(2)
      .foldLeft(List.empty[OrdEntry])((acc, entry) => {
        if (acc.isEmpty) {
          if (Order.eqv(entry(0)._2, entry(1)._2)) {
            List(
              (entry(1)._1, 0),
              (entry(0)._1, 0)
            )
          } else {
            List(
              (entry(1)._1, 1),
              (entry(0)._1, 0)
            )
          }
        } else {
          val current = if (Order.eqv(entry(0)._2, entry(1)._2)) {
            acc.head._2
          } else {
            acc.head._2 + 1
          }
          (entry(1)._1, current) :: acc
        }
      })

    sortedOrdEntries.sortBy(_._1)
  }
}
