package wiggly.poker

import cats.effect.*
import wiggly.poker.model.*

import java.io.{FileOutputStream, *}

object PrecomputePokerRank extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    serializePokerRanks().as(ExitCode.Success)

  private def serializePokerRanks(): IO[Unit] = {

    createObjectOutputStream("precomputed.dat")
      .use(out =>
        IO {
          val array = PokerHand
            .fromIterable(Deck.create.toList)
            // .take(250000)
            .map(hand => {
              val result = (hand.toBits, PokerRank.rankHand(hand))
              // println(s"${hand.show} => ${result._1.toBinaryString}")
              result
            })
            .toArray
          out.writeObject(array)
        }
      )
  }

  private def createObjectOutputStream(
      name: String
  ): Resource[IO, ObjectOutputStream] = {
    Resource
      .make(IO(new FileOutputStream(name)))(fileOut => IO(fileOut.close()))
      .evalMap(fileOut => IO(new ObjectOutputStream(fileOut)))
  }
}
