package wiggly.poker

import cats.implicits.*
import cats.effect.*
import cats.Order
import wiggly.poker.model.*

import java.io.{FileOutputStream, *}

object PrecomputePokerRankRank extends IOApp {

  type RankEntry = (Long, PokerRank)
  type OrdEntry = (Long, Int)

  override def run(args: List[String]): IO[ExitCode] =
    serializePokerRanks().as(ExitCode.Success)

  private def serializePokerRanks(): IO[Unit] = {

    createObjectOutputStream("poker-rank-ord.dat")
      .use(out =>
        IO {
          val sortedRankEntries: Array[RankEntry] = PokerHand
            .fromIterable(Deck.create.toList)
//            .take(5000)
            .map(hand => {
              val result = (hand.toBits, PokerRank.rankHand(hand))
              //println(s"${hand.show} => ${result._1.toBinaryString}")
              result
            })
            .toArray
            .sortBy(_._2)

          val sortedOrdEntries: Array[OrdEntry] = sortedRankEntries
            .sliding(2)
            .foldLeft(List.empty[OrdEntry])( (acc, entry) => {
              if(acc.isEmpty) {
                if(Order.eqv(entry(0)._2, entry(1)._2)) {
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
                val current = if(Order.eqv(entry(0)._2, entry(1)._2)) {
                  acc.head._2
                } else {
                  acc.head._2 + 1
                }
                (entry(1)._1, current) :: acc
              }
            })
            .toArray


          sortedRankEntries
            .foreach( (key, rank) => System.err.println(s"${key.toBinaryString} - $rank"))


          val keySortedOrdEntries = sortedOrdEntries
            .sortBy(_._1)

          out.writeObject(keySortedOrdEntries)

          println(s"wrote entries to file")
//          sortedOrdEntries.foreach( (bits, score) => System.err.println(s"${bits.toBinaryString}"))
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
