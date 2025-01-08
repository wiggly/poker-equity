package wiggly.poker.equity

import cats.implicits.*
import cats.effect.*
import wiggly.poker.model.*

import java.io.{FileOutputStream, *}

import scala.collection.mutable.{Seq => MSeq}

object PrecomputedPokerRankLoader {

  type Entry = (Long, PokerRank)

  def load(): IO[Array[Entry]] = {
    createObjectInputStream("precomputed.dat")
      .use(input =>
        IO(input.readObject().asInstanceOf[Array[Entry]])
      )

  }

  private def createObjectInputStream(
                                        name: String
                                      ): Resource[IO, ObjectInputStream] = {
    Resource
      .make(IO(new FileInputStream(name)))(fileIn => IO(fileIn.close()))
      .evalMap(fileIn => IO(new ObjectInputStream(fileIn)))
  }
}
