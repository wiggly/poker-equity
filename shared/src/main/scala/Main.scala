import cats.implicits.*
import wiggly.poker.*
import wiggly.poker.Decks.Deck

object Main {

  def main(args: Array[String]): Unit = {
    Deck.create.toList
      .foreach(c => println(c.show))
  }
}
