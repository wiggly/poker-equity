package wiggly.poker.equity

import cats.implicits.*
import wiggly.poker.model.PokerHand
import wiggly.poker.model.Card
import wiggly.poker.model.Card.*
import org.scalatest.prop.TableDrivenPropertyChecks.*
import cats.Order
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers._

trait PokerHandOrderExamples {

  val greaterThanExamples: TableFor2[String, String] = Table(
    ("a", "b"),

    // Parity failures
    ("7sAh5sAc4h", "7dAcTh4dJd"),
    ("5hAs6h5d9h", "2s8c5d7s4d"),
    ("Jd7hJh7cQd", "AhQdQhTd8c"),
    ("7sKd8c5h4d", "7s9h8hQd4d"),
    ("6cAc5c7d7c", "Jc6c8d7d7h"),

    // Straight Flush vs Straight Flush
    ("AsKsQsJsTs", "KsQsJsTs9s"),
    ("AsKsQsJsTs", "As2s3s4s5s"),
    ("KsQsJsTs9s", "As2s3s4s5s"),
    ("Td9d8d7d6d", "9d8d7d6d5d"),
    ("2s3s4s5s6s", "As2s3s4s5s"),

    // Quads vs Quads
    ("AsAhAdAc3s", "AsAhAdAc2s"), // kicker
    ("AsAhAdAcKs", "AsAhAdAc2s"), // kicker
    ("AsAhAdAcQs", "KsKhKdKcQs"), // quads better, same kicker
    ("2s2h2d2cKs", "2s2h2d2cQs"), // kicker
    ("3s3h3d3cQs", "2s2h2d2cQs"), // quads better, same kicker

    // Full House vs Full House
    ("AsAh3d3c3s", "AsAh2d2c2s"), // trips better
    ("AsAh3d3c3s", "3s3h2d2c2s"), // trips better
    ("AsAh3d3c3s", "TsTh2d2c2s"), // trips better
    ("AsAh3d3c3s", "KsKh3d3c3s"), // trips same, pair better
    ("AsAh3d3c3s", "2s2h3d3c3s"), // trips same, pair better

    // Flush vs Flush
    ("AsKsQs8s7s", "AsKsQs7s6s"),
    ("KsQs9s8s7s", "KsQs8s7s6s"),
    ("AsKs8s4s3s", "AsQs8s4s3s"),

    // Straight vs Straight
    ("AsKcQsJhTs", "KsQcJsTh9s"),
    ("AsKcQsJhTs", "As2c3s4h5s"),
    ("KsQcJsTh9s", "As2c3s4h5s"),
    ("Td9c8d7h6d", "9d8c7d6h5d"),
    ("2s3c4s5h6s", "As2c3s4h5s"),

    // Three of a Kind vs Three of a Kind
    ("KsKhKdAs9d", "QsQhQdAs9d"), // trips better
    ("KsKhKdAs9d", "KsKhKd8d3c"), // kicker
    ("KsKhKdAs9d", "KsKhKdAs8d"), // second kicker

    // TwoPair vs TwoPair
    ("AsAh8s8h3d", "KcKs8s8h3d"), // first pair better
    ("KsKh8s8h3d", "KcKs3s3h8c"), // second pair better
    ("KsKh8s8h3d", "KcKs3s3h2c"), // kicker

    // Pair vs Pair
    ("KsKhAs9d3s", "8s8dAs9d3s"), // pair better
    ("KsKhAs9d3s", "KsKdQs9d3s"), // first kicker
    ("KsKhAs9d3s", "KsKdAs8d3s"), // second kicker
    ("KsKhAs9d3s", "KsKdAs9d2s"), // third kicker

    // High Card
    ("Qs3hTd9c2s", "Ts8hQd3c2s"),
    ("Qs4hTd9c2s", "Ts8hQd3c2s"),
    ("Qs3hTd9cAs", "Ts8hQd3c2s"),
    ("Ks3hTd9c2s", "Ts8hQd3c2s")

  
  )


  val generatedEqualExamples: List[(String, String)] =
    greaterThanExamples.toList.flatMap(pair =>
      List((pair._1, pair._1), (pair._2, pair._2))
    )

  val curatedEqualExamples: List[(String, String)] = List(
    ("AsKsQsJsTs", "AcKcQcJcTc"),
    ("JsAsQsKsTs", "QcJcTcAcKc"),
    ("9dThKc2s3d", "2h9sTcKd3s"),
    ("KsQs8s4s3s", "KdQd8d4d3d"),
    ("AsAd8s5sAc", "AhAc8c5dAd")
  )

  val equalExamples: TableFor2[String, String] =
    Table(
      ("a", "b"),
      (generatedEqualExamples ++ curatedEqualExamples): _*
    )

  def testOrder(
      order: Order[PokerHand],
      example: (String, String),
      predicate: Int => Boolean
  ): Assertion = {
    val input = parseExample(example)
    val oc = order.compare(input._1, input._2)
    predicate(order.compare(input._1, input._2)) shouldEqual (true)
  }

  private def parseExample(hands: (String, String)): (PokerHand, PokerHand) = {
    val parsedA = parseHand(hands._1)
    val parsedB = parseHand(hands._2)
    (parsedA, parsedB)
  }

  private def parseHand(hand: String): PokerHand = {
    hand
      .grouped(2)
      .toList
      .traverse(Card.parse)
      .flatMap(cards =>
        val cs = cards.toSet
        Either.cond[String, PokerHand](
          cs.size == 5,
          PokerHand.fromIterable(cs).next(),
          "Incorrect number of cards"
        )
      )
      .toOption
      .get
  }
}
