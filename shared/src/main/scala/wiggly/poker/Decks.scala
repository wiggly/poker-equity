package wiggly.poker

object Decks {
  opaque type Deck = List[Card]

  object Deck {
    def create: Deck = {
      (for {
        s <- Suit.values
        r <- Rank.values
      } yield Card(s, r)).toList
    }

    extension (d: Deck) {
      def toList: List[Card] = d.toList
    }
  }
}
