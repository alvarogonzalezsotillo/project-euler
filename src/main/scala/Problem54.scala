import scala.io.Source

/*
n the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

High Card: Highest value card.
One Pair: Two cards of the same value.
Two Pairs: Two different pairs.
Three of a Kind: Three cards of the same value.
Straight: All cards are consecutive values.
Flush: All cards of the same suit.
Full House: Three of a kind and a pair.
Four of a Kind: Four cards of the same value.
Straight Flush: All cards are consecutive values of same suit.
Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
The cards are valued in the order:
2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights
beats a pair of fives (see example 1 below). But if two ranks tie, for example,
both players have a pair of queens, then highest cards in each hand are compared (see example 4 below);
if the highest cards tie then the next highest cards are compared, and so on.

Consider the following five hands dealt to two players:

Hand	 	Player 1	 	Player 2	 	Winner
1	 	5H 5C 6S 7S KD
Pair of Fives
 	2C 3S 8S 8D TD
Pair of Eights
 	Player 2

2	 	5D 8C 9S JS AC
Highest card Ace
 	2C 5C 7D 8S QH
Highest card Queen
 	Player 1

3	 	2D 9C AS AH AC
Three Aces
 	3D 6D 7D TD QD
Flush with Diamonds
 	Player 2

4	 	4D 6S 9H QH QC
Pair of Queens
Highest card Nine
 	3D 6D 7H QD QS
Pair of Queens
Highest card Seven
 	Player 1
5	 	2H 2D 4C 4D 4S
Full House
With Three Fours
 	3C 3D 3S 9S 9D
Full House
with Three Threes
 	Player 1
The file, poker.txt, contains one-thousand random hands dealt to two players. Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards. You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.

How many hands does Player 1 win?
*/

object Problem54 extends App {

  type Suit = Char
  type Rank = Int

  object Rank {
    val fromCharMap = Map('T' -> 10, 'J' -> 11, 'Q' -> 12, 'K' -> 13, 'A' -> 14).withDefault(c => c - '0')
    def valid(rank: Rank) = rank >= 2 && rank <= 14
    def apply(c: Char) = fromCharMap(c) ensuring valid _
    def apply(i: Int) = i ensuring valid _
    def unapply(r: Rank) = Some(r)
    def toString(rank: Rank) = {
      assert(valid(rank))
      String.format("%02d", rank: Integer)
    }
  }

  class Card(val rank: Rank, val suit: Suit) extends Pair(rank, suit)

  object Card {
    def apply(s: String) = new Card(Rank(s(0)), s(1))
  }

  class Hand(cards: Seq[Card]) extends Ordered[Hand] {
    val _cards = cards.toList.sortBy(-_.rank )

    lazy val kinds = {
      val groups = _cards.groupBy(_.rank).toList
      val groupsBySize = groups.map {
        case (rank, cards) => (cards.size, rank)
      }
      groupsBySize.toArray.sortBy {
        case (size, rank) => -rank
      }
    }

    type Match = Option[Seq[Rank]]

    def royalFlush: Match =  straightFlush match {
        case Some(Seq(Rank(10))) => Some(Seq(Rank(14)))
        case _ => None
    }

    def straightFlush: Match =  flush.isDefined && straight.isDefined match {
        case true => Some(Seq(_cards.head.rank))
        case _ => None
    }

    def straight: Match = {
      val first = _cards.head
      (1 to 4).forall(i => _cards(i).rank == first.rank + i) match {
        case true => Some(Seq(first.rank))
        case _ => None
      }
    }

    def findOfAKind(howMany: Int) = kinds.filter {
      case (size, _) => size == howMany
    }

    def someOfAKind(howMany: Int): Match = {
      val found = findOfAKind(howMany)
      if (found.size == 0)
        None
      else
        Some(found.map(_._2).sorted.reverse)
    }

    def fourOfAKind = someOfAKind(4)

    def fullHouse: Match = for (t3 <- threeOfAKind; t2 <- pair) yield t3 ++ t2

    def threeOfAKind = someOfAKind(3)

    def twoPairs : Match = {
      val found = findOfAKind(2)
      if (found.size < 2)
        None
      else
        Some( found.map( _._2 ) )
    }

    def pair = someOfAKind(2)

    def highest: Match = Some(_cards.reverse.map(_.rank))

    def flush = {
      _cards.forall(_.suit == _cards.head.suit) match {
        case true => highest
        case _ => None
      }
    }

    lazy val describe = {
      def matchToString( m: Match ) = m match{
        case Some(s) => s.foldLeft("")((str, r) => str + Rank.toString(r) )
        case None => ""
      }

      val priority = Seq(royalFlush, straightFlush, fourOfAKind, fullHouse, flush, straight, threeOfAKind, twoPairs, pair, highest)
      priority.zipWithIndex.foldLeft(""){ case (ret, (p,i)) => 
        ret + " " + ('Z' - i).toChar + matchToString(p)
      }
    }

    def compare(that: Hand): Int = describe compare that.describe

    override def toString = describe
  }

  object Hand {
    def apply(s: String) = new Hand(s.trim.split(" ").map(Card.apply))
    def twoHands( line: String ) = {
      val (h1,h2) = line.trim.splitAt( line.size/2 )
      (Hand(h1), Hand(h2))
    }
  }

  val lines = Source.fromFile( "./src/main/scala/poker.txt" ).getLines().toArray
  val hands = lines.map(Hand.twoHands)
  val solution = hands.count{ case (h1,h2) => h1 > h2 }
  println( s"Solution: $solution (376)" )
}