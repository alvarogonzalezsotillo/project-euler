import scala.annotation.tailrec

object Problem31 extends App{
/*
In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
How many different ways can £2 be made using any number of coins?
*/

  type Numero = Long

  val coins = List(1,2,5,10,20,50,100,200)
  
  def compute( pences: Int ) = {
  
    def compute( pences: Int, remainingCoins: List[Int] ): Numero = {
      val ret = remainingCoins match{
        case Nil => 
          0L
        case coin :: Nil => 
          1L
        case coin :: restOfCoins =>
          (0 to pences/coin).map( n => compute( pences - n*coin, restOfCoins ) ).sum
      }

      ret
    }
    compute(pences, coins.sorted.reverse)
  }
    
  
  val amount = args(0).toInt
  val solution = compute(amount)
  println( s"Solution:$solution" )
}
  

