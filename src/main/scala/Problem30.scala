import scala.annotation.tailrec

object Problem30 extends App{
/*
In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
How many different ways can £2 be made using any number of coins?
*/

  type Numero = Long

  val coins = List(1,2,5,10,20,50,100,200).sorted.reverse
  
  object ways{
  
    val cache = collection.mutable.Map[(Int,Int),Numero]()
  
    for( c <- coins ) cache((0,c)) = 1
    
    def apply( pences: Int, minCoin: Int ) : Numero = {
      val key = (pences,minCoin)
      if( pences < 0 ){
        0
      }
      else if( cache.contains(key) ){
        cache(key)
      }
      else{
        cache(key) = computeWithCoinsBiggerThan(pences,minCoin)
        cache(key)
      }
    }
    
    def compute( pences: Int, remainingCoins: Array ) = pences match{
      case p < remainingCoins.head => 
      
    }
    
    def computeWithCoinsBiggerThan( pences: Int, minCoin: Int  ){
      println( s"  compute: $pences $minCoin" )
      val ret = for( c <- coins if coin >= minCoin ) yield{
        for( n <- 0 to pences/c ) yield{
          apply( pences-n*c, 
        }
      }
      
      
      coins.filter( _ >= minCoin).map( c => apply(pences-c, minCoin) )
      println( s"    compute: $pences -> ${ret.mkString(",")}" )
      ret.sum
    }
    
  }
  
  val amount = args(0).toInt
  val solution = ways(amount, coins.max)
  println( s"Solution:$solution" )
    
}
  

