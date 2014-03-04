
object Problem47sieve extends App{
/*
The first two consecutive numbers to have two distinct prime factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?
*/

  val ini = System.currentTimeMillis

  type Numero = Long
  
  def makeSieve( max: Int ) = {
    val sieve = Array[Int].ofDim(max+1)
    (0 until sieve.size).foreach{ i =>
      if( sieve(i) == 0 )
        Range.count(i,sieve.size,i,false).foreach( sieve(_) += 1 )
    }
    sieve
  }
  
  def findSolution( size: Int, max: Int = 1000000 ) = {
    val sieve = makeSieve(max)
    
  }

  
  println( findSolution(4) )
  println( s"millis: ${System.currentTimeMillis-ini}")

}