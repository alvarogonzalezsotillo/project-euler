/*
There are exactly ten ways of selecting three from five, 12345:

123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

In combinatorics, we use the notation, 5C3 = 10.

In general,

nCr =	
n!
r!(n−r)!
,where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.

How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are greater than one-million?
*/

object Problem53 extends App{

  def measure[T]( msg: String )( proc: =>T ) = {
    println( s"-->$msg" )
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println( s"<--$msg: ${end-ini} ms" )
    ret
  }

  type Numero = BigInt

  implicit def toNumero(i:Int) : BigInt= BigInt(i)
  implicit def toWithCombinations(i:Int) : withCombinations = new withCombinations(i)

  class withCombinations(n: Numero){
    def C( r: Numero ) = {
      val nFact_rFact = ((r+1) to n).fold(toNumero(1))( _ * _ )
      val ret = ((n-r) to 1 by -1).fold(nFact_rFact)( _ / _ )
      ret
    }
  }

  measure("Problem 53"){
    val combinations = for( n <- 1 to 100 ; r <- 1 to n ) yield n C r
    val solution = combinations.count( _ > 1000000 )
    println( s"Solution:$solution" )
  }
}