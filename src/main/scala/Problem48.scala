

object Problem48 extends App{

/*
he series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
*/

  val ini = System.currentTimeMillis
  
  type Numero = Long
  
  val mod = 10000000000L
  
  def computeLastDigitsOfPower( n: Numero ) = {
    def compute( exp: Numero ) : Numero = if( exp == 0 ) 1 else  n*compute(exp-1) % mod
    compute(n)
  }
  
  val solution = (1 to 1000).
                 map( 1L*_ ).
                 map( computeLastDigitsOfPower ).
                 foldLeft(0L)( (accum,n) => (accum+n)%mod )
  
  println( s"Solution:$solution" )
  println( s"ms: ${System.currentTimeMillis-ini}" )

  val ini2 = System.currentTimeMillis

  val result = Range(1,1001).map(i => BigInt(i).pow(i)).sum.toString.takeRight(10).toLong
  println( s"result:$result" )
  println( s"ms: ${System.currentTimeMillis-ini2}" )
}