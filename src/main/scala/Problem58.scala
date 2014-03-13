/*
Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.

37 36 35 34 33 32 31
38 17 16 15 14 13 30
39 18  5  4  3 12 29
40 19  6  1  2 11 28
41 20  7  8  9 10 27
42 21 22 23 24 25 26
43 44 45 46 47 48 49

It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ˜ 62%.

If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?
*/

/*
	 4n^2 + 1.
	 4n^2 - 10n + 7.
	 (2n+1)^2
	 4*n^2 - 6*n + 3
*/

object Problem58 extends App{
  type Numero = Long

  implicit class WithPower( val n: Numero ) extends AnyVal{
    def pow( exp: Numero ) : Numero = if( exp == 0 ) 1 else n*(n pow (exp-1))
    def ↑( exp: Numero ) = n pow exp
    def between( p: Pair[Numero,Numero] ) = p._1 <= n && p._2 > n
    def isqrt : Numero = isqrt(1)
    def isqrt( candidate: Numero ) : Numero = {
      if( n between ( candidate ↑ 2 , (candidate+1) ↑ 2) )
        candidate
      else
        isqrt((n/candidate + candidate)/2)
    }
  }  
  
  def isPrime( n: Numero ) = {
    Iterator.from(2).takeWhile(_<=n.isqrt).forall( p => n%p != 0 )
  }

  (1 to 30).foreach( n => println( s"n:$n ${isPrime(n)}" ) )

}