/*
It is possible to show that the square root of two can be expressed as an infinite continued fraction.

v 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

By expanding this for the first four iterations, we get:

1 + 1/2 = 3/2 = 1.5
1 + 1/(2 + 1/2) = 7/5 = 1.4
1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.

In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?
*/

object Problem57 extends App{
  type Numero = Int

  class Fraction[N <: Numeric[T]](val num:N, val den:N ){
    type Number = N
    def +( f: Fraction ) : Fraction[N] = Fraction(num*f.den + f.num*den, den*f.den).simplify
    def +( n: Numero ) : Fraction[N] = this + Fraction(n,1)
    def simplify = {
      val m = mcd(num,den)
      Fraction(num/m,den/m)
    }
    def toString = s"$num/$den"
  }
  object Fraction{
    def apply[N <: Numeric[T]]( num: N, den: N ) = new Fraction(num,den)
    implicit def toFraction[N <: Numeric[T]](n:N) : Fraction = Fraction(n,Numeric[T].one)
  }
  import Fraction._
   
  def mcd[N <: Numeric[T]]( a: N, b: N ) : Numero = if( b == 0 ) a else mcd( b, a%b )
  def mcm[N <: Numeric[T]]( a: N, b: N ) = a*b/mcd(a,b)
  

  println( 1 + Fraction(32,34) )

    
}