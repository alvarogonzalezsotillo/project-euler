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

  object Fraction{
    def apply[T : Integral]( num: T, den: T ) : Fraction[T] = new Fraction(num,den)
    def apply[T : Integral]( num: T ) : Fraction[T] = new Fraction(num, implicitly[Integral[T]].one)
    implicit def toFraction[T : Integral](n:T) : Fraction[T] = Fraction.apply(n)
    def one[T : Integral] = implicitly[Integral[T]].one
  }

  import Fraction._
  import scala.math._
  import Integral.Implicits._
  class Fraction[T : Integral](val num:T, val den:T  ){
  
    type Self = Fraction[T]
    
    def mcd( a: T, b: T ) : T = if( b == 0 ) a else mcd( b, a%b )

    def mcm( a: T, b: T ) = a*b/mcd(a,b)

    def +( f: Self ) : Self = Fraction(num*f.den + f.num*den, den*f.den).simplify

    def simplify : Self = {
      val m = mcd(num,den)
      Fraction(num/m,den/m)
    }
    
    override def toString = s"$num/$den"
  }

   
  

  println( 1 + Fraction(32,34) )

    
}