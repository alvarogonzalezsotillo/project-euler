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


  def measure[T]( msg: String = "" )( proc: =>T ) = {
    println( s"-->$msg" )
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println( s"<--$msg: ${end-ini} ms" )
    ret
  }

  object Fraction{
    def apply[T : Integral]( num: T, den: T ) : Fraction[T] = new Fraction(num,den)
    def apply[T : Integral]( num: T ) : Fraction[T] = new Fraction(num, oneScalar)
    //implicit def toFraction[T : Integral](n:T) : Fraction[T] = Fraction.apply(n)
    def oneScalar[T : Integral] = implicitly[Integral[T]].one
    def oneFraction[T : Integral] = apply(oneScalar)
  }
  

  import Fraction._
  import scala.math._
  import Integral.Implicits._
  class Fraction[T](val num:T, val den:T  )(implicit numT: Integral[T] ){

    type Self = Fraction[T]
    
    def mcd( a: T, b: T ) : T = if( b == 0 ) a else mcd( b, a%b )

    def mcm( a: T, b: T ) = a*b/mcd(a,b)

    def +( f: Self ) : Self = Fraction(num*f.den + f.num*den, den*f.den).simplify

    private def +( n: T ) : Self = this + Fraction(n)

    def +( n: Int ) : Self = this + numT.fromInt(n)

    def /( f: Self ): Self = Fraction(num*f.den, den*f.num).simplify

    private def /( n: T ): Self = this / Fraction(n)

    def /( n: Int ): Self = this / numT.fromInt(n)


    def simplify : Self = {
      val m = mcd(num,den)
      Fraction(num/m,den/m)
    }
    
    override def toString = s"$num/$den"
  }


  def expand[T : Integral]( timesExpand: Int ) = {
    val one = oneFraction[T]
    val half = one / 2
    def expand0( timesExpand: Int, accum: Fraction[T] ) : Fraction[T] = {
      if( timesExpand == 0 )
        accum
      else
        expand0( timesExpand-1, one/( accum + 2 ) )
    }
    expand0( timesExpand, half ) + one
  }
  

  println( Fraction(32,34))
  println( Fraction(32,34) + 1)  
  println( oneFraction[Int]/2 )

  measure(){
    for( t <- 0 to 1000 ){
      println( s"$t  -->  ${expand[BigInt](t)}" )
    }
  }

    
}