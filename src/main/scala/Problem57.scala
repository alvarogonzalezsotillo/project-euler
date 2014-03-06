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

  case class Fraction(val num:Numero, val den:Numero)
 
  
  def mcd( a: Numero, b: Numero ) : Numero = if( b == 0 ) a else mcd( b, a%b )
  def mcm( a: Numero, b: Numero ) = a*b/mcd(a,b)
  
  def sum( n: Numero, f: Fraction ) = {
    val denominator = f.den
    val numerator = n*denominator + f.num
    simplify( Fraction(numerator,denominator) )
  }

  def simplify( f: Fraction ) = {
    val m = mcd(f.num,f.den)
    Fraction( f.num/m, f.den/m )
  }


  println( sum( 16, Fraction( 14, 2) ) )
  
    
}