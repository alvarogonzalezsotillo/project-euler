/*
The square root of 2 can be written as an infinite continued fraction.

v2 = 1 +	
1
 	2 +	
1
 	 	2 +	
1
 	 	 	2 +	
1
 	 	 	 	2 + ...
The infinite continued fraction can be written, v2 = [1;(2)], (2) indicates that 2 repeats ad infinitum. In a similar way, v23 = [4;(1,3,1,8)].

It turns out that the sequence of partial values of continued fractions for square roots provide the best rational approximations. Let us consider the convergents for v2.

1 +	
1
= 3/2
 	
2
 
1 +	
1
= 7/5
 	2 +	
1
 	 	
2
 
1 +	
1
= 17/12
 	2 +	
1
 
 	 	2 +	
1
 
 	 	 	
2
 
1 +	
1
= 41/29
 	2 +	
1
 	 	2 +	
1
 
 	 	 	2 +	
1
 
 	 	 	 	
2
 
Hence the sequence of the first ten convergents for v2 are:

1, 3/2, 7/5, 17/12, 41/29, 99/70, 239/169, 577/408, 1393/985, 3363/2378, ...
What is most surprising is that the important mathematical constant,
e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].

The first ten terms in the sequence of convergents for e are:

2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...
The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.

Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.
*/

object Problem65 extends App {


  def measure[T](proc: => T) = {
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println(s"${end - ini} ms")
    ret
  }

  object Fraction {
    def apply[T: Integral](num: T, den: T): Fraction[T] = new Fraction(num, den)

    def apply[T: Integral](num: T): Fraction[T] = new Fraction(num, oneScalar)

    def oneScalar[T: Integral] = implicitly[Integral[T]].one

    def oneFraction[T: Integral] = apply(oneScalar)
  }

  class StringIntegral extends Integral[String]{
    type S = String
    def compare( x: S, y: S ) = ???
    def fromInt( i: Int ) = i.toString
    def minus( x: S, y: S ) = s"($x)-($y)"
    def negate( x: S ) = s"-($x)"
    def plus( x: S, y: S ) = s"($x)+($y)"
    def quot( x: S, y: S ) = s"IntPart(($x)/($y))"
    def rem( x: S, y: S ) = s"($x)%($y)"
    def times(x: S, y: S ) = s"($x)*($y)"
    def toDouble(x: S) = ???
    def toFloat(x: S) = ???
    def toInt(x: S) = ???
    def toLong(x: S) = ???
  }


  import Fraction._

  import scala.math._
  import Integral.Implicits._

  class Fraction[T](val num: T, val den: T)(implicit numT: Integral[T]) {

    type Self = Fraction[T]

    def mcd(a: T, b: T): T = if (b == 0) a else mcd(b, a % b)

    def mcm(a: T, b: T) = a * b / mcd(a, b)

    def add(f: Self): Self = Fraction(num * f.den + f.num * den, den * f.den).simplify

    def divide(f: Self): Self = Fraction(num * f.den, den * f.num).simplify

    def simplify: Self = {
      val m = mcd(num, den)
      Fraction(num / m, den / m)
    }

    override def toString = s"$num/$den"
  }


  implicit class FractionEx[T:Integral]( f: Fraction[T] ){
    type F = Fraction[T]
    def +( fr: F ) : F = f add fr
    def +( t: T ) : F = f add Fraction(t)
    def /( fr: F )  : F = f divide fr
    def /( t: T ) : F = f divide Fraction(t)
  }
  
  implicit class EscalarAsFractionEx[T:Integral]( t: T ){
    type F = Fraction[T]
    def +( fr: F ) : F = Fraction(t) add fr
    def /( fr: F )  : F = Fraction(t) divide fr
  }

  case class ContinuedFraction[T]( intPart: T, terms: Seq[T] )(implicit numT: Integral[T]){
    def expand( n: Int ) = {
      if( n == 0 ){
        Fraction(intPart)
      }
      else{
        var ret = Fraction( terms(n-1) )
        val one = Fraction.oneFraction[T]
        for( i <- n-2 to 0 by -1 ){
          ret = (one / ret ) + terms(i)
        }
        ret = intPart + ( one / ret)
        ret
      }
    }      
  }

  
  val eExpansion = {
    val ones = Stream.continually( 1 )
    val sequence = Stream.from(1)
    val expansion = ones zip sequence.map(_ * 2) zip ones 
    def flattener( t: ((Int,Int),Int) ) = Seq(t._1._1,t._1._2,t._2)
    expansion.flatten( flattener ).map( BigInt(_) )
              //map( _.toString )
  }
  
  val e = ContinuedFraction( BigInt(2), eExpansion )
  
  println( eExpansion.take(20).toList )
  
  for( i <- 0 to 99 ){
    println( i + "  --  " + e.expand(i) )
  }


}