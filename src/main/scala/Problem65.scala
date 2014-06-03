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


  import Fraction._

  import scala.math._
  import Integral.Implicits._

  class Fraction[T](val num: T, val den: T)(implicit numT: Integral[T]) {

    type Self = Fraction[T]

    def mcd(a: T, b: T): T = if (b == 0) a else mcd(b, a % b)

    def mcm(a: T, b: T) = a * b / mcd(a, b)

    def +(f: Self): Self = Fraction(num * f.den + f.num * den, den * f.den).simplify

    def +(n: T): Self = this + Fraction(n)

    def /(f: Self): Self = Fraction(num * f.den, den * f.num).simplify

    def /(n: T): Self = this / Fraction(n)

    def simplify: Self = {
      val m = mcd(num, den)
      Fraction(num / m, den / m)
    }

    override def toString = s"$num/$den"
  }


  def expand[T](timesExpand: Int)(implicit num: Integral[T]) = {
    val one = oneFraction[T]
    val two = num.fromInt(2)
    val half = one / two
    def expand0(timesExpand: Int, accum: Fraction[T], ret: List[Fraction[T]]): List[Fraction[T]] = {
      if (timesExpand == 0){
        ret
      }
      else{
        val nextValue = one / (accum + two)
        val nextRet = accum :: ret
        expand0(timesExpand - 1, nextValue, nextRet)
      }
    }
    expand0(timesExpand, half, Nil)
  }

  case class ContinuedFraction( intPart: Int, terms: Seq[Int] ){
  }

  val e = ContinuedFraction( 2, Stream.from(1) zip Stream.from(1).map(_*2) zip Stream.from(1)

  println(Fraction(32, 34))
  println(Fraction(32, 34) + 1)
  println(oneFraction[Int] / 2)

  measure() {
    val expansions = expand[BigInt](1000).map( _ + 1 )
    val solution = expansions.count( f => f.num.toString.size > f.den.toString.size )
    println( s"Solution: $solution" )
  }


}