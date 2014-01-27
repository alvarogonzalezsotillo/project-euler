import scala.annotation.tailrec

object Problem27 extends App{
/*
Euler discovered the remarkable quadratic formula:

n� + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41� + 41 + 41 is clearly divisible by 41.

The incredible formula  n� - 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, -79 and 1601, is -126479.

Considering quadratics of the form:

n� + an + b, where |a| < 1000 and |b| < 1000

where |n| is the modulus/absolute value of n
e.g. |11| = 11 and |-4| = 4
Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.
*/

  type Numero = Int


  def isPrime(n: Numero) = {
    val ret = primes.dropWhile(p => p * p <= n && n % p != 0)
    ret.head * ret.head > n
  }

  lazy val primes: Stream[Numero] = {
    def next(p: Numero): Stream[Numero] = {

      @tailrec
      def nextPrime(v: Numero): Numero = if (isPrime(v)) v else (nextPrime(v + 1))

      val np = nextPrime(p + 1)
      np #:: next(np)
    }

    2 #:: 3 #:: next(3)
  }
  

  val candidates = for( a <- -999 to 999 ; b <- -999 to 999 ) yield{
    def p(n: Numero) = n > 1 && isPrime(n)
    def f(n: Numero) = n*n + a*n +b
    val seq = Iterator.from(0).map(f).takeWhile(p).toSeq
    val yieldValue = (a,b,seq)
    yieldValue
  }

  val solution = candidates.maxBy{ case (a,b,seq) => seq.size }
  println( s"\nSolution: $solution: ${solution._1*solution._2}")
}
  

