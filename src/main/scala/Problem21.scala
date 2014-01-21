import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 21/01/14
 * Time: 12:52
 * To change this template use File | Settings | File Templates.
 */
object Problem21 extends App {
  /*
  Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
   */

  val ini = System.currentTimeMillis()
  type Numero = Long

  lazy val primes: Stream[Numero] = {
    def next(p: Numero): Stream[Numero] = {

      def isPrime(n: Numero) = {
        val ret = primes.dropWhile(p => p * p <= n && n % p != 0)
        ret.head * ret.head > n
      }

      @tailrec
      def nextPrime(v: Numero): Numero = if (isPrime(v)) v else (nextPrime(v + 1))

      val np = nextPrime(p + 1)
      np #:: next(np)
    }

    2 #:: 3 #:: next(3)
  }

  def primeFactors(n: Numero) = {

    @tailrec
    def pf(n: Numero, accum: List[Numero]): List[Numero] = n match {
      case 0 => accum
      case 1 => accum
      case _ =>
        val factor = primes.find(p => n % p == 0).get
        pf(n / factor, factor :: accum)
    }

    pf(n, Nil)
  }

  def divisors(n: Numero) = {
    def subsets(s: Seq[Numero]) = for (l <- 0 to s.size; c <- s.combinations(l)) yield c
    val factors = primeFactors(n)
    def product(s: Seq[Numero]) = if (s.size == 0) 1L else s.product
    for (s <- subsets(factors) if s.size != factors.size) yield product(s)
  }

  def sumOfDivisors(n: Numero) = divisors(n).sum

  val NONUMBER: Numero = -1

  def amicableOf(a: Numero) = {
    val da = sumOfDivisors(a)
    val db = sumOfDivisors(da)
    if (a == db && a != da)
      da
    else
      NONUMBER
  }


  var solution: Numero = 0
  val amicableNumbers = for (a <- 1 until 10000) {
    val b = amicableOf(a)
    if ( b > a ) solution += a + b
  }

  println(s"Solution: $solution")
  println(s"${System.currentTimeMillis - ini}")
}
