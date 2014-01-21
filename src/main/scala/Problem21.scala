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

  def amicables(a: Numero, b: Numero) = {
    lazy val da = divisors(a).sum
    lazy val db = divisors(b).sum
    a != b && da == b && db == a
  }

  object amicableOf{
    val NONUMBER = -1L
    val cache = collection.mutable.Map[Numero,Numero]()

    def apply(n: Numero)  = cache.get(n) match{
      case Some(NONUMBER) => None
      case Some(a)        => Some(a)
      case None           => compute(n) match{
        case Some(b) =>
          cache(b) = n
          cache(n) = b
          Some(b)
        case None => None
          cache(n) = NONUMBER
          None
      }
    }

    def compute(a: Numero) = {
      val da = divisors(a).sum
      val db = divisors(da).sum
      if( a == db ) Some(da) else None
    }
  }


  //val amicableNumbers = for (a <- 1 until 10000; b <- a + 1 until 10000 if amicables(a, b)) yield Seq(a, b)
  val amicableNumbers = for( a <- 1 until 10000 ) yield amicableOf(a) match{
    case Some(b) => Seq(a,b)
    case None => Nil
  }
  val solution = amicableNumbers.flatten.sum
  println(s"Solution: $solution")
}
