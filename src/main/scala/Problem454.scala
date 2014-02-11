import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 16/01/14
 * Time: 11:47
 * To change this template use File | Settings | File Templates.
 */
object Problem454 extends App{
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

  def primeFactors( n: Numero ): Map[Numero, Numero] = {

    @tailrec
    def pf( n: Numero, primesFrom: Stream[Numero], accum : List[Numero] ) : List[Numero] = n match {
      case 1 => accum
      case _ =>
        val nextPrimes = primesFrom.dropWhile( p => n % p != 0 )
        val factor = nextPrimes.head
        pf( n/factor, nextPrimes, factor :: accum )
    }

    val factors = pf( n, primes, Nil )
    factors.groupBy(n => n).mapValues(_.size.asInstanceOf[Numero])
  }

  def numberOfDivisors( n: Numero ) = primeFactors(n).values.map(_+1).product


  val p = primes.takeWhile( _ < 1000000 )
  println( primeFactors(100) )
  println( primeFactors(1234567891234L) )
}
