import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 14/01/14
 * Time: 12:49
 * To change this template use File | Settings | File Templates.
 */
object Problem10 extends App {

  /*
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.
  */
  val ini = System.currentTimeMillis
  type Numero = Long

  lazy val primes: Stream[Numero] = {
    def continueStream(p: Numero): Stream[Numero] = {

      def isPrime(n: Numero) = {
        val ret = primes.dropWhile(p => p * p <= n && n % p != 0)
        ret.head * ret.head > n
      }

      @tailrec
      def nextPrime(v: Numero): Numero = if (isPrime(v)) v else (nextPrime(v + 1))

      val np = nextPrime(p + 1)
      np #:: continueStream(np)
    }

    2 #:: 3 #:: continueStream(3)
  }

  val solution = primes.takeWhile(_<2000000).sum
  println(s"Solution:$solution")
  println(s"${System.currentTimeMillis-ini}")
}
