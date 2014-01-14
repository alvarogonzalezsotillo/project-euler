import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 14/01/14
 * Time: 11:52
 * To change this template use File | Settings | File Templates.
 */
object Problem7 extends App {

  /*
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

  What is the 10 001st prime number?
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

  val solution = primes.drop(10000).head

  println(s"Solution:$solution")
  println( s"${System.currentTimeMillis - ini}" )

}
