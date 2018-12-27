import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 13/01/14
 * Time: 11:17
 * To change this template use File | Settings | File Templates.
 */
object Problem3 extends App {

  /*
  The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
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

  val number: Numero = 600851475143L

  def largestPrimeFactor(n: Numero) = primes.takeWhile(p => p * p < n).filter(p => n % p == 0).last

  println(s"Solution: ${largestPrimeFactor(number)}")

}
