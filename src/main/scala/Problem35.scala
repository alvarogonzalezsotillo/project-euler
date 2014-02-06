import scala.annotation.tailrec

object Problem35 extends App {
  /*
The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?
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
  
  val primesBelowTenMillion = primes.takeWhile( _ < 10000000 ).toSet
  
  def rotations( n: Numero ) = {
	val digits = n.toString
	def s = digits.size
	val ret = for( i <- 0 until s ) yield digits.slice(i,s) + digits.slice(0,i)
	ret.map( _.toLong )
  }

  
  
  println( s"Number of primesBelowTenMillion: ${primesBelowTenMillion.size}")
  
  def isCircular(p: Numero) = rotations(p).forall( primesBelowTenMillion.contains(_) )
  
  val solution = primes.takeWhile( _ < 1000000 ).filter(isCircular)
  
  println( s"Solution:${solution.size}")
}