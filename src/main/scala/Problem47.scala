
object Problem47 extends App{
/*
The first two consecutive numbers to have two distinct prime factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?
*/

  val ini = System.currentTimeMillis

  type Numero = Long

  lazy val primes: Stream[Numero] = {
    def next(p: Numero): Stream[Numero] = {

      def isPrime(n: Numero) = {
        val ret = primes.dropWhile(p => p * p <= n && n % p != 0)
        ret.head * ret.head > n
      }

      def nextPrime(v: Numero): Numero = if (isPrime(v)) v else (nextPrime(v + 1))

      val np = nextPrime(p + 1)
      np #:: next(np)
    }

    2 #:: 3 #:: next(3)
  }

  def primeFactorsSize( n: Numero ) = {
    def pf( n: Numero, primesFrom: Stream[Numero], accum : Int ) : Int = {
      val nextPrimes = primesFrom.dropWhile( p => n % p != 0 && p <= n)
      val factor = nextPrimes.head
      if( factor > n )
        accum
      else
        pf( n/factor, nextPrimes.tail, accum+1 )
    }

    pf( n, primes, 0 )
  }
  
  def findSolution(totalToFind: Int) = {
    def find( currentFound: Int, candidate: Numero ) : Seq[Numero]= {
      if( currentFound == totalToFind )
        candidate-totalToFind until candidate
      else if( primeFactorsSize(candidate) == totalToFind )
        find( currentFound+1, candidate+1 )
      else
        find( 0, candidate+1)
    }
    find(0,1)
  }
  
  println( findSolution(4) )
  println( s"millis: ${System.currentTimeMillis-ini}")

}