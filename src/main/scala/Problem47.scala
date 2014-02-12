
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
  
  def find( totalToFind: Int, currentFound: Int, candidate: Numero ) : Seq[Numero]= {
    if( primeFactors(candidate).size == totalToFind ){
      if( currentFound == totalToFind-1 ){
        candidate-totalToFind+1 to candidate
      }
      else{
        find( totalToFind, currentFound+1, candidate+1 )
      }
    }
    else{
      find( totalToFind, 0, candidate+1)
    }
  }
  
  println( find(2,0,1) )

}