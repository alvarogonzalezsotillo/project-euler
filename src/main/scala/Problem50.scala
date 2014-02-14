object Problem50 extends App{


/*
The prime 41, can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13
This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
*/


 type Numero = Long


  lazy val primes: Stream[Numero] = {
    def next(p: Numero): Stream[Numero] = {

      def nextPrime(v: Numero): Numero = if (isPrime(v)) v else (nextPrime(v + 1))

      val np = nextPrime(p + 1)
      np #:: next(np)
    }

    2 #:: 3 #:: next(3)
  }
  
  def isPrime(n: Numero) = {
    val ret = primes.dropWhile(p => p * p <= n && n % p != 0)
    ret.head * ret.head > n
  }
  
  
  val limit = 1000000
  val primeNumbers = primes.takeWhile(_<limit)
  
  val candidates = for( size <- 2 to primeNumbers.size ;
                        candidate <- primeNumbers.sliding(size) ;
                        if( isPrime(candidate.sum) ) )
                        yield candidate
                        
  println( s"candidates:$candidates" )                        

}