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

      def isPrime(n: Numero) : Boolean = {
        val ret = primes.dropWhile(p => p * p <= n && n % p != 0)
        ret.head * ret.head > n
      }

      def nextPrime(v: Numero): Numero = if (isPrime(v)) v else (nextPrime(v + 1))

      val np = nextPrime(p + 1)
      np #:: next(np)
    }

    2 #:: 3 #:: next(3)
  }
  
  val limit = 1000000
  println( "Computing primes..." )
  val primeNumbers = primes.takeWhile(_<=limit)
  val primeNumbersReversed = primeNumbers.reverse.toArray
  val bitmap = Array.ofDim[Boolean](limit)
  
  println( "Computing bitmap..." )
  primeNumbers.foreach( n => bitmap(n.toInt) = true )
  def isPrime( n: Numero ) = bitmap(n.toInt)
  
  
  lazy val candidates = for( size <- (2 to primeNumbers.size).view ) yield {
    val candidate = primeNumbersReversed.
                    sliding(size).
                    map( c => (c,c.sum) ).  
                    dropWhile( _._2 >= limit ).
                    find( c => isPrime(c._2) ).
                    map( _._1 )

    println( s"size:$size -> ${(candidate getOrElse Array()).mkString(",")} -> ${(candidate getOrElse Array()).sum}" )
    candidate
  }

  println( s"candidates:${candidates.mkString("\n")}" )                        
                        

  // no es 92951
}