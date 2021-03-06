object Problem50 extends App{


/*
The prime 41, can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13
This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
*/

  def measure[T]( msg: String )( proc: =>T ) = {
    println( s"-->$msg" )
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println( s"<--$msg: ${end-ini} ms" )
    ret
  }

  type Numero = Long
 
  measure( "Total time" ){ 

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
    val primeNumbers = measure( "Computing primes"){
      primes.takeWhile(_<=limit)
    }
    
    val bitmap = Array.ofDim[Boolean](limit)
    measure( "Computing bitmap..." ){
      primeNumbers.foreach( n => bitmap(n.toInt) = true )
    }
    
    def isPrime( n: Numero ) = if( n >= 0 && n < bitmap.size) bitmap(n.toInt) else false

    def firstPrimeSum( primes: Array[Numero], size: Int ) = {
      var ini = 0
      var sum = primes.slice(ini,ini+size).sum
      
      def found = isPrime(sum) && sum < limit

      while( !found && primes.size > ini+size ){
        sum -= primes(ini)
        sum += primes(ini+size)
        ini += 1
      }
      
      if( found ) Some(primes.slice(ini,ini+size)) else None
    }
    
    val maxSize = measure( "Computing maxSize" ){
      def m(s: Int, primes: Stream[Numero] = primeNumbers,acum:Numero = 0) : Int = {
        if( acum < limit ) 
          m(s+1,primes.tail,acum+primes.head) 
        else
          s
      }
      m(1)
    }

    println( s"maxSize:$maxSize" )
    val primeNumbersReversed = primeNumbers.reverse.toArray
    val candidates = for( size <- (maxSize to 2 by -1).view ) yield{
      firstPrimeSum(primeNumbersReversed,size)
    }
    
    val firstCandidate = measure( "Finding first candidate" ){
      candidates.filter( !_.isEmpty ).map(_.get).head
    }
    
    println( s"FirstCandidate:${firstCandidate.sum} ${firstCandidate.mkString(",")}" )
  }
  // 997651
}