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
  val primeNumbersReversed = primeNumbers.reverse.map( BigInt(_) ).toArray
  val bitmap = Array.ofDim[Boolean](limit)
  
  println( "Computing bitmap..." )
  primeNumbers.foreach( n => bitmap(n.toInt) = true )
  def isPrime( n: Numero ) = bitmap(n.toInt)
  def isPrime( n: BigInt ) = bitmap(n.toInt)

  def firstPrimeSum( primes: Array[BigInt], size: Int ) : Option[Array[BigInt]] = {
    var ini = 0
    var end = size-1
    var sum = primes.slice(ini,size).sum
    println( s"size:$size  ini:$ini end:$end sum:$sum" )
    println( s"  slice:${primes.slice(ini,size).toList}" )

    while( (sum >= limit || !isPrime(sum)) && primes.size > ini+size ){
      sum -= primes(ini)
      sum += primes(ini+size)
      ini += 1
      
      println( s"  ini:$ini sum:$sum" )
    }
    
    if( isPrime(sum) ) Some(primes.slice(ini,size)) else None
  }
  

  val candidates = for( size <- (2 to primeNumbersReversed.size) ) yield{
    val yi = firstPrimeSum(primeNumbersReversed,size)
    println( s"$size -> " + (
      yi match{
        case Some(p) => p.mkString(",") + " --> " + p.sum
        case None => ""
      } ) )
      
    yi
  }
  
  println( s"candidates:${candidates.mkString("\n")}" )                        
                        

  // no es 92951
}