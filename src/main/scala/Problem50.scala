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
  
  val limit = 100
  println( "Computing primes..." )
  val primeNumbers = primes.takeWhile(_<=limit)
  val primeNumbersReversed = primeNumbers.reverse.map( BigInt(_) ).toArray
  val bitmap = Array.ofDim[Boolean](limit)
  
  println( "Computing bitmap..." )
  primeNumbers.foreach( n => bitmap(n.toInt) = true )
  def isPrime( n: Numero ) : Boolean = if( n >= 0 && n < bitmap.size) bitmap(n.toInt) else false
  def isPrime( n: BigInt ) : Boolean = isPrime(n.toLong)

  def firstPrimeSum( primes: Array[BigInt], size: Int ) : Option[Array[BigInt]] = {
    var ini = 0
    var end = size-1
    var sum = primes.slice(ini,size).sum
    println( s"size:$size  ini:$ini end:$end sum:$sum" )
    println( s"  slice:${primes.slice(ini,size).toList}" )

    println( s"  ${!isPrime(sum)}  ${primes.size > ini+size}" )
    while( !isPrime(sum) && primes.size > ini+size ){
      sum -= primes(ini)
      sum += primes(ini+size)
      ini += 1
      println( s"  sum:$sum ini:$ini" )
      println( s"  ${!isPrime(sum)}  ${primes.size > ini+size}" )
      
    }
    println( s"  $ini  $sum" )
    if( isPrime(sum) ) Some(primes.slice(ini,size)) else None
  }
  

  val candidates = for( size <- (2 to primeNumbersReversed.size) if(size%2==1) ) yield{
    val yi = firstPrimeSum(primeNumbersReversed,size)
    println( s"$size -> " + (
      yi match{
        case Some(p) => p.mkString(",") + " --> " + p.sum
        case None => "(none)"
      } ) )
      
    yi
  }
  
  println( s"candidates:${candidates.map( _.map(_.mkString("\n")))}" )
                        

  // no es 92951
}