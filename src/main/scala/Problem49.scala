
object Problem49 extends App{


/*
The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?
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
  
  val fourDigitsPrimes = primes.dropWhile(_<1000).takeWhile(_<10000)
  
  def check( step: Int ) = {
    def c( p: Stream[Numero], n: Int ) : Boolean = 
      if( n == 4 ) 
        true
      else
           
      
  }
  
  



}