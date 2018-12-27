import scala.annotation.tailrec

object Problem37 extends App
{
/*
The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
*/

  type Numero = Long

  lazy val primes: Stream[Numero] = {
    def next(p: Numero): Stream[Numero] = {

      @tailrec
      def nextPrime(v: Numero): Numero = if (isPrime(v)) v else (nextPrime(v + 1))

      val np = nextPrime(p + 1)
      np #:: next(np)
    }

    2 #:: 3 #:: next(3)
  }
  
  def isPrime(n: Numero) = {
    val ret = primes.dropWhile(p => p * p <= n && n % p != 0)
    ret.head * ret.head > n && n != 1
  }

   // from lpiepiora
  def isTruncatable(p: Numero) = truncateRight(p).forall(isPrime) && truncateLeft(p).forall(isPrime)
  
  // from lpiepiora
  def truncateRight(n: Numero): Stream[Numero] = 
    if(n / 10 == 0) Stream(n)
    else n #:: truncateRight(n / 10)

  // from lpiepiora
  def truncateLeft(n: Numero, b: Numero = 10): Stream[Numero] =
    if(b > n) Stream(n)
    else (n % b) #:: truncateLeft(n, b * 10)  
    
  
  val solution = primes.dropWhile(_<10).filter( isTruncatable ).take(11)
  
  println( s"Solution:$solution ${solution.sum}" )    
  
  
}