object Problem41 extends App {

  /*We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?
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


  def isPandigital(n: Numero, digits: Int) = {
    val s = n.toString.toSet
    s.size == digits && n.toString.size == digits && !s.contains('0')
  } 
  
  def pow(base: Numero, exp: Int) : Numero = if( exp == 0 ) 1 else base*pow( base, exp-1)
  
  val candidates = for( digits <- 2 to 9 ) yield{
    val max = pow(10,digits)
    primes.takeWhile(_<max).filter(isPandigital(_,digits)).max
  }
  
  val solution = candidates.max
  println( s"Solution:$solution" )
  
}