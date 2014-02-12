object Problem46 extends App{

/*
It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.

9 = 7 + 2×12
15 = 7 + 2×22
21 = 3 + 2×32
25 = 7 + 2×32
27 = 19 + 2×22
33 = 31 + 2×12

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
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
  
  val doubleSquares = {
    def next(n: Numero) : Stream[Numero] = (2*n*n) #:: next(n+1)
    2 #:: next(2)
  }
  
  val oddNotPrimes = {
    def next(n: Numero) : Stream[Numero] = n #:: next(n+2)
    ( 1 #:: next(3) ).filter( !isPrime(_) )
  }
  
  def conjeture( n: Numero ) = {
    println( s"Conjeture: $n" )
    val primesToTest = primes.takeWhile(_<n)
    primesToTest.exists{ p => 
      doubleSquares.takeWhile( _ + p <= n ).last + p == n
    }
  }
  
  val solution = oddNotPrimes.find( !conjeture(_) ) 
  println( s"Solution: $solution" )

}