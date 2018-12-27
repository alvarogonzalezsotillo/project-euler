/*

Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of positive numbers less than or equal to n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.

Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.

Find the value of n, 1 < n < 107, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.

 */
object Problem70 extends App {

  def measure[T](message: String)(proc: => T): T = {
    println(s"$message")
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println(s"$message ${end - ini} ms")
    ret
  }

  def measure[T](proc: => T): T = {
    measure("")(proc)
  }

  type Numero = Long
  type Flotante = Double

  lazy val primes: Stream[Numero] = {
    def next(p: Numero): Stream[Numero] = {

      def isPrime(n: Numero) = {
        val ret = primes.dropWhile(p => p * p <= n && n % p != 0)
        ret.head * ret.head > n
      }

      @scala.annotation.tailrec
      def nextPrime(v: Numero): Numero = if (isPrime(v)) v else (nextPrime(v + 1))

      val np = nextPrime(p + 1)
      np #:: next(np)
    }

    2 #:: 3 #:: next(3)
  }


  def primesOf(n: Numero) = {

    @scala.annotation.tailrec
    def extractFactor(n: Numero, f: Numero): Numero = n % f match {
      case 0 => extractFactor(n / f, f)
      case _ => n
    }

    @scala.annotation.tailrec
    def primesOf_0(n: Numero, previousPrime: Numero, primes: List[Numero]): List[Numero] = {

      if (n <= previousPrime) primes
      else {
        Iterator.iterate(previousPrime + 1)(_ + 1).find(n % _ == 0) match {
          case Some(p) => primesOf_0(extractFactor(n, p), p, p :: primes)
          case None => throw new IllegalStateException(s"cant find a prime factor of $n bigger than $previousPrime")
        }
      }
    }

    val ret = primesOf_0(n, 1, Nil)
    ret
  }

  def isPermutation(n1: Numero, n2: Numero): Boolean = {
    val st1 = n1.toString
    val digits1 = st1.groupBy(c => c)
    val st2 = n2.toString
    lazy val digits2 = st2.groupBy(c => c)
    val ret = digits1 == digits2
    ret
  }

  def isPermutation(p: (Numero, Numero)): Boolean = isPermutation(p._1, p._2)


  def φ(n: Numero) = {
    val primes = primesOf(n)
    if( primes.product == n ){
      // OPTIMIZATION FOR NON REPEATING FACTORS
      primes.map( _ - 1 ).product
    }
    else {
      (n * primes.map(1.0 - 1.0 / _).product).toLong
    }
  }


  def criteria(p: (Numero, Numero)): Double = {
    val n = p._1
    val φ = p._2
    val ret = n.toDouble / φ
    ret
  }

  println( "φ(87109)=79180")
  println(s"87109 ${φ(87109)} ${primesOf(87109)}")
  println( s"primes of 8319823: ${primesOf(8319823)}")
  println( s"φ(8319823):${φ(8319823)}  ${(3557-1)*(2339-1)}")
  isPermutation( 8319823, 8319832 )
  isPermutation( 8319823, 8319831 )


  measure {
    val limit = 1e7.toLong
    val candidatePrimes = measure("Finding primes...") {
      primes.takeWhile(_ < Math.sqrt(limit) * 2).toArray
    }


    val reversedCandidatePrimes = measure("Reversing...") {
      candidatePrimes.toArray.reverse.takeWhile( _ > Math.sqrt(limit)/3 )
    }

    println( s"first prime: ${reversedCandidatePrimes.head}  last prime:${reversedCandidatePrimes.last}" )

    val candidate = measure("Finding candidate...") {
      val candidates = for (
        p1 <- reversedCandidatePrimes ;
        p2 <- reversedCandidatePrimes ;
        c = p1*p2 if ( c <= limit );
        _ = (if(c == 8319823) println( "Aqui") else () );
        totient = φ(c) if (isPermutation(c, totient)))
        yield {
          val value = criteria(c, totient)
          val can = (value, limit, p1, p2, c, totient)
          println( s"candidate: $can" )
          can

      }
      candidates.minBy( _._1 )
    }

    // 8319823
    println(candidate)


    /*
    val solution = (2 to 10e5.toInt).
      view.
      map( n=> (n,φ(n)) ).
      filter( isPermutation ).
      minBy( criteria )
     

    println( s"Solution:$solution" )
     */
  }

}
