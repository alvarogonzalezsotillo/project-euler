/*



Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that there are 21 elements in this set.

How many elements would be contained in the set of reduced proper fractions for d ≤ 1,000,000?

 */
object Problem72 extends App {

  def measure[T](message: String = "")(proc: => T): T = {
    println(s"$message")
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println(s"$message ${end - ini} ms")
    ret
  }

  type Numero = Long
  type Flotante = Double


  lazy val primes: Stream[Numero] = {
    def next(p: Numero): Stream[Numero] = {

      @scala.annotation.tailrec
      def nextPrime(v: Numero): Numero = if (isPrime(v)) v else (nextPrime(v + 1))

      val np = nextPrime(p + 1)
      np #:: next(np)
    }

    2L #:: 3L #:: next(3)
  }

  def isPrime(n: Numero) = {
    val ret = primes.dropWhile(p => p * p <= n && n % p != 0)
    ret.head * ret.head > n
  }



  def primesOf(n: Numero) = {

    @scala.annotation.tailrec
    def extractFactor(n: Numero, f: Numero): Numero = n % f match {
      case 0 => extractFactor(n / f, f)
      case _ => n
    }

    @scala.annotation.tailrec
    def factorsOf(n: Numero, primes: Stream[Numero], factors: List[Numero]): List[Numero] = {

      if (n < primes.head)
        factors
      else {
        val nextPrimes = primes.dropWhile( n % _ != 0 )
        val p = nextPrimes.head
        factorsOf(extractFactor(n, p), nextPrimes, p :: factors)
      }
    }

    val ret = factorsOf(n, primes, Nil)
    ret
  }

  def φ(n: Numero) = {
    val primes = primesOf(n)
    val ret = if( primes.product == n ){
      // OPTIMIZATION FOR NON REPEATING FACTORS
      primes.map( _ - 1 ).product
    }
    else {
      Math.round(n * primes.map(1.0 - 1.0 / _).product).toLong
    }
    if( n % 10000 == 0 ){
      println( s"φ($n)=$ret")
    }
    ret
  }


  def comprobacion(den: Numero) = {
    def mcd( a: Numero, b: Numero ) : Numero= b match{
      case 0 => a
      case _ => mcd(b,a%b)
    }

    val mcds = for( num <- 1 until den.toInt ) yield mcd(num,den)
    val ret = mcds.count( _ == 1 )
    if( ret != φ(den) ){
      ???
    }
    if( den % 1000 == 0 ){
      println( s"comprobacion($den)=$ret")
    }
    ret
  }



  for( limit <- Seq[Numero](8, 10,1000,100000,1000000)){

    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global

    measure(){
      val solution = (limit to 2 by -1).map(φ).sum
      println( s"limit:$limit solution:$solution" )
    }

  }



}
