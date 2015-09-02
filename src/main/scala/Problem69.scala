object Problem69 extends App{

 def measure[T](proc: => T) = {
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println(s"${end - ini} ms")
    ret
  }

  type Numero = Int
  type Flotante = Double



  def phi_lammer(n:Numero) = {
    def relativePrime(a:Numero,b:Numero) = mcd(a,b) == 1
    def mcd(a:Numero, b:Numero) = {
      @scala.annotation.tailrec
      def mcd_0(a:Numero, b:Numero) : Numero = {
        if(b==0) a
        else mcd_0(b, a%b)
      }
      mcd_0(a max b, a min b)
    }
    (1 until n).count(relativePrime(n,_))
  }

  def primesOf(n:Numero) = {

    def log(s:String) = {
      //println(s)
    }

    @scala.annotation.tailrec
    def extractFactor(n:Numero,f:Numero): Numero = n%f match{
      case 0 => extractFactor(n/f,f)
      case _ => n
    }

    @scala.annotation.tailrec
    def primesOf_0(n:Numero, previousPrime: Numero, primes:List[Numero] ): List[Numero] = {

      if( n <= previousPrime ) primes
      else{
        Iterator.from(previousPrime+1).find( n % _ == 0 ) match{
          case Some(p) => primesOf_0( extractFactor(n,p), p, p::primes )
          case None => throw new IllegalStateException(s"cant find a prime factor of $n bigger than $previousPrime" )
        }
      }
    }
    val ret = primesOf_0(n,1,Nil)
    log( s"n:$n primes:$ret" )
    ret
  }

  def phi_hacker(n:Numero) = {
    /*
     Solution:510510
     681526 ms
     */
    val primes = primesOf(n)
    (n*primes.map( 1.0 - 1.0/_).product).toInt
  }

  def comparePhi = {
    (2 to 100).map( n => (n, phi_hacker(n),phi_lammer(n)) ).find( p => p._2 - p._3 != 0 ).foreach( p => throw new IllegalStateException(p.toString) )
  }

  def criteria(phiCalculator:(Numero)=>Numero)(n:Numero): Flotante = {
    val p = phiCalculator(n)
    val ret = 1.0*n/p
    if( n%1000 == 0 ) println( s"$n\t phi:$p\t ret:$ret")
    ret
  }

  def findLargestPrimeEqualOrLess(n:Numero) = {
    def isPrime(n:Numero) = (2 to n/2+1).find(n%_ == 0).isEmpty
    (n to 1 by -1).find(isPrime).get
  }

  measure{
    println( criteria(phi_hacker)(510510) )
    println( phi_hacker(510510) )
    println( phi_lammer(510510) )
    println( primesOf(510510))
    println( findLargestPrimeEqualOrLess(1000000))
    println( criteria(phi_hacker)(findLargestPrimeEqualOrLess(1000000)))
    comparePhi
    val solution = (2 to 1000000).maxBy( criteria(phi_hacker) )
    println( s"Solution:$solution" )
  }

}
