/*
The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.

Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

*/
object Problem60 extends App{


  type Numero = Long

  def isPrime(n: Numero) = {
    val ret = primes.dropWhile(p => p * p <= n && n % p != 0)
    ret.head * ret.head > n
  }
  
  lazy val primes: Stream[Numero] = {
    def next(p: Numero): Stream[Numero] = {

      def isPrime(n: Numero) = {
        val ret = primes.dropWhile(p => p * p <= n && n % p != 0)
        ret.head * ret.head > n
      }

      def nextPrime(v: Numero): Numero = if (isPrime(v)) v else (nextPrime(v + 1))

      val np = nextPrime(p + 1)
      np #:: next(np)
    }

    2 #:: 3 #:: next(3)
  }

  def concat( a: Numero, b: Numero ) = (a.toString + b.toString).toLong
  def isConcatenatedPrimes(a: Numero, b: Numero) = isPrime( concat(a,b) ) && isPrime( concat(b,a) )

  import collection.mutable.{Set => MSet}
  
  var solution : Option[MSet[Numero]] = None
  val solutionSize = 5
  
  object multimap{
    private val m = collection.mutable.Map[Numero, MSet[MSet[Numero]]]()
    private def add( key: Numero, value: Numero ) = {
      val sets = m.get(key) match{
        case Some(s) => 
          s
        case None =>
          val ret = MSet[MSet[Numero]]()
          m(key) = ret
          ret
      }
      sets += MSet(value)
      for( set <- sets if set.forall( isConcatenatedPrimes(_,value) ) ){
        set += value
        if( set.size == solutionSize-1 ){
          solution = Some( set + key )
        }
      }
    }  
    
    def fill( p: Numero ){
      for( candidate <- primes.takeWhile( _ < p ) if isConcatenatedPrimes(p,candidate) ){
        add( candidate, p )
      }
    }
  }



  var p = primes
  while( solution.isEmpty ){
    val prime = p.head
    multimap.fill(prime)
    p = p.tail
  }
  
  println( s"Solution:$solution  ${solution.get.sum}" )

}