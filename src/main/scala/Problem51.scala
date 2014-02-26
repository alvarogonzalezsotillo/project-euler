/*
By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.

*/
object Problem51 extends App{

  def measure[T]( msg: String = "" )( proc: =>T ) = {
    println( s"-->$msg" )
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println( s"<--$msg: ${end-ini} ms" )
    ret
  }

  type Numero = Long

  def isPrime( n: Numero ) = {
  
    def isqrt( n: Numero, candidate: Numero = 1 ) : Numero = {
      if( candidate*candidate <= n && (candidate+1)*(candidate+1) > n )
        candidate
      else
        isqrt(n,(n/candidate + candidate)/2)
    }
  
    val sqrt = isqrt(n)
    Iterator.from(2).takeWhile(_<=sqrt).find( p => n%p == 0 ).isEmpty
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

  def check( digitSize: Int, familySize: Int ) = measure( s"check digitSize:$digitSize  familySize:$familySize" ){

    val min = ("0"+"9"*(digitSize-1)).toLong + 1
    val max = ("9"*digitSize).toLong

  
    def checkPrime( prime: Numero, changeSize: Numero ) = {
    
      val masks = (0 until digitSize).combinations(changeSize.toInt)
      
      def applyMask( mask: Seq[Int], digit: Int ) = {
        val digits : Array[Int]= prime.toString.map(_.toInt-'0').toArray
        mask.foreach( d => digits(d)=digit )
        digits.mkString.toLong
      }
      
      def familyOfMask( mask: Seq[Int] ) = {
        val fullFamily = for( d <- 0 to 9 ) yield applyMask(mask,d)
        fullFamily.filter(isPrime).filter(_.toString.size == digitSize)
      }
      
      val validFamily = masks.map( familyOfMask ).find( _.size >= familySize )
      if( !validFamily.isEmpty ){
        println( s"    validFamily:$validFamily" )
      }
      !validFamily.isEmpty
    }

    def primesToCheck = primes.dropWhile(_<min).takeWhile(_<=max)
    
    def checks = for( prime <- primesToCheck; 
                      changeSize <- (1 until digitSize).view ) yield{
      checkPrime( prime, changeSize )
    }
    
    checks.exists(b=>b)
    
  }  

  measure("test"){
    val checks = for( digitSize <- (1 to 10).view ) yield check( digitSize, 8 )
    checks.find( b => b )
  }
}