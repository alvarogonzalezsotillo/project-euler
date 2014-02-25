/*
By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.

*/
object Problem51 extends App{

  def measure[T]( msg: String )( proc: =>T ) = {
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

  measure("test"){
    Iterator.from(12345678).takeWhile(_<12349999).foreach( n => println( s"$n -> ${isPrime(n)}" ) )
  }
}