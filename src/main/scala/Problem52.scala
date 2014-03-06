
object Problem52 extends App{
/*


It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

*/


/*
- The number must start with 1
- The second digit is in [0,6] (17abcd x 6 = a number with more digits)
- The rest of the digits include 0,1,2,3,4,5,6 (since 1abc x 2 = 2def, ... 1abc x 6 = 6def
- The number has at least 6 digits
*/

  def measure[T]( msg: String = "" )( proc: =>T ) = {
    println( s"-->$msg" )
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println( s"<--$msg: ${end-ini} ms" )
    ret
  }

  type Numero = Int
  
  val starts = (1 to 6).map( d => s"1${d}" ).toSet
  val sureDigits = (1 to 6).map( _.toString )
  
  
  def isSolution( n: Numero ) = {
    val ns = digits( n )
    
    if( n % 1000000 == 0 ) println( s"...$ns" )
   
    def sameDigits( s1: Array[Int], s2: Array[Int] ) = s1.forall( c => s2.contains(c) ) && s2.forall( c => s1.contains(c) )
    
    def check( i: Numero = 2 ) : Boolean = {
      if( i == 7 ) 
        true
      else{
        val s = digits( n*i )
        sameDigits(ns,s) && check(i+1)
      }
    }
    
    check()
  }
  

  def digits(n : Numero, array: Array[Int] = null ) = {
    val size = digitsSize(n)
    val ret = if( array == null || array.size != size ) new Array[Int](size) else array
    var remainder = n
    for( i <- 0 until size ){
      ret(size-i-1) = remainder
      remainder /= 10
    }
    ret
  }

  def digitsSize( n: Numero ) : Int = {
    if( n < 10 ) 1
    else 1 + digitsSize(n/10)
  }

  def pow( n: Numero, exp: Int ) : Numero = if( exp == 0 ) 1 else n*pow(n,exp-1)
  
  def nextCandidate( n: Numero ) = {
   
    def firstDigits( n: Numero, digits: Int ) = {
      val div = pow(10,digitsSize(n)-digits)
      n / div
    }
    
    val next = n+1
    
    if( firstDigits(next,2) > 16 ){
      val ret = pow(10,digitsSize(next))
      println( s"jump from $next to $ret" )
      ret
    }
    else{
      next
    }
  }

  measure(){
    val candidates = Iterator.iterate(100000)(nextCandidate).filter(isSolution)
    val solution = candidates.next
    println( s"Solution:$solution  ${(2 to 6).map(_*solution).toList}" )
  }


}