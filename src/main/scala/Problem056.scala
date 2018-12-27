
/*
A googol (10100) is a massive number: one followed by one-hundred zeros; 100100 is almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.

Considering natural numbers of the form, ab, where a, b < 100, what is the maximum digital sum?
*/
object Problem56 extends App{

 def measure[T]( msg: String = "" )( proc: =>T ) = {
    println( s"-->$msg" )
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println( s"<--$msg: ${end-ini} ms" )
    ret
  }
 

  type Numero = BigInt
  
  def sumsOfDigits( n: Numero, upToExp: Int ) = {
    var power = BigInt(1) 
    for( i <- 1 to upToExp ) yield {
      power *= n
      power.toString.map( _ - '0' ).sum
    }
  }
 
  measure(){
    val sums = (1 to 100).flatMap( sumsOfDigits(_,100) )
    val solution = sums.max
    println( s"Solution:$solution" )
  }

}