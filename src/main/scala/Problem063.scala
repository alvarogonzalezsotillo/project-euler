/*
The 5-digit number, 16807=75, is also a fifth power. Similarly, the 9-digit number, 134217728=89, is a ninth power.

How many n-digit positive integers exist which are also an nth power?
*/
object Problem63 extends App{

  def measure[T]( msg: String = "" )( proc: =>T ) = {
    println( s"-->$msg" )
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println( s"<--$msg: ${end-ini} ms" )
    ret
  }

  type Numero = BigInt
  
  implicit class NumeroOps( n: Numero ){
    def **(e: Numero) : Numero = if( e == 0) 1 else n * ( n**(e-1) )
    def digitsSize : Numero = if(n/10==0) 1 else 1 + (n/10).digitsSize
  }
  
  def basesFor( n: Numero ) = {
    Iterator.from(1).
             map( BigInt(_) ).
             dropWhile( i => (i**n).digitsSize < n ).
             takeWhile( i => (i**n).digitsSize == n ).
             toList
  }

  measure(){
    val s = Iterator.from(1).
                     map( n => basesFor(n) ).
                     takeWhile( _.size > 0 ).
                     toList
                     
    s.zipWithIndex.foreach{ b => 
      val exp = b._2+1
      println( b )
      b._1.foreach( n => println( s"$n^$exp = ${n**exp}" ) )
    }
    
    println( s )
    val solution = s.flatten.size
    println( s"Solution:$solution" )
  }
}