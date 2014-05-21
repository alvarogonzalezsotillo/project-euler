/*
The 5-digit number, 16807=75, is also a fifth power. Similarly, the 9-digit number, 134217728=89, is a ninth power.

How many n-digit positive integers exist which are also an nth power?
*/
object Problem63 extends App{

  type Numero = Long
  
  implicit class NumeroOps( n: Numero ){
    def **(e: Numero) : Numero = if( e == 0) 1 else n * ( n**(e-1) )
    def digitsSize : Numero = if(n/10==0) 1 else 1 + (n/10).digitsSize
  }
  
  def basesFor( n: Numero ) = {
    Iterator.from(1).
             map(_.toLong).
             dropWhile( (_**n).digitsSize < n )
             takeWhile( (_**n).digitsSize == n ).
             toList
  }
  
  val s = (1 to 30).map( n => basesFor(n) )
  println( s.zipWithIndex.mkString("\n") )
}