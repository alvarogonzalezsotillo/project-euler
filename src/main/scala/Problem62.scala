/*
The cube, 41063625 (3453), can be permuted to produce two other cubes: 56623104 (3843) and 66430125 (4053). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are cube.
*/

object Problem62 extends App{

  type Numero = BigInt
  implicit class NumeroOps( n: Int ){
    def toNumero = BigInt(n)
  }
  
  def cubesOfDigits( d: Int ) = {
    def digits( n: Numero ) : Int = {
      val next = n/10
      if( next == 0 )
        1
      else
        1 + digits(next)
    }
  
    Iterator.from(1).
             map( _.toNumero ).
             map( n => n*n*n ).
             dropWhile( digits(_) < d ).
             takeWhile( digits(_) == d )
  }
  
  for( d <- Iterator.from(1).take(4+
  .+-
  0) ){
    val c = cubesOfDigits(d)
    println( s"cubes of $d digits: ${c.size}" )
  }
}