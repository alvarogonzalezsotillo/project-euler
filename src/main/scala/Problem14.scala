/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 17/01/14
 * Time: 12:26
 * To change this template use File | Settings | File Templates.
 */
object Problem14 extends App {

  /*
  The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
   */

  type Numero = BigDecimal

  implicit def toNumero( l: Long ) = BigDecimal(l)
  implicit def toNumero( l: Int ) = BigDecimal(l)
  implicit def toInt( n: Numero ) = n.toInt
  implicit def toLong( n: Numero ) = n.toLong

  object Numero{
    def apply(l: Long) = toNumero(l)
    def apply(l: Int) = toNumero(l)
    def unapply(n: Numero) = Some(toLong(n))
  }


  class collatzSize( upTo: Int ){

    val cache = Array.tabulate(upTo*4)( i => Numero(0) )


    def apply(n : Numero ) : Numero = {

      def cs( n: Numero, accum: Numero ) : Numero = {

        if( n < cache.size && cache(n) != 0 )
          cache(n)+accum-1
        else n match {
          case Numero(1)   => accum
          case n if n%2==0 => cs( n/2, accum+1 )
          case n if n%2!=0 => cs( n*3+1, accum+1 )
        }
      }
      val ret = cs(n,1)
      if( n < cache.size ) cache(n) = ret
      println( s"$n -> $ret")
      ret
    }
  }

  val cs = new collatzSize(1000000)
  val c = (1 until 1000000).map( i => (i,cs( Numero(i) ) ) )
  val solution = c.maxBy( _._2 )._1
  println( s"Solution:$solution")

}
