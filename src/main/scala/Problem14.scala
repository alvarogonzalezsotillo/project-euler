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

  type Numero = Long

  class collatzSize( upTo: Int ){

    val cache = new Array[Numero](upTo*10)

    def apply(n: Int ) : Numero = apply(n.asInstanceOf[Numero])

    def apply(n : Numero ) : Numero = {

      def cs( n: Numero, accum: Numero ) : Numero = {

        if( n < cache.size && cache(n.asInstanceOf[Int]) != 0 ) cache(n.asInstanceOf[Int])+accum-1
        else n match {
          case 1           => accum
          case n if n%2==0 => cs( n/2, accum+1 )
          case n if n%2!=0 => cs( 3*n+1, accum+1 )
        }
      }
      val ret = cs(n,1)
      if( n < cache.size ) cache(n.asInstanceOf[Int]) = ret
      println( s"$n -> $ret")
      ret
    }
  }

  val cs = new collatzSize(100000)
  val c = (1 until 1000000).map( cs.apply _ )
  val solution = c.max
  println( s"Solution:$solution")

}
