import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 13/01/14
 * Time: 13:07
 * To change this template use File | Settings | File Templates.
 */
object Problem5 extends App{

  /*
  2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

    What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
  */

  type Numero = Long

  val numbers = (1 to 20).map( _.toLong )

  @tailrec
  def mcd( a: Numero, b: Numero ) : Numero = a min b match{
    case n if n < 0 => throw new IllegalArgumentException( s"$a, $b" )
    case 0 => a max b
    case _ => mcd( a min b, (a max b) -(a min b) )
  }

  def mcm( a: Numero, b: Numero ) : Numero = a*b/mcd(a,b)

  val solution = numbers.fold(1L)( mcm )

  println( s"Solution:$solution")
}
