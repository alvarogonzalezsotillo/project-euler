import scala.collection.immutable.NumericRange.Inclusive

/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 14/01/14
 * Time: 11:33
 * To change this template use File | Settings | File Templates.
 */
object Problem6 extends App{
  /*
  The sum of the squares of the first ten natural numbers is,

  1^2 + 2^2 + ... + 10^2 = 385
  The square of the sum of the first ten natural numbers is,

  (1 + 2 + ... + 10)^2 = 55^2 = 3025
  Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

  Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

   250166416500
    */

  type Numero = BigDecimal

  object Numero{
    def apply( n: Long ) = BigDecimal(n)
    def apply( n: Numero ) = n
  }

  val numbers = for( n <- 1 to 100 ) yield Numero(n)

  def sqr( n: Numero ) = n*n

  val sumOfSquares = numbers.map( sqr ).sum
  val squareOfSum = sqr( numbers.sum )
  val solution = squareOfSum - sumOfSquares
  println( s"Solution:$solution")


}
