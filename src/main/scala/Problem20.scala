/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 20/01/14
 * Time: 11:32
 * To change this template use File | Settings | File Templates.
 */
object Problem20 extends App {
  /*
  n! means n × (n − 1) × ... × 3 × 2 × 1

  For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
  and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

  Find the sum of the digits in the number 100!
  */
  type Numero = BigInt

  def factNoExtraZeroes(n: Numero): Numero = {
    def stripFinalZeroes(n: Numero) = BigInt(n.toString.reverse.dropWhile(_ == '0').reverse)
    if (n == 1) {
      1
    }
    else {
      val ret = factNoExtraZeroes(n - 1) * n
      stripFinalZeroes(ret)
    }
  }

  def digits(n: Numero) = n.toString.map(_.toInt - '0')

  val solution = digits(factNoExtraZeroes(1000)).sum
  println(s"Solution:$solution")

  for( n <- 1 to 100 ) println( s"$n-> ${digits(factNoExtraZeroes(n)).sum}")
}
