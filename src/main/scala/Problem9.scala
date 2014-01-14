/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 14/01/14
 * Time: 12:32
 * To change this template use File | Settings | File Templates.
 */
object Problem9 extends App {

  /*
  A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
   */

  type Numero = Long

  def pythagoreanTriplets(n: Numero) = {
    def isPythagorean(a: Numero, b: Numero, c: Numero) = a * a + b * b == c * c

    for (a <- 1L until n;
         b <- a + 1 until n;
         c = (n - a - b)
         if isPythagorean(a, b, c) ) yield Seq(a, b, c)
  }

  val solution = pythagoreanTriplets(1000).head.product

  println(s"Solution:$solution")

}
