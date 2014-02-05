import scala.annotation.tailrec

object Problem33 extends App {

  /*
The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
*/

  def digits(n: Int) = n.toString.map(_ - '0')

  def mcd(a: Int, b: Int): Int = if (a == 0) b else mcd(b % a, a)

  class Fraction(val num: Int, val den: Int) extends Pair(num, den) {
    def sameValueAs(f: Fraction) = f.den * num == f.num * den

    def simplifyThatWay = {
      val dn = digits(num)
      val dd = digits(den)
      for (a <- 0 to 1; b <- 0 to 1 if (dn(a) == dd(b) && dn(a) != 0)) yield Fraction(dn(1 - a), dd(1 - b))
    }

    def simplify = mcd(num, den) match {
      case 1 => this
      case m => Fraction(num / m, den / m)
    }

    def isCurious() = simplifyThatWay.exists(sameValueAs)

    def *(f: Fraction) = Fraction(f.num * num, f.den * den)
  }

  object Fraction {
    def apply(num: Int, den: Int) = new Fraction(num, den)
  }

  val curious = for (
    denominator <- 10 to 99;
    numerator <- 10 until denominator;
    f = Fraction(numerator, denominator) if (f.isCurious)
  ) yield f

  val product = curious.foldLeft(Fraction(1, 1))((a, f) => a * f)

  println(s"Solution: $product  ${product.simplify}")

}