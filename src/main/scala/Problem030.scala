object Problem30 extends App {
  /*
Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

1634 = 14 + 64 + 34 + 44
8208 = 84 + 24 + 04 + 84
9474 = 94 + 44 + 74 + 44
As 1 = 14 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
*/

  /*
   
   With n digits, the maximum number is below 10^n, and the maximum sum is n*9^5.
   The sum grows in a lineal way and the number in an exponential way.
   The intersection of the two functions give the maximum number of digits, n
   
   10^n = n*9^5
   -->  n log 10 = log n + 5 log 9
   -->  n = (log n + 5 log 9)/log 10
   -->  n ~= 5 log 9 / log 10 = 4,7712125471966243729502790325512
   
   So, the maximum number of digits is about 5, or maybe 6 (exponent + 1)
   
   
   */

  val exp = 5
  val numDigits = exp + 1

  def pow(base: Int, exp: Int): Int = if (exp <= 0) 1 else base * pow(base, exp - 1)
  val power = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).map(d => (d, pow(d, exp))).toMap
  def digits(n: Int) = n.toString.map(_ - '0')
  def isSumOfItsPowers(n: Int) = digits(n).map(power).sum == n
  val solution = (2 until pow(10, numDigits)).filter(isSumOfItsPowers)
  println(s"Solution: ${solution.mkString(",")} ${solution.sum}")

}