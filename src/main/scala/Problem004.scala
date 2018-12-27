object Problem4 extends App{
  /*
  A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
   */

  type Numero = Int

  def isPalindrome(n: Numero) = n.toString == n.toString.reverse

  val numbers = 100 to 999
  def x[T,U]( s1: Seq[T], s2:Seq[U] ) = for( v1 <- s1; v2 <- s2 ) yield (v1,v2)

  val solution = x(numbers,numbers).map( p => p._1 * p._2 ).filter( isPalindrome ).max

  println( s"Solution: $solution")
}
