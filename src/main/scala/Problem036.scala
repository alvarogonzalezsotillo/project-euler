object Problem36 extends App{
/*
The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)
*/

  def palindrome(s:String) : Boolean = s == s.reverse
  def palindrome(base: Int*)(n: Int) : Boolean = base.forall( b => palindrome(Integer.toString(n,b)) )

  val solution = (1 until 1000000).filter( palindrome(2,10)_ ).sum
  println( s"Solution: $solution" )
}