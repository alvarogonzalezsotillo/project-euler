

object Problem34 extends App{

/*
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.

*/

  def fact( n: Int ) : Int = if( n <= 1 ) 1 else n*fact(n-1)
  val factorial = (0 to 9).map( n => (n,fact(n))).toMap
  def digits(n :Int) = n.toString.map( _ - '0')
  def curious( n: Int ) = digits(n).map(factorial).sum == n
  
  val max = 100000000
  
  val solution = (1 to max).filter(curious)
  
  println( s"Solution:${solution.sum}  \n${solution.mkString("\n")}" )

}