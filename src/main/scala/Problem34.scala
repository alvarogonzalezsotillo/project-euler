

object Problem34 extends App{

/*
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.

*/
  
  /*
  
    Maxima:
    
    n*9! = 10^n;
    (%o1) 362880*n=10^n
    find_root(%o1, n, 1,10);
    (%o8) 6.363456084155379
    
    
    So, max number is about 10^7
  */

  def fact( n: Int ) : Int = if( n <= 1 ) 1 else n*fact(n-1)
  val factorial = (0 to 9).map( n => (n,fact(n))).toMap
  def digits(n :Int) = n.toString.map( _ - '0')
  def curious( n: Int ) = digits(n).map(factorial).sum == n && digits(n).size > 1
  
  val max = 10000000
  
  val solution = (1 to max).filter(curious)
  
  println( s"Solution:${solution.sum}  \n${solution.mkString("\n")}" )

}