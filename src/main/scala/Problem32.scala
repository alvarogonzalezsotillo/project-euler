object Problem32 extends App{

/*
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
*/
  
  /*
   
    log ab = log a + log b
    
    log ab + log a + log b = 9
    
    log ab = 4.5
    
    0 < log a < 4.5 
    
    log b = 4.5 - log a
   
   */

  def pow(b: Int, exp: Int) : Int = if(exp<=0) 1 else b*pow(b,exp-1)
  def numDigits(n: Int) = n.toString.size
  
  val digits = 1 to 9
  val maxA = Math.sqrt( pow(10,4) ).toInt + 1
  
  def arePandigital( nums: Int* ) = {
    val d = nums.map( n => n.toString ).flatten
    !d.contains('0') && d.size == 9 && d.toSet.size == 9
  }


  val pandigitalProducts = for( a <- 1 to maxA ;
       maxB = pow(10, 5-numDigits(a));
       b <- 1 to maxB if( arePandigital(a,b,a*b) ) ) yield (a,b,a*b)
  
  
  val solution = pandigitalProducts.map( _._3 ).toSet.sum
  println( s"Solution:$solution \n ${pandigitalProducts.mkString("\n")}" )  
  
}