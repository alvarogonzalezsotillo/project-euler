object Problem32 extends App{

/*
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
*/

  def arePandigital( nums: Int* ) = {
    val digits = nums.map( n => n.toString ).flatten
    !digits.contains('0') && digits.size == 9 && digits.toSet.size == 9
  }

  val digits = (1 to 9).toSet
  

  for( numDigitsA <- 1 to 5 ; 
       numDigitsB = 9 - numDigitsA ;
       a <- digits.combinations(numDigitsA).map( _.permutations ).flatten ;
       b <- (digits - a).map( _.permutations ).flatten 
       
    
  val solution = pandigitalProducts.toSet.sum
  println( s"Solution:$solution" )  
  
}