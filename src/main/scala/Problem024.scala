/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 22/01/14
 * Time: 13:19
 * To change this template use File | Settings | File Templates.
 */
object Problem24 extends App{

  /*
  A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
   */

  type Numero = Long

  def fact( n: Numero ) : Numero= if( n == 1 ) 1 else n*fact(n-1)

  
  def positionsOfDigits( digitSize: Int, permutation: Numero ) = {

    
    /**
     * Digit in position, assuming it is the lowest possible digit
     */
    def poisitionOfDigits( digit: Numero ) = {
      if( digit == digitSize-1 ) 
        0
      else 
        permutation%fact(digitSize-digit) / fact(digitSize-digit-1)
    }

    val NODIGIT = -1
    val ret = Array.tabulate(digitSize)(i=>NODIGIT)
    
    for( d <- 0 until digitSize ){
      var pod = poisitionOfDigits(d)
      var index = 0
      while( pod > 0 || ret(index) != NODIGIT ){
        if( ret(index) == NODIGIT ) pod -= 1
        index += 1
      }
      ret(index) = d
    }
    
    ret
  }
  
  def lexicographicPermutation( digitSize: Int, permutation: Numero ) = {
    val pod = positionsOfDigits(digitSize,permutation)
    val ret = Array.ofDim[Int](digitSize)
    pod.zipWithIndex.foreach{ case (d,i) => ret(d)=i }
    ret
  }
  
  val solution = lexicographicPermutation(10,999999).mkString
  println( s"Solution:$solution" )
  
  val solution2 = List(0,1,2,3,4,5,6,7,8,9).permutations.drop(999999).next
  println( s"Solution2:$solution2" )
}
