import scala.annotation.tailrec

object Problem26 extends App {

  /*


A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

    1/2	= 	0.5
    1/3	= 	0.(3)
    1/4	= 	0.25
    1/5	= 	0.2
    1/6	= 	0.1(6)
    1/7	= 	0.(142857)
    1/8	= 	0.125
    1/9	= 	0.(1)
    1/10	= 	0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
  */

  def recurringCycleSize( n : Int ) = {
    
    @tailrec
    def remainderIterate( remainder: Int, remainders: Set[Int] ) : Int = {
      val next = 10*remainder / n
      val nextRemainder = (10*remainder) % n
      if( nextRemainder == 0 ) 0
      else if( remainders contains nextRemainder ) remainders.size
      else remainderIterate( nextRemainder, remainders + nextRemainder) 
    }
    
    remainderIterate( 1, Set() )
  }

  val maxNumber = 1000
  
  // IN ANY DIVISION, THE MAX LENGTH OF THE CYCLE IS THE DENOMINATOR MINUS 1
  val solution = (1 to 1000).reverse.find( n => n == recurringCycleSize(n)+1 )

  println(s"Solution:$solution")
}
