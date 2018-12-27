object Problem1 extends App {
  /*
  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

  Find the sum of all the multiples of 3 or 5 below 1000.
   */

  def isMultipleOf3Or5(n:Int) = n%3 == 0 || n%5 == 0

  val numbers = 1 until 10000
  val multiplesOf3Or5 = numbers.filter( isMultipleOf3Or5 )
  val solution = multiplesOf3Or5.sum
  println( s"Solution: $solution")
}
