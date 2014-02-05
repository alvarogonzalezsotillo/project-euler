object Problem38 extends App{

/*
Take the number 192 and multiply it by each of 1, 2, and 3:

192 × 1 = 192
192 × 2 = 384
192 × 3 = 576
By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

*/

  def isPandigital(n: Int) = {
    val s = n.toString.toSet
    s.size == 9 && !s.contains('0')
  }
  
  def concatenatedProduct(n: Int) : Stream[Int] = {
    def concat( previous: Int, m: Int ) : Stream[Int] = {
      val next = n*m
      (previous.toString + next.toString).toInt #:: concat( next, m+1)
    }
    n #:: concat(n, 2)
  }
  
  def isPandigitalGenerator(n: Int) = concatenatedProduct(n).takeWhile(_<1000000000).exists(isPandigital)
  
  val pandigitals = Stream.from(1).filter(isPandigitalGenerator).takeWhile(_!=987654321)
  
  println( s"$pandigitals" )  
}