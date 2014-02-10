import scala.collection.immutable.Stream.consWrapper

object Problem40 extends App {
  /*
   An irrational decimal fraction is created by concatenating the positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If dn represents the nth digit of the fractional part, find the value of the following expression.

d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
   */
  
  val digits = {
    def next(n: Int) : Stream[Int] = n.toString.map(_-'0').toStream #::: next(n+1)
    0 #:: next(1)
  }
  val solution = Seq(1,10,100,1000,10000,100000,1000000).map(digits).product 
  println( s"Solution:$solution" )
}