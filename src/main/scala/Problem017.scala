/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 17/01/14
 * Time: 23:48
 * To change this template use File | Settings | File Templates.
 */
object Problem17 extends App {

  /*
  If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
   */

  val numbers = Map(
    1 -> "one",
    2 -> "two",
    3 -> "three",
    4 -> "four",
    5 -> "five",
    6 -> "six",
    7 -> "seven",
    8 -> "eight" ,
    9 -> "nine",
    10 -> "ten",
    11 -> "eleven",
    12 -> "twelve",
    13 -> "thirteen",
    14 -> "fourteen",
    15 -> "fifteen",
    16 -> "sixteen",
    17 -> "seventeen",
    18 -> "eighteen",
    19 -> "nineteen",
    20 -> "twenty",
    30 -> "thirty",
    40 -> "forty",
    50 -> "fifty",
    60 -> "sixty",
    70 -> "seventy",
    80 -> "eighty",
    90 -> "ninety"
  )

  val multipliers = Map( 100 -> "hundred", 1000 -> "thousand")

  def toLetters( n: Int ) = {
    assert( n < 10000 )
    var remainder = n
    var ret = ""

    multipliers.keys.toArray.sorted.reverse.foreach( m => {
      val f = remainder / m
      if( f > 0 ){
        ret += numbers(f) + multipliers(m)
        remainder -= f*m
      }
    })

    if( ret != "" && remainder > 0 ) ret += "and"

    numbers.keys.toArray.sorted.reverse.foreach( m => {
      val f = remainder - m
      if( f >= 0 ){
        ret +=  numbers(m)
        remainder -= m
      }
    })

    ret
  }


  val strings = (1 to 1000).map( toLetters )
  println( strings.mkString("\n"))
  val solution = strings.foldLeft(0)( (n,s) => n + s.size )
  println( s"Solution:$solution")
}
