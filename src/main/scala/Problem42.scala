import scala.io.Source

/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 10/02/14
 * Time: 10:01
 * To change this template use File | Settings | File Templates.
 */
object Problem42 extends App {

  /*
  The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?
   */

  val words = Source.fromFile("src/main/scala/words.txt").
                     getLines.
                     next.
                     replaceAll("\"","").
                     split(",")

  val wordsNumber = words.map( w => w.toUpperCase.sum - w.size*('A'-1) )
  val triangulars = {
    def t(n: Int ) : Stream[Int] = (n*(n+1)/2) #:: t(n+1)
    1 #:: t(2)
  }
  def isTriangular(t: Int) = triangulars.takeWhile(_<=t).last == t

  val solution = wordsNumber.filter(isTriangular).size
  println( s"Solution:$solution")
}
