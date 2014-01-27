import scala.io.Source

/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 21/01/14
 * Time: 16:47
 * To change this template use File | Settings | File Templates.
 */
object Problem22 extends App {
           /*
           Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
            */
  val input = Source.fromFile("src/main/scala/names.txt").getLines().next
  val names = input.replace("\"","").split(",").sorted
  def nameValue(n:String) = n.sum - n.size*('A'-1)
  val solution = names.zipWithIndex.map{
    case (n,i) => nameValue(n)*(i+1)
  }.sum
  println( s"Solution:$solution")
}
