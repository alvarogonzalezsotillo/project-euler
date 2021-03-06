/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 12/02/14
 * Time: 14:39
 * To change this template use File | Settings | File Templates.
 */
object Problem45 extends App{

  /*
  Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:

    Triangle	 	Tn=n(n+1)/2	 	1, 3, 6, 10, 15, ...
  Pentagonal	 	Pn=n(3n−1)/2	 	1, 5, 12, 22, 35, ...
  Hexagonal	 	Hn=n(2n−1)	 	1, 6, 15, 28, 45, ...
  It can be verified that T285 = P165 = H143 = 40755.

  Find the next triangle number that is also pentagonal and hexagonal.
  */

  type Numero = Long
  def p(i:Numero) = i*(3*i-1)/2
  def h(i:Numero) = i*(2*i-1)
  def findNext( ip: Numero, ih: Numero ) : (Numero,Numero) = {
    val pn = p(ip)
    val hn = h(ih)
    if( pn == hn ) (ip,ih)
    else if( pn < hn ) findNext(ip+1,ih)
    else findNext(ip,ih+1)
  }

  val first = findNext(2,2)
  val second = findNext( first._1+1, first._2+1 )
  val solution = p(second._1)
  println( s"Solution:$solution")
}
