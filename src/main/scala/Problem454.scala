import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 16/01/14
 * Time: 11:47
 * To change this template use File | Settings | File Templates.
 */
object Problem454 extends App{
  type Numero = BigDecimal

  val n = BigDecimal(1000000000000L)
  val m = BigDecimal(999999999999L)
  val nm = n*m
  val f = nm / (n+m)

  println( s"$nm  ${n+m}  $f")
}
