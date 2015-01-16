/*



Consider quadratic Diophantine equations of the form:

x2 – Dy2 = 1

For example, when D=13, the minimal solution in x is 6492 – 13×1802 = 1.

It can be assumed that there are no solutions in positive integers when D is square.

By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:

32 – 2×22 = 1
22 – 3×12 = 1
92 – 5×42 = 1
52 – 6×22 = 1
82 – 7×32 = 1

Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.

Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.


*/

object Problem66 extends App {


  def measure[T](proc: => T) = {
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println(s"${end - ini} ms")
    ret
  }

  type Numero = Int

  def findMinimalFor(d: Numero) : (Numero,Numero) = {
    val solutions = for( x <- Iterator.from(1) ; y <- (1 to x*x/d).view if x*x - d*y*y == 1 ) yield (x,y)
    solutions.head
  }

  measure{
    def values = Iterator.from(1).takeWhile( _ <= 1000 ).toSeq
    val squares = values.map( d => d*d )
    val dvalues = values filterNot squares.contains 

    val minimals = dvalues zip dvalues.map( findMinimalFor )

    println( dvalues.mkString(",") )
    println( minimals.mkString(",") )
  }
}
