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

  type Numero = BigInt

  def toNumero( d: Double ) = BigInt( "%1.0f".format( d ) )
  def sqrt( n: Numero ) = toNumero( Math.sqrt(n.toDouble) )
  def it( ini: Numero ) : Iterator[Numero] = Iterator.iterate(ini)( (n: Numero) => n+1 )
  def it( ini: Numero, end: Numero ) : Iterator[Numero] = it(ini).takeWhile( _ <= end )


  def findMinimalFor(d: Numero) : (Numero,Numero) = {
    val solutions = for( x <- it(1) ;
      y = sqrt((x*x-1)/d) if( y >= 1 );
      v = x*x - d*y*y ;
      _ = if( x%100000000 == 0 ) println( s"    v:$v  x:$x  d:$d  y:$y" ) else () if v == 1 ) yield (x,y)
    
    val ret = solutions.next
    println( s"d:$d -> $ret" )
    ret
  }

  def findMinimalFor_fast(max:Numero)(d: Numero) : Option[(Numero,Numero)] = {
    var x : Numero = 0
    var y : Numero = 0
    while( y < max && (y == 0 || x*x - d*y*y != 1) ){
      y += 1
      val newx = sqrt( 1 + y*y*d )
      x = newx max x+1
      if( y%1000000  == 0 ) println( s"  x:$x  d:$d  y:$y" )
    }
    val ret = if( x < max ) Some((x,y)) else None
    println( s"d:$d -> $ret" )
    ret // ensuring (_ == findMinimalFor(d) )
  }

  measure{
    val limit = 1000L
    def values = it(1,limit).toSeq
    val squares = values.map( d => d*d )
    val dvalues = values filterNot squares.contains 

    val minimals = dvalues zip dvalues.map( findMinimalFor_fast(10000000) )

    val candidates = minimals.filter{ case (d,option) => option.isEmpty }
    println( "Candidates\n:" + candidates.mkString("\n") )

    val solution = minimals.maxBy{ case (d,Some( (x,y) ) ) => x }._1

    println( s"Solution: $solution" )
  }
}
