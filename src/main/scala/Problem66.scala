import scala.annotation.tailrec

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

  def it( ini: Numero ) : Iterator[Numero] = Iterator.iterate(ini)( (n: Numero) => n+1 )
  def it( ini: Numero, end: Numero ) : Iterator[Numero] = it(ini).takeWhile( _ <= end )

  def findMinimalFor_slow(max:Numero)(d: Numero) : Option[(Numero,Numero)] = {
    def sqrt( n: Numero ) = {
      @tailrec
      def _sqrt( candidate: Numero ) : Numero = (candidate + n/candidate)/2 match{
        case c if c*c <= n && (c+1)*(c+1) > n => c
        case c => _sqrt(c)
      }
      _sqrt(1 + n/2)
    }
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

  def findMinimalFor( max: Numero )( d: Numero ) : Option[(Numero,Numero)] = {

    implicit class Fraction( v:(Numero,Numero) ){
      val a = v._1
      val b = v._2
      val value = a.toDouble / b.toDouble
    }

    def convergentsOf( d : Numero ) = {
      val cf = Problem64.continuedFraction(d.toInt)
      val dfbi = cf.map{ case (b,remainder) => b }.map( BigInt(_) )
      lazy val convergents : Stream[(Numero,Numero)]= dfbi.zipWithIndex.map{
        case (b,i) if i == 0 => (b,BigInt(1))
        case (b,i) if i == 1 => (b*dfbi(0)+1, b)
        case (b,i) =>
          val Ai = b*convergents(i-1).a + convergents(i-2).a
          val Bi = b*convergents(i-1).b + convergents(i-2).b
          (Ai,Bi)
      }
      convergents
    }

    convergentsOf(d).find( c => c.a*c.a - d*c.b*c.b == 1 )
  }

  measure{

    val limit = 1000L
    def values = it(1,limit).toIndexedSeq
    val squares = values.map( d => d*d )
    val dvalues = (values filterNot squares.contains)

    val minimals = dvalues zip dvalues.map( findMinimalFor(1000000000) )

    val candidates = minimals.filter{ case (d,option) => option.isEmpty }.toArray
    println( "Candidates\n:" + candidates.mkString("\n") )

    val solution = minimals.maxBy{ 
      case (d,Some( (x,y) ) ) => x 
      case (_,None) => throw new IllegalStateException
    }._1

    println( s"Solution: $solution" )
  }
}
