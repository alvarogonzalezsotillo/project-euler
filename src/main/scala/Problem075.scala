/*


 1 -2 2
 2 -1 2
 2 -2 3

 1 2 2
 2 1 2
 2 2 3

 -1 2 2
 -2 1 2
 -2 2 3



 */
object Problem75 extends App {

  def measure[T](message: String = "")(proc: => T): T = {
    println(s"$message")
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println(s"$message ${end - ini} ms")
    ret
  }

  def log(msg: =>String) = {
    //println(msg)
  }



  type Numero = Int
  type Flotante = Double

  case class Pythagorean( a: Numero, b: Numero, c: Numero ){
    assert(a*a + b*b == c*c)
    val size = a+b+c
    override def toString = s"($size) $a $b $c"
  }

  object FirstPythagorean extends Pythagorean(3,4,5)

  def nextPythagoreans( p: Pythagorean ) = {
    import p._

    val A = Pythagorean(
      1*a - 2*b + 2*c,
      2*a - 1*b + 2*c,
      2*a - 2*b + 3*c
    )

    val B = Pythagorean(
      1*a + 2*b + 2*c,
      2*a + 1*b + 2*c,
      2*a + 2*b + 3*c
    )

    val C = Pythagorean(
      -1*a + 2*b + 2*c,
      -2*a + 1*b + 2*c,
      -2*a + 2*b + 3*c
    )

    Seq(A,B,C)
  }

  def allPythagoreans( filterp: (Pythagorean) => Boolean ) : Stream[Pythagorean] = {

    def next( p: Pythagorean ) : Stream[Pythagorean] = {
      val n = nextPythagoreans(p).filter(filterp).toStream
      n #::: n.map(next).flatten
    }

    FirstPythagorean #:: next(FirstPythagorean)
  }


  measure(){
    val limit = 1500000

    val ps = allPythagoreans( _.size <= limit )

    val siege = new Array[Int](limit+1)

    for( p <- ps ; i <- p.size to limit by p.size ){
      siege(i) += 1
    }

    val solution = siege.count(_==1)

    println( s"solution: $solution") //161667
    
  }


}
