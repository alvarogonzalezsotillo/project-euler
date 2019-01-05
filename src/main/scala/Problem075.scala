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
    //assert(a*a + b*b == c*c)
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


  def DFS[T]( first: T, next: (T)=>Seq[T] ) : Stream[T] = {
    def nextStream( t: T ) : Stream[T] = {
      val n = next(t).toStream
      n #::: n.map(nextStream).flatten
    }
    first #:: nextStream(first)
  }

  def BFS[T]( first: T, next: (T)=>Seq[T] ) : Stream[T] = {
    def nextStream( queue: List[T] ) : Stream[T] = {
      if( queue.size == 0 ){
        Stream.empty
      }
      else{
        val (head,tail) = (queue.head,queue.tail)
        val nextT = next(head).toList
        head #:: nextStream( tail ++ nextT )
      }
    }
    nextStream( List(first) )
  }

  val limit = 1500000

  def adhoc() = {
    val siege = new Array[Int](limit+1)

    def next( p: Pythagorean ) : Unit = {
      if( p.size > limit ){
        return
      }
      var i = p.size
      while( i <= limit ){
        siege(i) += 1
        i += p.size
      }
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
      next(A)
      next(B)
      next(C)
    }

    next(FirstPythagorean)
    var ret = 0
    var i = 0
    while( i <= limit ){
      if( siege(i) == 1 ){
        ret += 1
      }
      i += 1
    }
    ret
  }

  def solution( ps: Stream[Pythagorean] ) = {

    val siege = new Array[Int](limit+1)

    for( p <- ps ; i <- p.size to limit by p.size ){
      siege(i) += 1
    }

    siege.count(_==1)
  }

  measure(){
    val s = solution( DFS(FirstPythagorean, (p:Pythagorean) => nextPythagoreans(p).filter(_.size <= limit) ) )
    println( s"solution DFS: $s") //161667 815ms
  }

  measure(){
    val s = solution( BFS(FirstPythagorean, (p:Pythagorean) => nextPythagoreans(p).filter(_.size <= limit) ) )
    println( s"solution BFS: $s") //161667 7995ms
    
  }

  measure(){
    val s = adhoc
    println( s"solution adhoc: $s") //161667 125ms
    
  }


}
