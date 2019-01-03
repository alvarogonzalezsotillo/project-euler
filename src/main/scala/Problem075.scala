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

  class Pytagorean( a: Numero, b: Numero, c: Numero ){
    assert(a*a == b*b + c*c)
  }

  def nextPythagoreans( p: Pytagorean ) = {
    import p._

    val A = Pytagorean(
        a - 2*b + 2*c,
      2*a -   b + 2*c,
      2*a - 2*b + 3*c
    )

    val B = Pytagorean(
        a + 2*b + 2*c,
      2*a +   b + 2*c,
      2*a + 2*b + 3*c
    )

    val C = Pytagorean(
      -a + 2*b + 2*c,
      -2*a + b + 2*c,
      -2*a + 2*b + 3*c
    )

    Seq(A,B,C)
  }


  def checkPitagorean( size : Numero ) = {
    if( size % 10 == 0 ){
      //log(size)
    }
    val minHipotenuse = size/3
    val maxHipotenuse = size/2
    for(
      h <- (minHipotenuse to maxHipotenuse).iterator ;
      h2 = h*h ;
      catetos = size - h ;
      cateto1 <- 1L to catetos/2 ;
      cateto2 = catetos-cateto1
      if( h2 == cateto1*cateto1 + cateto2*cateto2 && cateto1 <= cateto2 ) ) yield{
        val ret = (size,h,cateto1,cateto2)
        //log( s"yield: $ret")
        ret
    }
  }

  measure(){
    val limit = 10000
    val array : Array[Int] = Array.fill(limit+1)(-1)

    def sizeIsOne( it: Iterator[_]) = {
      //it.size == 1
      if( !it.hasNext ){
        0
      }
      else{
        it.drop(1)
        if( it.hasNext ) 2 else 1
      }
    }

    def suma( posicion: Int, valor: Int ) = {
      if( array(posicion) == -1 ){
        array(posicion) = 0
      }
      array(posicion) += valor
    }

    for( i <- 1 to limit ){
      if( i % 2 == 1 ){
        array(i) = 0
      }
      else if( array(i) == -1 ){
        log( s"Calculo posición $i")
        val check = checkPitagorean(i)
        val size = sizeIsOne(check)
        log( s"  size:$size")
        if( size == 0 ){
          log( s"  posición $i a 0")
          suma(i,size)
        }
        else{
          for( j <- i to limit by i ){
            log( s"  sumo $size a la posición $j")
            suma(j,size)
            log( s"  la posición $j queda a ${array(j)}")
          }
        }
      }
      else{
        println( s"No calculo posición $i")
      }
    }

    println( array.zipWithIndex.mkString("\n"))


    //log( s"solution: $solution") 
 
  }


}
