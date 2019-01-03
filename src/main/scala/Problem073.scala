/*




 */
object Problem73 extends App {

  def measure[T](message: String = "")(proc: => T): T = {
    println(s"$message")
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println(s"$message ${end - ini} ms")
    ret
  }

  type Numero = Long
  type Flotante = Double


  def mcd( a: Numero, b: Numero ) : Numero = b match{
    case 0 => a
    case _ => mcd(b,a%b)
  }

  def naive( limit: Numero, min: Flotante, max: Flotante ) = {
    println( s"limit: $limit min:$min max:$max")
    def between(d:Flotante) = {
      //println( s"  d:$d")
      min < d && d < max
    }
    (for( den <- 1L to limit ; num <- 1L to den if mcd(num,den) == 1 && between(1.0*num/den) ) yield{
      1
    }).size
    
  }

  measure(){
    val solution = naive(12000, 1.0/3, 1.0/2 )
    println( s"naive: $solution") //7295372
  }


}
