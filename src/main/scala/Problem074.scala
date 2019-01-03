/*




 */
object Problem74 extends App {

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


  implicit class Factorializable(n:Numero){
    def !() : Numero = n match{
      case 0 => 1
      case 1 => 1
      case n =>  n * (n-1).! 
    }

    def digits : Seq[Int] = n.toString.map( _.toInt - '0'.toInt  )

    def step = digits.map(_!).sum
  }


  def findCycle[T](first: T)( step: T=>T ) = {
    val it = Iterator.iterate(first)(step)
    val set = collection.mutable.Set(it.next)
    var next = it.next
    while( !set.contains(next) ){
      set += next
      next = it.next
    }
    set
  }

  measure(){

    def findFactCycle(n:Numero) = findCycle(n)( f => f.step )

    val cores = 4
    val limit = 1000000

    val seqs = for( c <- (0 until cores).par ) yield {
      for( n <- c*limit/cores to (c+1)*limit/cores if( findFactCycle(n).size == 60 ) ) yield{
        1
      }
    }

    val all = seqs.flatten

    val solution = all.size
    println( s"solution: $solution") //402
  }


}
