/**
  * Created by alvaro on 14/07/17.
  */
object ParaJaime extends App{

  def mideTiempo( proc : => Unit ) = {
    val ini = System.currentTimeMillis()
    proc
    val end = System.currentTimeMillis()
    println( s"Tiempo total: ${end-ini} ms")
  }


  mideTiempo {
    import Problem3.Numero


    val primes = Problem3.primes

    val threshold = 1000000

    def isPalindrome(n: Numero) = n.toString == n.toString.reverse

    val solution = primes.filter(p => p >= threshold && isPalindrome(p)).head
    println(s"El primo más pequeño mayor de $threshold y capicúa es $solution")
  }

  mideTiempo{

    type Numero = BigInt

    def esPrimo(p: Numero) = {
      def log(s: String) = {
        //println(s)
      }
      val divisorLimite = Math.sqrt(p.toDouble).toLong
      log( s"esPrimo p:$p  divisorLimite:$divisorLimite")
      def pruebaDivisor( d: Numero ) : Boolean = {
        log( s"  prueba d:$d")
        if( p%d == BigInt(0) ) {
          log( "    false")
          false
        }
        else if( d > divisorLimite ){
          log( "    true")
          true
        }
        else{
          pruebaDivisor(d+(if(d==2) 1 else 2))
        }
      }
      if( p == 2 ) true else pruebaDivisor( 2 )
    }

    for( i <- 2 to 200 if( esPrimo(i))){
      println( i )
    }


    for( i <- 0 to 10 ; n <- 0 to 9 ){
      print(".")
      val p = BigInt("1" + "0"*i + n.toString + "0"*i + "1")
      if( esPrimo(p) ) {
        println(s"\n$p")
      }
    }
  }

}
