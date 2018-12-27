/*


 Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

 If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

 It can be seen that 2/5 is the fraction immediately to the left of 3/7.

 By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending order of size, find the numerator of the fraction immediately to the left of 3/7.

 */
object Problem71 extends App {

  def measure[T](message: String)(proc: => T): T = {
    println(s"$message")
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println(s"$message ${end - ini} ms")
    ret
  }

  def measure[T](proc: => T): T = {
    measure("")(proc)
  }

  type Numero = Long
  type Flotante = Double

  
  

  def mcd( a: Numero, b: Numero ) : Numero= b match{
    case 0 => a
    case _ => mcd(b,a%b)
  }

  case class Aproximation( num: Numero, den: Numero){
    val value = num.toDouble/den
  }

  
  def findBelow( target: Flotante, limit: Numero, num: Numero = 0, den: Numero = 1, better: Aproximation = Aproximation(0,1) ) : Aproximation = {
    val current = num.toDouble/den
    if( current >= target ){
      findBelow(target, limit,  num-200, den+1,better)
    }
    else{

      val nextBetter = if( current < better.value ){
        better
      }
      else{
        val `new` = Aproximation(num,den)
        //println( "New : " + `new`)
        `new`
      }

      if( den < limit ){
        findBelow(target, limit, num+1,den, nextBetter)
      }
      else{
        better
      }
    }
  }


  measure {
    val solution = findBelow( 3.0/7,1000001.toLong)
    println( solution )
    println( solution.value )
    println( 3.0/7 )
    println( mcd(solution.den,solution.num))
  }

}
