object Problem69 extends App{

 def measure[T](proc: => T) = {
    val ini = System.currentTimeMillis
    val ret = proc
    val end = System.currentTimeMillis
    println(s"${end - ini} ms")
    ret
  }

  type Numero = Int

  def mcd(a:Numero, b:Numero) = {
    @scala.annotation.tailrec
    def mcd_0(a:Numero, b:Numero) : Numero = {
      if(b==0) a
      else mcd_0(b, a%b)
    }
    mcd_0(a max b, a min b) 
  }

  def relativePrime(a:Numero,b:Numero) = mcd(a,b) == 1

  def phi(n:Numero) = (1 until n).count(relativePrime(n,_))

  def criteria(n:Numero) = {
    val p = phi(n)
    val ret = 1.0*n/p
    if( n%1000 == 0 ) println( s"$n\t phi:$p\t ret:$ret")
    ret
  }

  measure{
    val solution = (2 to 1000000).maxBy( criteria )

    println( s"Solution:$solution" )
  }

}
