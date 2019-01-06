/*




 */
object Problem76 extends App {

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



  type Numero = Long
  type Flotante = Double




  if( false ) measure(){
    type Suma = IndexedSeq[Numero]

    def sums( total: Int, size: Int ) = {

      val set = scala.collection.mutable.Set[Suma]()

      def next( s: Suma ) : Seq[Suma] = {
        log( s"s:$s" )
        for(
          p <- 1 until size if( s(p-1)+1 <= s(p) );
          i <- p-1 to 0 by -1 if(s(i)+1 <= s(i+1) ) ;
          n = s.updated(i,s(i)+1).updated(p,s(p)-1)
          if( n(i) <= n(p) && !set.contains(n) ) )
        yield{
          set += n
          log( s"  i:$i p:$p n:$n")
          n
        }
        
      }

      def nextStream( s: Suma ) : Stream[Suma] = {
        val n = next(s).toStream
        n #::: n.map(nextStream).flatten
      }

      if( size <= total ){
        val first = {
          val f = Array.fill[Numero](size)(1)
          f(size-1) = total - size + 1
          f.toList.toIndexedSeq
        }

        first #:: nextStream(first)
      }
      else{
        Stream.empty
      }
    }

    def allSums(total: Int) = (1 to total).map(sums(total,_).toSet.size).sum



    def checkFormula1(total: Int, size: Int ) = {
      log( s"check1: $total $size")
      assert( sums(total,size).size == sums(total+size, size).size - sums(total+size-1, size-1).size )
    }

    def checkFormula2(total: Int, size: Int ) = {
      log( s"check2: $total $size")
      assert( sums(total,size).size == sums(total-size, size).size + sums(total-1, size-1).size )
    }

    def checkFormula3(total: Int, size: Int ) = {
      val v1 = sums(total,size).size
      val v2 = fast(total,size)
      log( s"check3: $total $size $v1 $v2")
      assert( v1 == v2 )
    }


    val limit = 30

    print( "\t")
    for( n <- 1 to limit ){
      print( s"\tn:$n")
    }
    println()


    for( size <- 1 to limit ){
      print( s"\tsize:$size")
      for( n <- 1 to limit ){
        print( s"\t${sums(n,size).toSet.size}" )
      }
      println()
    }

    print( "\n\n\t")
    for( n <- 1 to limit ){
      print( s"\t${allSums(n)}")
    }
    println()

    for( t <- 2 to limit ; s <- 2 to limit if( s < t ) ){
      checkFormula1(t,s)
      checkFormula2(t,s)
      checkFormula3(t,s)
    }
  }


  def fast( total: Int, size: Int, level: Int = 0 ) : Numero = {
    log( s"${"  "*level}f($total,$size)")
    val ret = (total,size) match{
      case (_,1) => 1
      case (n,s) if n < s => 0
      case (n,s) if n == s => 1
      case (n,s) => fast(n-s,s,level+1) + fast(n-1,s-1,level+1)
    }
    log( s"${"  "*level}$ret")
    ret
  }
  

  val n = 100

  if( true ) measure(){


    def solution(n: Int ) = (2 to n).map( fast(n,_) ).sum

    assert( solution(1) == 0 )
    assert( solution(2) == 1 )
    assert( solution(3) == 2 )
    assert( solution(4) == 4 )
    assert( solution(5) == 6 )
    assert( solution(6) == 10 )
    println( s"solution: ${solution(n)}" )
  }


  measure(){

    object memoize{
      val memo = scala.collection.mutable.Map[(Int,Int),Numero]()

      def apply( total: Int, size: Int ) : Numero = apply( (total,size) )


      def apply( totalAndSize: (Int,Int) ) : Numero = memo.get(totalAndSize) match{
        case Some(ret) =>
          ret
        case None =>
          val ret = totalAndSize match{
            case (_,1) => 1
            case (n,s) if n < s => 0
            case (n,s) => apply( (n-s,s) )  + apply((n-1,s-1))
          }

          memo( totalAndSize ) = ret
          ret
      }

      def solution(n: Int ) = (n to 2 by -1).map( memoize(n,_) ).sum
    }


    println( s"solution: ${memoize.solution(n)}" ) // 190569291
  }

}
