

object Problem41 extends App {

  /*We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?
*/
    type Numero = Long

  def isPrime(n: Numero) = {
    if( n%2 == 0 )
      false
    else{ 
      val found = Iterator.from(3, 2).find( i => i*i > n || n%i == 0 ).get
      found*found > n
    }
  } 
  
  def candidates( size: Int ) = {
    val last = Seq(1,3,7,9).filter(_<=size)
    
    val ret = for( l <- last ) yield{
      val first = (1 to 9).filter(_<=size).filter(_!=l)
      first.permutations.map( _.toIndexedSeq ).map( p => p.:+(l) )
    }
    
    ret.flatten.map( _.mkString.toLong )
  }
  
  val allCandidates = (2 to 7).flatMap( candidates )
  val solution = allCandidates.filter( isPrime ).max
  println( s"Solution: $solution" )
  
  
  
}