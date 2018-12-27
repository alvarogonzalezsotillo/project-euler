/*
The cube, 41063625 (3453), can be permuted to produce two other cubes: 56623104 (3843) and 66430125 (4053). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

Find the smallest cube for which exactly five permutations of its digits are cube.
*/

object Problem62 extends App{

  type Numero = BigInt
  implicit class NumeroOps(n: Numero){
	def cube = n*n*n
	def numberOfDigits = digits.sum
	
	def digits = {
		def _digits( n: Numero, d: Array[Int] ){
			if( n != 0 ){
				val digit = n%10
				d(digit.toInt) += 1
				_digits( n/10, d )
			}
		}
		val ret = new Array[Int](10)
		_digits(n,ret)
		ret.toSeq
	 }
	
  }
  
  
  def cubesOfDigits( d: Int ) = {
    Iterator.from(1).
             map( BigInt(_) ).
             map( _.cube ).
             dropWhile( _.numberOfDigits < d ).
             takeWhile( _.numberOfDigits == d ).
			 toSeq
  }
  
  
  def cubesWithPermutation(permutations: Int) = Iterator.from(1).map{ d =>
    val c = cubesOfDigits(d)
    println( s"cubes of $d digits: ${c.size}" )
	c.groupBy( _.digits ).filter{ case(k,v) => v.size == permutations }
  }
  
  val solutions = cubesWithPermutation(5).find( _.size > 0 ).get
  solutions.foreach{ case (k,v) => println( s"$k -> $v" ) }
  
  val solution = solutions.values.flatten.min
  println( s"Solution: $solution" )
}