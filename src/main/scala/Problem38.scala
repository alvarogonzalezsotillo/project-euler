object Problem38 extends App{

/*
Take the number 192 and multiply it by each of 1, 2, and 3:

192 × 1 = 192
192 × 2 = 384
192 × 3 = 576
By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

*/
  
  /*
   
   Let P the biggest pandigital generator with n > 1
   P generates pandigital D
   P has log(P) digits
   
   P * 10^log(P) + 2*P = D >= 918273645
   
   10^log(P) >= (918273645-2*P)/P
   
   log(P) >= log((918273645-2*P)/P)
   
   Using Maxima:
   
   (%i1) find_root(P * 10^log10(P) + 2*P > 918273645,P,1,999999999);
   (%o1) 30302.03031051515 
   
   */
  
  type Numero = BigInt

  def isPandigital(n: Numero) = {
    val s = n.toString.toSet
    s.size == 9 && !s.contains('0')
  }
  
  def canBePandigitalGenerator(n: Numero) = {
    val s = n.toString.toSet
    s.size == n.toString.size && !s.contains('0')
  }
  
  def concatenatedProduct(n: Numero) : Stream[Numero] = {
    def concat( previous: Numero, m: Numero ) : Stream[Numero] = {
      if( isPandigital(previous) || previous > 999999999 ){
        Stream()
      }
      else{
        val next = BigInt(previous.toString + (n*m).toString)
        next #:: concat( next, m+1)
      }
    }
    println(n)
    n #:: concat(n, 2)
  }
  
  def isPandigitalGenerator(n: Numero) =  {
    canBePandigitalGenerator(n) && concatenatedProduct(n).takeWhile(_<1000000000).exists(isPandigital)
  }
  
  
  
  val solution = Stream.from(1).map( i => BigInt(40000-i) ).takeWhile(_>0).find(isPandigitalGenerator).get
  println( s"Solution:$solution ${concatenatedProduct(solution).mkString(",")}")
}