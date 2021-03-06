import scala.annotation.tailrec

object Problem28 extends App{
/*
Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
*/

/*
43 44 45 46 47 48 49
42 21 22 23 24 25 26
41 20  7  8  9 10 27
40 19  6  1  2 11 28
39 18  5  4  3 12 29
38 17 16 15 14 13 30
37 36 35 34 33 32 31  
*/

  type Numero = Long
  
  def pow( n: Numero, exp: Numero ) : Numero = if( exp == 0 ) 1 else n*pow(n,exp-1)
  def sumDiagonalDown(m: Numero) = (m+1)*(m*m-m+3)/3
  def sumDiagonalUp(m: Numero) = (4*m*m*m + 6*m*m + 8*m - 3*pow(-1,m) + 3)/12
  
  val size = 1001
  val solution = sumDiagonalDown(size) + sumDiagonalUp(size) - 3
  println( s"Solution: $solution" )

}
  

