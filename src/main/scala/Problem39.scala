object Problem39 extends App {
/*
 If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?
*/
  
  
  def triangles(perimeter: Int) = {
    for( a <- 1 until perimeter/2 ; 
         b <- a+1 until perimeter/2 ;
         c = perimeter - a - b 
         if a*a + b*b == c*c ) yield (a,b,c)
  }
  
  val solution = (1 to 1000).
                 map( p => (p,triangles(p).size) ).
                 maxBy{ case(p,n) => n }
  
  println( s"Solution:$solution")
  
}