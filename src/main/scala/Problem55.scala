/*
If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.

Not all numbers produce palindromes so quickly. For example,

349 + 943 = 1292,
1292 + 2921 = 4213
4213 + 3124 = 7337

That is, 349 took three iterations to arrive at a palindrome.

Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome. A number that never forms a palindrome through the reverse and add process is called a Lychrel number. Due to the theoretical nature of these numbers, and for the purpose of this problem, we shall assume that a number is Lychrel until proven otherwise. In addition you are given that for every number below ten-thousand, it will either (i) become a palindrome in less than fifty iterations, or, (ii) no one, with all the computing power that exists, has managed so far to map it to a palindrome. In fact, 10677 is the first number to be shown to require over fifty iterations before producing a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).

Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.

How many Lychrel numbers are there below ten-thousand?

NOTE: Wording was modified slightly on 24 April 2007 to emphasise the theoretical nature of Lychrel numbers.
*/
object Problem55 extends App{
  
    def measure[T]( msg: String = "" )( proc: =>T ) = {
      println( s"-->$msg" )
      val ini = System.currentTimeMillis
      val ret = proc
      val end = System.currentTimeMillis
      println( s"<--$msg: ${end-ini} ms" )
      ret
    }
    
  type Numero = BigInt
  implicit def toNumero(i: Int) : BigInt =BigInt(i)
  implicit def toNumero(i: String) : BigInt =BigInt(i)
  def reverse( n: Numero ) : Numero = n.toString.reverse
  def palindromic( n: Numero ) =  n == reverse(n)
  
  def lychrel( n: Numero, iterations: Int = 50 ) : Boolean = {
  
    if( iterations <= 0 ){
      true
    }
    else{
      //println( s"  iterations: $iterations  n:$n  reverse:${reverse(n)}  sum:${n + reverse(n)}" )
      val sum = n + reverse(n)
      if( palindromic ( sum ) ){
        //println( s"  palindromic:$sum" )
        false
      }
      else{
        lychrel( sum, iterations-1 )
      }
    }
  }
 
  Seq(47,196,349,4994).foreach( n => println( s"$n ${lychrel(n,100)}" ) )
 
  measure(){ 
    val solution = (1 until 10000).map( lychrel(_) ).count( b => b )
    println( s"Solution: $solution" )
  }
  
}