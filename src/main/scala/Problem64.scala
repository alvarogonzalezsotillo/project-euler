/*
All square roots are periodic when written as continued fractions and can be written in the form:

vN = a0 +	
1
 	a1 +	
1
 	 	a2 +	
1
 	 	 	a3 + ...
For example, let us consider v23:

v23 = 4 + v23 — 4 = 4 + 	
1
 = 4 + 	
1
 	
1
v23—4
 	1 + 	
v23 – 3
7
If we continue we would get the following expansion:

v23 = 4 +	
1
 	1 +	
1
 	 	3 +	
1
 	 	 	1 +	
1
 	 	 	 	8 + ...
The process can be summarised as follows:

a0 = 4,	 	
1
v23—4
 = 	
v23+4
7
 = 1 + 	
v23—3
7
a1 = 1,	 	
7
v23—3
 = 	
7(v23+3)
14
 = 3 + 	
v23—3
2
a2 = 3,	 	
2
v23—3
 = 	
2(v23+3)
14
 = 1 + 	
v23—4
7
a3 = 1,	 	
7
v23—4
 = 	
7(v23+4)
7
 = 8 + 	v23—4
a4 = 8,	 	
1
v23—4
 = 	
v23+4
7
 = 1 + 	
v23—3
7
a5 = 1,	 	
7
v23—3
 = 	
7(v23+3)
14
 = 3 + 	
v23—3
2
a6 = 3,	 	
2
v23—3
 = 	
2(v23+3)
14
 = 1 + 	
v23—4
7
a7 = 1,	 	
7
v23—4
 = 	
7(v23+4)
7
 = 8 + 	v23—4
It can be seen that the sequence is repeating. For conciseness, we use the notation v23 = [4;(1,3,1,8)], to indicate that the block (1,3,1,8) repeats indefinitely.

The first ten continued fraction representations of (irrational) square roots are:

v2=[1;(2)], period=1
v3=[1;(1,2)], period=2
v5=[2;(4)], period=1
v6=[2;(2,4)], period=2
v7=[2;(1,1,1,4)], period=4
v8=[2;(1,4)], period=2
v10=[3;(6)], period=1
v11=[3;(3,6)], period=2
v12= [3;(2,6)], period=2
v13=[3;(1,1,1,1,6)], period=5

Exactly four continued fractions, for N = 13, have an odd period.

How many continued fractions for N = 10000 have an odd period?
*/
object Problem64 extends App{
  
  type Numero = Long
  
  def isqrt( n: Numero, candidate: Numero = 1 ) : Numero = {
    if( candidate*candidate <= n && (candidate+1)*(candidate+1) > n )
      candidate
    else
      isqrt(n,(n/candidate + candidate)/2)
  }
  
  def log( s : => String ) = println( s )

  import Math.sqrt

  def mcd(a: Numero, b: Numero): Numero = if (b == 0) a else if( a == 0 ) b else mcd(b, a % b)
  
  def divisible( n: Numero, by: Numero ) = n % by == 0
  
  case class Remainder( rooted: Numero, minus: Numero, dividedBy: Numero ){
  
    assert( rooted > 0 )
    assert( minus > 0 )
    assert( dividedBy > 0 )
    assert( divisible( rooted - minus*minus, dividedBy ) )
  
    lazy val nextDB = rooted - minus*minus
    lazy val nextIP = (( dividedBy*(sqrt(rooted) + minus) )/nextDB ).toLong
    lazy val nextM = -dividedBy*minus + nextIP*nextDB
    
    //val nextIP = ( dividedBy * minus + nextM )/nextDB
    
    log( s"nextDB:$nextDB  nextM:$nextM  nextIP:$nextIP" )

    //assert( mcd( nextM, nextDB) == dividedBy )
    
    lazy val nextDividedBy = nextDB/dividedBy
    lazy val nextIntegerPart = nextIP/dividedBy
    lazy val nextMinus = nextM/dividedBy
  
    lazy val nextRemainder = Remainder( rooted, nextMinus, nextDividedBy )
  }
    
  
  def continuedFraction( n: Numero ) = {

    def next( r: Remainder ) : Stream[(Numero,Remainder)] = {
      val nextR = r.nextRemainder
      (r.nextIntegerPart, nextR ) #:: next( nextR )
    }
    
    val a0 = isqrt(n)
    val remainder0 = Remainder(n, a0, 1 )
    (a0, remainder0 ) #:: next( remainder0 )
  
  }
  
  for( n <- 2 to 13 if isqrt(n).toDouble != sqrt(n) ){
    val  cf = continuedFraction( n )
    println( cf.take(10).toList )
  }
}