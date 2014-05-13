/*

Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers are all figurate (polygonal) numbers and are generated by the following formulae:

Triangle	 	P3,n=n(n+1)/2	 	1, 3, 6, 10, 15, ...
Square	 	P4,n=n2	 	1, 4, 9, 16, 25, ...
Pentagonal	 	P5,n=n(3n-1)/2	 	1, 5, 12, 22, 35, ...
Hexagonal	 	P6,n=n(2n-1)	 	1, 6, 15, 28, 45, ...
Heptagonal	 	P7,n=n(5n-3)/2	 	1, 7, 18, 34, 55, ...
Octagonal	 	P8,n=n(3n-2)	 	1, 8, 21, 40, 65, ...
The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three interesting properties.

The set is cyclic, in that the last two digits of each number is the first two digits of the next number (including the last number with the first).
Each polygonal type: triangle (P3,127=8128), square (P4,91=8281), and pentagonal (P5,44=2882), is represented by a different number in the set.
This is the only set of 4-digit numbers with this property.
Find the sum of the only ordered set of six cyclic 4-digit numbers for which each polygonal type: triangle, square, pentagonal, hexagonal, heptagonal, and octagonal, is represented by a different number in the set.

*/

object Problem61 extends App{

  type Numero = Int
  
  def sequence( generator: (Numero)=>Numero ) = {
    lazy val ret : Stream[Numero] = {
      def next(n:Numero) : Stream[Numero] = generator(n) #:: next(n+1)
      generator(1) #:: next(2)
    }
    ret
  }
  
  val min = 1000
  val max = 9999
  val candidates = min to max

  def sequenceCandidates( generator: (Numero)=>Numero ) : Seq[Numero] = sequence(generator).dropWhile(_<min).takeWhile(_<=max).toArray

  val triangles = sequenceCandidates( n => n*(n+1)/2 )
  val squares = sequenceCandidates( n => n*n )
  val pentagons = sequenceCandidates( n => n*(3*n-1)/2 )
  val hexagons = sequenceCandidates( n => n*(2*n-1)/2 )
  val heptagons = sequenceCandidates( n => n*(5*n-3)/2 )
  val octagons = sequenceCandidates( n => n*(n+1)/2 )
  
  //val all = List( triangles, squares, pentagons, hexagons, heptagons, octagons )
  val all = List( triangles, squares, pentagons )
  
  def valid( a: Numero, b: Numero ) = a.toString.drop(2) == b.toString.take(2)
  
  val permutations = all.permutations
  
  def cyclicSet( sets: List[Seq[Numero] ] ) = {
  
    println( s"trying..." )
  
    def findLineal( current: List[Numero], remainingSets: List[ Seq[Numero] ] ) : Option[List[Numero]]= {
      if( remainingSets.size == 0 ){
        if( valid( current.last, current.head ) ){
          Some(current)
        }
        else{
          None
        }
      }
      else{

        val currentSet = remainingSets.head
        val nextSets = remainingSets.tail
        def valid_( n: Numero ) = current.size == 0 || valid( current.last, n)
        val ret = currentSet.
                  filter( n => valid_( n ) ).
                  map( n => findLineal( current :+n, nextSets ) ).
                  find( _.isDefined )
        
        ret match{
          case Some(l) => l
          case None => None
        }
      }
    }
    
    findLineal( Nil, sets )
  }
  
  val solution = permutations.map( sets => cyclicSet(sets) ).find( _.isDefined ).get.get

  println( s"Solution:$solution ${solution.sum}")
  
  def whatType( n: Numero ) = all.indexWhere( s => s.contains(n) ) + 3

}