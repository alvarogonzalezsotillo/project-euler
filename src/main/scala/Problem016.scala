/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 20/01/14
 * Time: 10:19
 * To change this template use File | Settings | File Templates.
 */
object Problem16 extends App{

  /*
  By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot be solved by brute force, and requires a clever method! ;o)
   */
  val inputsSample = """3
                       |7 4
                       |2 4 6
                       |8 5 9 3
                       | """.stripMargin

  val inputsProblem = """75
                        |95 64
                        |17 47 82
                        |18 35 87 10
                        |20 04 82 47 65
                        |19 01 23 75 03 34
                        |88 02 77 73 07 63 67
                        |99 65 04 28 06 16 70 92
                        |41 41 26 56 83 40 80 70 33
                        |41 48 72 33 47 32 37 16 94 29
                        |53 71 44 65 25 43 91 52 97 51 14
                        |70 11 33 28 77 73 17 78 39 68 17 57
                        |91 71 52 38 17 14 91 43 58 50 27 29 48
                        |63 66 04 68 89 53 67 30 73 16 69 87 40 31
                        |04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
                        | """.stripMargin

  type Numero = Long

  class Triangle(inputs: String) {

    val lines = inputs.split( """\r*\n+""").map(_.trim).filter(_ != "")
    val values = lines.map(_.split("\\s+").filter(_ != "").map(_.toLong))

    val size = values.size
    val range = 0 until size

    def apply(row: Int, col: Int) = {
      if (col <= row && range.contains(row))
        values(row)(col)
      else
        0L
    }

    def v( s: Seq[Numero], i: Int ) = if( i >= 0 && i < s.size ) s(i) else 0L

    def computeMaxRow( row: Int ) : Seq[Numero]= {
      if( row == 0 ){
        apply(0,0) :: Nil
      }
      else{
        val previousMax = computeMaxRow(row-1)
        println( previousMax )
        for( col <- 0 to row ) yield {
          val candidates = v(previousMax,col) :: v(previousMax,col-1) :: Nil
          val value=apply(row, col)
          value + candidates.max
        }
      }
    }

    def computeMaxSum() = computeMaxRow(size-1).max
  }

  val solution = new Triangle(inputsProblem).computeMaxSum
  println( s"Solution:$solution")
}