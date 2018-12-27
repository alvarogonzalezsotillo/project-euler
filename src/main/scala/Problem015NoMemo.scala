
/**
 * Created with NumeroelliJ IDEA.
 * User: alvaro
 * Date: 15/01/14
 * Time: 10:38
 * To change this template use File | Settings | File Templates.
 */
object Problem15NoMemo extends App {

  /*
  Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down,
  there are exactly 6 routes to the bottom right corner.


  How many such routes are there through a 20×20 grid?
  */

  /**
   * Counting line intersections (not tiles):
   *
   * n x m grid => same paths as m x n grid
   * n x 1 grid => 1 path
   * n x m grid => 1 path to n-1 x m grid
   * 1 path to n x m-1 grid
   */
  type Numero = Long

  object routes {
    def apply(n: Numero, m: Numero) = {
      // Arguments expressed in tiles, algorithm in line intersections
      pathsTo((n + 1, m + 1))
    }

    private def pathsTo(key: (Numero, Numero)): Numero = {
      val paths = key match {
        case (n, m) if n > m => pathsTo((m, n))
        case (1, m) => 1.asInstanceOf[Numero]
        case (n, m) => pathsTo((n - 1, m)) + pathsTo((n, m - 1))
      }
      paths
    }
  }

  val solution = routes(20, 20)
  println(s"Solution:$solution")
}
