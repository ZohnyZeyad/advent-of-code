import scala.io.Source
import scala.collection.mutable.Queue

val file = Source.fromFile("day12.input")
lazy val lines = file.getLines.toArray

case class Point(row: Int, col: Int)

val dirs = Vector(
  Point(1, 0), // UP
  Point(0, 1), // RIGHT
  Point(-1, 0), // DOWN
  Point(0, -1) // LEFT
)

def isValid(p: Point): Boolean =
  p.row >= 0 && p.col >= 0
    && p.row < lines.length && p.col < lines(0).length

def floodFill(
    map: Array[Array[Char]],
    visited: Array[Array[Boolean]],
    start: Point
): (Int, Int) = {
  val rows = map.length
  val cols = map(0).length
  val plantType = map(start.row)(start.col)
  var area = 0
  var perimeter = 0

  val queue = Queue[Point]()
  queue.enqueue(start)
  visited(start.row)(start.col) = true

  while (queue.nonEmpty) {
    val current = queue.dequeue()
    area += 1

    for (dir <- dirs) {
      val neighbor = Point(current.row + dir.row, current.col + dir.col)
      if (isValid(neighbor)) {
        if (
          map(neighbor.row)(neighbor.col) == plantType
          && !visited(neighbor.row)(neighbor.col)
        ) {
          visited(neighbor.row)(neighbor.col) = true
          queue.enqueue(neighbor)
        } else if (map(neighbor.row)(neighbor.col) != plantType) {
          perimeter += 1
        }
      } else {
        perimeter += 1
      }
    }
  }

  (area, perimeter)
}

def calculateTotalPrice(map: Array[Array[Char]]): Int = {
  val rows = map.length
  val cols = map(0).length
  val visited = Array.ofDim[Boolean](rows, cols)
  var totalPrice = 0

  for (i <- 0 until rows; j <- 0 until cols) {
    if (!visited(i)(j)) {
      val (area, perimeter) = floodFill(map, visited, Point(i, j))
      totalPrice += area * perimeter
    }
  }

  totalPrice
}

lazy val map = lines.map(_.toCharArray)
calculateTotalPrice(map)

file.close()
