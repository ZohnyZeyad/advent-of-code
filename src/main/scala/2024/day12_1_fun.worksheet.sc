import scala.io.Source
import scala.annotation.tailrec
import scala.collection.immutable.HashSet

case class Point(row: Int, col: Int)

val file = Source.fromResource("day12.test")
lazy val lines = file.getLines.toVector

lazy val points =
  for
    row <- lines.indices
    col <- lines(row).indices
  yield Point(row, col)

lazy val dirs = Vector(
  Point(1, 0), // UP
  Point(0, 1), // RIGHT
  Point(-1, 0), // DOWN
  Point(0, -1) // LEFT
)

object Garden:
  @tailrec
  def floodFill(
    queue: Vector[Point],
    visited: HashSet[Point],
    area: Long = 0L,
    perimeter: Long = 0L
  ): (Long, Long, HashSet[Point]) = {

    if queue.isEmpty then (area, perimeter, visited)
    else
      val currentPoint = queue.head
      val plantType = lines(currentPoint.row)(currentPoint.col)
      val adjacents = neighbors(plantType, currentPoint, dirs)

      val (oldAdjacents, newAdjacents) = adjacents.partition(visited.contains)
      val perimeters = 4 - newAdjacents.size - oldAdjacents.size

      floodFill(
        queue.tail :++ newAdjacents,
        visited ++ newAdjacents,
        area + 1,
        perimeter + perimeters
      )
  }

def neighbors(plant: Char, point: Point, dirs: Vector[Point]): List[Point] =
  dirs
    .map(delta => Point((point.row + delta.row), (point.col + delta.col)))
    .filter(isValid)
    .filter(np => lines(np.row)(np.col) == plant)
    .toList

def isValid(p: Point): Boolean =
  p.row >= 0 && p.col >= 0
    && p.row < lines.length && p.col < lines(0).length

lazy val totalPrice = points
  .foldLeft[(Long, HashSet[Point])](0L, HashSet.empty) { (acc, point) =>
    {
      if acc._2.contains(point) then acc
      else
        val (area, perimeter, visited) =
          Garden.floodFill(Vector(point), acc._2 + point)
        (acc._1 + (area * perimeter), visited)
    }
  }
  ._1

println(totalPrice)

file.close()
