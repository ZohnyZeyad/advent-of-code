import scala.io.Source
import scala.collection.immutable.ArraySeq

val file = Source.fromFile("day10.input")
val lines = file.getLines

val topoMap = ArraySeq.from(lines)

case class Point(row: Int, col: Int)

val trailHeads =
  for
    y <- topoMap.indices
    x <- topoMap(0).indices
    if topoMap(y)(x) == '0'
  yield Point(y, x)

val dirs = Vector(
  Point(1, 0), // UP
  Point(0, 1), // RIGHT
  Point(-1, 0), // DOWN
  Point(0, -1) // LEFT
)

def dfs(topoMap: ArraySeq[String], trailHeads: List[Point]): List[Int] =
  trailHeads.map(head => walk(topoMap, List(0 -> head), Nil))

def walk(
    field: ArraySeq[String],
    toWalk: List[(Int, Point)],
    acc: List[Point]
): Int = {
  if toWalk.isEmpty then acc.distinct.size
  else
    val (currVal, currPos) = toWalk.head
    val nextValue = currVal + 1

    val adjacents = neighbors(currPos, dirs)
      .filter(np => field(np.row)(np.col).asDigit == nextValue)

    if adjacents.isEmpty then walk(field, toWalk.tail, acc)
    else if nextValue == 9 then walk(field, toWalk.tail, adjacents ::: acc)
    else walk(field, adjacents.map((nextValue -> _)) ::: toWalk.tail, acc)
}

def neighbors(p: Point, dirs: Vector[Point]): List[Point] =
  dirs
    .map(dp => Point((p.row + dp.row), (p.col + dp.col)))
    .filter(isValid)
    .toList

def isValid(p: Point): Boolean =
  p.row >= 0 && p.col >= 0
    && p.row < topoMap.length && p.col < topoMap(0).length

lazy val scores = dfs(topoMap, trailHeads.toList).sum

println(scores)

file.close()
