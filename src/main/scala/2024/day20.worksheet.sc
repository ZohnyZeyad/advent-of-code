import aoc.{ Area, Dir, Point }

val file = io.Source.fromResource("day20.input")
lazy val grid = file.getLines.toVector

val area = Area(grid)
lazy val startPos = area.pointsIterator.find(grid(_) == 'S').get
lazy val endPos = area.pointsIterator.find(grid(_) == 'E').get
lazy val walls = area.pointsIterator.filter(grid(_) == '#').toSet

def search(pos: Point, visited: Set[Point]): List[Point] = {
  pos.adjacent.diff(visited).diff(walls).headOption match {
    case Some(next) => pos :: search(next, visited + pos)
    case None       => List(pos)
  }
}

lazy val path = search(startPos, Set.empty)

val normalTime = path.zipWithIndex.toMap

def cheat(p: Point): Option[Int] = {
  val pathPoints = p.adjacent.filter(area.contains).filterNot(walls)

  Option.when(pathPoints.size == 2) {
    val List(a, b) = pathPoints.toList.sortBy(normalTime)
    normalTime(endPos) - normalTime(b) + normalTime(a) + 2
  }
}

lazy val innerWallPoints = walls.filter(area.expand(-1).contains).toList

val ans1 = innerWallPoints.flatMap(cheat).count(_ <= normalTime(endPos) - 100)

def timeSaved(from: Point, to: Point): Int =
  -(normalTime(from) - normalTime(to) + from.dist(to))

def cheatable(from: Point): List[Point] =
  path.filter(to => from.dist(to) <= 20 && timeSaved(from, to) >= 100)

val ans2 = path.map(cheatable(_).size).sum

file.close()
