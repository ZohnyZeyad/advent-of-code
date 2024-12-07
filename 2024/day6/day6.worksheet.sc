import scala.io.Source

val file = Source.fromFile("day6.input")
val lines = file.getLines.toArray
val guardReg = raw"\^|v|>|<".r

val directions = Map(
  "^" -> ((-1, 0), ">"),
  ">" -> ((0, 1), "v"),
  "v" -> ((1, 0), "<"),
  "<" -> ((0, -1), "^")
)

val (startPos, startDir) =
  (for
    line <- lines
    if guardReg.findFirstMatchIn(line).isDefined
  yield (
    (lines.indexOf(line), guardReg.findFirstMatchIn(line).map(_.start).get),
    (guardReg.findFirstMatchIn(line).get.toString)
  )).head

val area = lines.map(_.toCharArray)
def walk(
    position: (Int, Int),
    direction: String,
    visited: List[(Int, Int)]
): List[(Int, Int)] = {
  val deltaPos = directions(direction)._1
  val (ny, nx) = (position._1 + deltaPos._1, position._2 + deltaPos._2)
  if ny < 0 || ny >= area.length || nx < 0 || nx >= area(0).length then visited.distinct
  else
    area(ny)(nx) match {
      case '#' => walk(position, directions(direction)._2, visited)
      case _   => walk((ny, nx), direction, (ny, nx) :: visited)
    }
}

walk(startPos, startDir, List(startPos)).length
