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

def isValid(row: Int, col: Int): Boolean =
  row >= 0 && col >= 0 && row < area.length && col < area(0).length

def walk(
    position: (Int, Int),
    direction: String,
    visited: List[(Int, Int)]
): List[(Int, Int)] = {
  val deltaPos = directions(direction)._1
  val (ny, nx) = (position._1 + deltaPos._1, position._2 + deltaPos._2)
  if !isValid(ny, nx) then visited.distinct
  else
    area(ny)(nx) match {
      case '#' => walk(position, directions(direction)._2, visited)
      case _   => walk((ny, nx), direction, (ny, nx) :: visited)
    }
}

val path = walk(startPos, startDir, List(startPos))
println(path.length)

def isLoop(obstaclePos: (Int, Int)): Boolean = {
  val newArea = area.updated(
    obstaclePos._1,
    area(obstaclePos._1).updated(obstaclePos._2, '#')
  )

  causesLoop(newArea, startPos, startDir)
}

def causesLoop(
    area: Array[Array[Char]],
    startPos: (Int, Int),
    startDir: String
): Boolean = {

  def walkWithObstruction(
      position: (Int, Int),
      direction: String,
      visited: Set[((Int, Int), String)]
  ): Boolean = {

    val deltaPos = directions(direction)._1
    val (ny, nx) = (position._1 + deltaPos._1, position._2 + deltaPos._2)

    if !isValid(ny, nx) then false
    else
      area(ny)(nx) match {
        case '#' =>
          val newDirection = directions(direction)._2
          if visited.contains((position, newDirection)) then true
          else
            walkWithObstruction(
              position,
              newDirection,
              visited + ((position -> newDirection))
            )
        case _ =>
          if visited.contains(((ny, nx) -> direction)) then true
          else
            walkWithObstruction(
              (ny, nx),
              direction,
              visited + (((ny, nx) -> direction))
            )
      }
  }

  walkWithObstruction(startPos, startDir, Set((startPos, startDir)))
}

val validObstaclePositions =
  for
    y <- area.indices
    x <- area(0).indices
    if area(y)(x) == '.' && (y, x) != startPos
    if isLoop((y, x))
  yield (y, x)

validObstaclePositions.size

file.close()
