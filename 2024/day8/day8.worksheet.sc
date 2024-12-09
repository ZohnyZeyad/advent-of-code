import scala.io.Source

val file = Source.fromFile("day8.input")
val lines = file.getLines.toList

val antennas =
  (for
    x <- lines.indices
    y <- lines(x).indices
    if lines(x)(y).isLetterOrDigit
  yield (x, y) -> lines(x)(y)).toList

val antennasMap = antennas.groupBy(_._2).view.mapValues(_.map(_._1)).toList

def calculateAntinodes(
    antenna1: (Int, Int),
    antenna2: (Int, Int)
): List[(Int, Int)] = {
  val (y1, x1) = antenna1
  val (y2, x2) = antenna2

  val dy = y2 - y1
  val dx = x2 - x1

  def generateAntinodes(
      y: Int,
      x: Int,
      dy: Int,
      dx: Int,
      acc: List[(Int, Int)]
  ): List[(Int, Int)] = {
    val antinode = (y + dy, x + dx)
    if !isInBounds(antinode._1, antinode._2) then acc
    else generateAntinodes(antinode._1, antinode._2, dy, dx, antinode :: acc)
  }

  def isInBounds(y: Int, x: Int): Boolean = {
    y >= 0 && x >= 0 && y < lines.length && x < lines(0).length
  }

  val antinodes1 = generateAntinodes(y1, x1, -dy, -dx, List(antenna1))
  val antinodes2 = generateAntinodes(y2, x2, dy, dx, List(antenna2))

  antinodes1 ::: antinodes2
}

val antinodes = antennasMap
  .flatMap(annMap =>
    annMap._2
      .combinations(2)
      .flatMap(comb =>
        comb match
          case an1 :: an2 :: _ => calculateAntinodes(an1, an2)
          case _               => Nil
      )
      .toList
  )
  .distinct

antinodes.length
