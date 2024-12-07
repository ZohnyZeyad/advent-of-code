import scala.io.Source

val file = Source.fromFile("day4.input")
val lines = file.getLines()
val linesArray = lines.toArray

val rows = linesArray.length
val cols = linesArray(0).length

enum Direction(val delta: (Int, Int)):
  case Right extends Direction((0, 1))
  case Left extends Direction((0, -1))
  case Down extends Direction((1, 0))
  case Up extends Direction((-1, 0))
  case DownRight extends Direction((1, 1))
  case DownLeft extends Direction((1, -1))
  case UpRight extends Direction((-1, 1))
  case UpLeft extends Direction((-1, -1))

def isValid(x: Int, y: Int): Boolean =
  x >= 0 && y >= 0 && x < rows && y < cols

import Direction._

def countWordOccurrences(
    grid: Array[String],
    word: String
): Int =
  val wordLength = word.length

  def checkDirection(x: Int, y: Int, dx: Int, dy: Int): Boolean =
    (0 until wordLength).forall { i =>
      val newX = x + i * dx
      val newY = y + i * dy
      isValid(newX, newY) && grid(newX)(newY) == word(i)
    }

  var count = 0
  for
    x <- 0 until rows
    y <- 0 until cols
    if grid(x).charAt(y) == 'X'
    (dx, dy) <- Direction.values.map(_.delta)
    if checkDirection(x, y, dx, dy)
  do count += 1

  count

println(countWordOccurrences(linesArray, "XMAS"))

def countXmasOccurences(
    grid: Array[String],
    word: String
): Int =

  val directions =
    List(UpRight, DownLeft, UpLeft, DownRight)

  def checkForMas(x: Int, y: Int): Boolean =
    val Array((dx1, dy1), (dx2, dy2), (dx3, dy3), (dx4, dy4)) =
      directions.map(_.delta).toArray

    val (x1, y1) = (x + dx1, y + dy1)
    val (x2, y2) = (x + dx2, y + dy2)
    val (x3, y3) = (x + dx3, y + dy3)
    val (x4, y4) = (x + dx4, y + dy4)

    if !isValid(x1, y1) || !isValid(x2, y2)
      || !isValid(x3, y3) || !isValid(x4, y4)
    then return false

    val word1 = List(grid(x1)(y1), 'A', grid(x2)(y2)).mkString
    val word2 = List(grid(x3)(y3), 'A', grid(x4)(y4)).mkString

    (word1 == word || word1 == word.reverse) && (word2 == word || word2 == word.reverse)

  var count = 0
  for
    x <- 0 until rows
    y <- 0 until cols
    if grid(x).charAt(y) == 'A'
    if checkForMas(x, y)
  do count += 1

  count

println(countXmasOccurences(linesArray, "MAS"))

file.close()
