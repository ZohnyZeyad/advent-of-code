import scala.io.Source
import scala.annotation.tailrec

val file = Source.fromResource("day14.input")
lazy val lines = file.getLines.toList

case class Point(x: Long, y: Long)
case class Robot(position: Point, velocity: Point)

lazy val robots = lines.collect {
  case s"p=$x,$y v=$a,$b" =>
    Robot(Point(x.toLong, y.toLong), Point(a.toLong, b.toLong))
}

val width = 101
val height = 103

def calculatePosition(robot: Robot, seconds: Long, width: Int, height: Int): Point = {
  import robot.{ position => p, velocity => v }

  val newX = (p.x + v.x * seconds) % width
  val newY = (p.y + v.y * seconds) % height

  val normalizedX = if newX < 0 then newX + width else newX
  val normalizedY = if newY < 0 then newY + height else newY

  Point(normalizedX, normalizedY)
}

lazy val quadrants =
  val midX = width / 2
  val midY = height / 2
  Map(
    1L -> (0 until midX, 0 until midY),
    2L -> ((midX + 1) until width, 0 until midY),
    3L -> (0 until midX, (midY + 1) until height),
    4L -> ((midX + 1) until width, (midY + 1) until height)
  )

def assignQuadrant(p: Point, quadrants: Map[Long, (Range, Range)]): Option[Long] = {
  quadrants
    .find { case (_, (qx, qy)) => qx.contains(p.x) && qy.contains(p.y) }
    .map(_._1)
}

val safetyFactor = robots.view
  .map(r => calculatePosition(r, 100, width, height))
  .map(p => assignQuadrant(p, quadrants))
  .filter(_.isDefined)
  .map(_.get)
  .groupMapReduce(identity)(_ => 1L)(_ + _)
  .values
  .product

def displayChristmasTree(positions: List[Point]): String = {
  val minX = positions.map(_.x).min
  val maxX = positions.map(_.x).max
  val minY = positions.map(_.y).min
  val maxY = positions.map(_.y).max

  val width = maxX - minX + 1
  val height = maxY - minY + 1

  val grid = Array.fill(height.toInt, width.toInt)(' ')

  positions.foreach {
    case p =>
      grid((p.y - minY).toInt)((p.x - minX).toInt) = '#'
  }

  grid.map(_.mkString).mkString("\n")
}

lazy val finalPositions =
  robots.map(r => calculatePosition(r, 100, width, height))
// println(displayChristmasTree(finalPositions))

file.close()
