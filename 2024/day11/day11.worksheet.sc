import scala.io.Source
import scala.annotation.tailrec

val file = Source.fromFile("day11.input")
val line = file.getLines.mkString
val stones = line.split("\\s").map(_.toLong).toVector

val stoneCounts = stones.map(s => s -> 1L).toMap

def series = Iterator.iterate(stoneCounts)(next)

def next(stoneCounts: Map[Long, Long]): Map[Long, Long] =
  stoneCounts.toList
    .flatMap((stone, count) => transform(stone).map(_ -> count))
    .groupMapReduce(_._1)(_._2)(_ + _)

def transform(stone: Long): List[Long] =
  stone match
    case 0                                   => List(1)
    case e if stone.toString.length % 2 == 0 => splitEven(e.toString)
    case n                                   => List(n * 2024)

def splitEven(stone: String): List[Long] =
  val (left, right) = stone.splitAt(stone.length / 2)
  List(left.toLong, right.toLong)

lazy val finalStones = series.drop(25).next.view.values.sum
lazy val finalStonesLong = series.drop(75).next.view.values.sum

println(finalStones)
println(finalStonesLong)

file.close()
