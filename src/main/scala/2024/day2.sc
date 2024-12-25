import scala.annotation.tailrec
import scala.io.Source

val file = Source.fromResource("./day2.input")
val lines = file.getLines()

val reports = lines
  .filterNot(_.isEmpty)
  .map(line => line.split(" ").map(_.toInt).toList)
  .toList

def isSafe(report: List[Int], count: Int): Boolean =
  if count > 1 then false
  else isIncreasing(report, Nil, count) || isDecreasing(report, Nil, count)

@tailrec
def isIncreasing(report: List[Int], acc: List[Int], count: Int): Boolean =
  report match
    case r1 :: r2 :: rest =>
      if 1 <= (r2 - r1) && (r2 - r1) <= 3 then isIncreasing(report.tail, acc :+ r1, count)
      else isSafe((acc :+ r1) ::: rest, count + 1) || isSafe((acc :+ r2) ::: rest, count + 1)
    case _                => true

@tailrec
def isDecreasing(report: List[Int], acc: List[Int], count: Int): Boolean =
  report match
    case r1 :: r2 :: rest =>
      if (r2 - r1) <= -1 && -3 <= (r2 - r1) then isDecreasing(report.tail, acc :+ r1, count)
      else isSafe((acc :+ r1) ::: rest, count + 1) || isSafe((acc :+ r2) ::: rest, count + 1)
    case _                => true

println(reports.count(isSafe(_, 0)))

// reports.filter(isSafe(_, 0)).foreach(println)
// println("---------")
// reports.filterNot(isSafe(_, 0)).foreach(println)

file.close()
