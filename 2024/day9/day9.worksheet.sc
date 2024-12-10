import scala.io.Source
import scala.annotation.tailrec

val file = Source.fromFile("day9.input")
val line = file.getLines.mkString

val occupiedRanges: Vector[Range] = (line + "0")
  .grouped(2)
  .foldLeft(0, Vector.empty[Range]):
    case ((index, ranges), chars) =>
      val fileBlocksSize = chars(0).asDigit
      val freeBlocksSize = chars(1).asDigit
      val newIndex = index + fileBlocksSize + freeBlocksSize

      val newRange = index until (index + fileBlocksSize)
      newIndex -> (ranges :+ newRange)
  ._2

val freeRanges: Vector[Range] = line
  .grouped(2)
  .withFilter(_.size == 2)
  .foldLeft(0, Vector.empty[Range]):
    case ((index, ranges), chars) =>
      val fileBlocksSize = chars(0).asDigit
      val freeBlocksSize = chars(1).asDigit
      val newIndex = index + fileBlocksSize + freeBlocksSize

      if freeBlocksSize == 0 then newIndex -> ranges
      else
        val newRange = (index + fileBlocksSize) until newIndex
        newIndex -> (ranges :+ newRange)
  ._2

val zippedIds = occupiedRanges.zipWithIndex

val idsFromEnd = zippedIds.reverse
  .flatMap((range, id) => range.map(_ => id))

val sizeOccupied = occupiedRanges.map(_.size).sum

object Fragment:
  @tailrec
  def checkSum(from: Int, fill: Vector[Int], acc: Long): Long = {
    if from >= sizeOccupied then acc
    else if occupiedRanges.exists(_.contains(from)) then
      val fileCheck = from * zippedIds.find(_._1.contains(from)).get._2
      checkSum(from + 1, fill, acc + fileCheck)
    else checkSum(from + 1, fill.tail, acc + from * fill.head)
  }

println(Fragment.checkSum(0, idsFromEnd, 0))

file.close()
