import scala.io.Source
import scala.annotation.tailrec

val file = Source.fromResource("day9.input")
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

lazy val zippedIds = occupiedRanges.zipWithIndex

lazy val idsFromEnd = zippedIds.reverse
  .flatMap((range, id) => range.map(_ => id))

lazy val sizeOccupied = occupiedRanges.map(_.size).sum

object Fragment:
  @tailrec
  def checksum(from: Int, fill: Vector[Int], acc: Long): Long = {
    if from >= sizeOccupied then acc
    else if occupiedRanges.exists(_.contains(from)) then
      val fileCheck = from * zippedIds.find(_._1.contains(from)).get._2
      checksum(from + 1, fill, acc + fileCheck)
    else checksum(from + 1, fill.tail, acc + from * fill.head)
  }

println(Fragment.checksum(0, idsFromEnd, 0))

object Compact:
  @tailrec
  def compact(
    compacted: Vector[(Int, Range)],
    freeSpaces: Vector[Range],
    toMove: Vector[(Int, Range)]
  ): Vector[(Int, Range)] =
    if toMove.isEmpty then compacted
    else
      val (id, range) = toMove.head
      val spaceIndex = freeSpaces
        .takeWhile(_.start < range.start)
        .indexWhere(_.size >= range.size)

      if spaceIndex == -1 then compact(compacted :+ (id -> range), freeSpaces, toMove.tail)
      else
        val space = freeSpaces(spaceIndex)
        val newFreeSpace = (space.start + range.size) until space.end

        val shift = range.start - space.start
        val newPosition = (range.start - shift) until (range.end - shift)

        compact(
          compacted :+ (id -> newPosition),
          freeSpaces.updated(spaceIndex, newFreeSpace),
          toMove.tail
        )

lazy val toMove = zippedIds.map(_.swap).reverse
val compacted = Compact.compact(Vector.empty, freeRanges, toMove)

val compactedChecksum = compacted.view
  .sortBy(_._2.start)
  .map[Long]:
    case (id, range) =>
      range.map(_.toLong * id.toLong).sum.toLong
  .sum

file.close()
