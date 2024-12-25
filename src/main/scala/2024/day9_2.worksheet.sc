import scala.io.Source
import scala.annotation.tailrec

val file = Source.fromResource("day9.input")
val line = file.getLines.mkString

def parseDiskMap(diskMap: String): Vector[(Int, Boolean)] = {
  diskMap.zipWithIndex.map {
    case (char, index) =>
      (char.asDigit, index % 2 == 0)
  }.toVector
}

def createDiskRepresentation(segments: Vector[(Int, Boolean)]): Vector[Char] = {
  segments.zipWithIndex.flatMap {
    case ((length, isFile), fileId) =>
      if isFile then Vector.fill(length)(((fileId / 2) + '0').toChar)
      else Vector.fill(length)('.')
  }
}

def compactDisk(disk: Vector[Char]): Vector[Char] = {
  @tailrec
  def moveBlocks(disk: Vector[Char], leftmostFree: Int, rightmostFile: Int): Vector[Char] = {
    if leftmostFree >= rightmostFile then disk
    else {
      val updatedDisk = disk
        .updated(leftmostFree, disk(rightmostFile))
        .updated(rightmostFile, '.')
      moveBlocks(
        updatedDisk,
        updatedDisk.indexOf('.', leftmostFree + 1),
        updatedDisk.lastIndexWhere(_ != '.', rightmostFile - 1)
      )
    }
  }

  moveBlocks(disk, disk.indexOf('.'), disk.lastIndexWhere(_ != '.'))
}

def calculateChecksum(disk: Vector[Char]): Long = {
  disk.iterator.zipWithIndex.collect {
    case (block, pos) if block != '.' => (block - '0').toLong * pos
  }.sum
}

val map = parseDiskMap(line)
val diskRep = createDiskRepresentation(map)
val compact = compactDisk(diskRep)

println(calculateChecksum(compact))

file.close()
