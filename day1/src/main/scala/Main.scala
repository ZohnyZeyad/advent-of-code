import scala.io.Source

object Main extends App {

  private val input = Source.fromFile("F:/PROGRAMMING/SCALA/advent-of-code/day1/data/input.txt")

  private val lines = input.getLines()

  private val (ids1, ids2) = lines
    .map(line => line.split(" {3}").map(_.toInt))
    .foldLeft((List[Int](), List[Int]())) {
      case ((acc1, acc2), ids) => (acc1 :+ ids(0), acc2 :+ ids(1))
    }

  private val ids1Sorted = ids1.sorted
  private val ids2Sorted = ids2.sorted

  private val sum: Int =
    ids1.sorted.zip(ids2.sorted)
      .map { case (id1, id2) => Math.abs(id1 - id2) }
      .sum

  println(sum)

  private val similarityScore = ids1Sorted.foldLeft(0) { case (acc, id) =>
    ids2Sorted.count(_ == id) * id + acc
  }

  println(similarityScore)

  input.close()
}