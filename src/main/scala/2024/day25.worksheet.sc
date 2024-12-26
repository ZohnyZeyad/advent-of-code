val file = io.Source.fromResource("day25.input")

val schematics: List[Vector[String]] =
  val lines = file.getLines
  val lb = collection.mutable.ListBuffer.empty[Vector[String]]
  while lines.hasNext do
    val schematic = lines.takeWhile(_.nonEmpty)
    lb += schematic.toVector
  lb.toList

val locks: List[Vector[Int]] = schematics
  .filter(_.head.forall(_ == '#'))
  .map(_.transpose.map(_.count(_ == '#') - 1))

val keys = schematics
  .filter(_.last.forall(_ == '#'))
  .map(_.transpose.map(_.count(_ == '#') - 1))

val ans1 = keys
  .map: k =>
    locks.count: l =>
      l.zip(k).map(_ + _).forall(_ <= 5)
  .sum

file.close()
