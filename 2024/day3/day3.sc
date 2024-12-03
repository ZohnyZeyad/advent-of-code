import scala.io.Source

val file = Source.fromFile("day3.input")
val lines = file.getLines()
val regex = raw"mul\((\d{1,3}),(\d{1,3})\)".r

val result = lines.map { line =>
  val muls =
    for
    m <- regex.findAllMatchIn(line)
    yield (m.group(1).toInt, m.group(2).toInt)

  muls.map(_ * _).foldLeft(0)(_ + _)
}.foldLeft(0)(_ + _)

println(result)
