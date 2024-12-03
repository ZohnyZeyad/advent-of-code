import scala.io.Source

val mulReg = raw"mul\((\d{1,3}),(\d{1,3})\)".r
val doReg = raw"do\(\)"
val dontReg = raw"don\'t\(\)"

val file = Source.fromFile("day3.input")
val lines = file.getLines()

val doList = lines.foldLeft("")(_ + _).split(doReg)

val filteredDos = doList.map { sp =>
  val dont = dontReg.r.findAllIn(sp)
  if (!dont.isEmpty) then sp.split(dontReg)(0)
  else sp
}

val result = filteredDos.map { line =>
  val muls =
    for
    m <- mulReg.findAllMatchIn(line)
    yield (m.group(1).toInt, m.group(2).toInt)

  muls.map(_ * _).foldLeft(0)(_ + _)
}.foldLeft(0)(_ + _)

println(result)

file.close()
