import scala.io.Source

val file = Source.fromFile("day7.test")
val lines = file.getLines.toList

val eqs = lines.map(line =>
  line.split(':') match {
    case Array(t, ops) => (t.toInt, ops.trim.split(' ').map(_.toInt).toList)
})

def isEquation(test: Int, operators: List[Int]): Boolean = ???

eqs.filter(isEquation).map(_._1).sum
