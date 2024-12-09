import scala.io.Source

val file = Source.fromFile("day7.test")
val lines = file.getLines.toList

val eqs = lines.map(line =>
  line.split(':') match {
    case Array(t, ops) => (t.toInt, ops.trim.split(' ').map(_.toInt).toList)
  }
)

val operators = List("+", "*")

def generateCombinations(operators: List[String], length: Int): List[String] = {
  if (length == 0) then List("")
  else
    for
      op <- operators
      combination <- generateCombinations(operators, length - 1)
    yield op + combination
}

generateCombinations(operators, 2)

def isEquation(test: Int, operands: List[Int]): Boolean = ???

// eqs.filter(isEquation).map(_._1).sum
