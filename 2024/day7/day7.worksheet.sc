import scala.io.Source

val file = Source.fromFile("day7.input")
val lines = file.getLines.toList

val eqs = lines.map(line =>
  line.split(':') match {
    case Array(t, ops) =>
      (t.toLongOption.get, ops.trim.split(' ').map(_.toInt).toList)
  }
)

val operators = List("+", "*", "|")

def generateCombinations(operators: List[String], length: Int): List[String] = {
  if (length == 0) then List("")
  else
    for
      op <- operators
      combination <- generateCombinations(operators, length - 1)
    yield op + combination
}

generateCombinations(operators, 2)

def isEquation(test: Long, operands: List[Int]): Boolean = {
  val opComb = generateCombinations(operators, operands.length - 1)
    .map(_.toCharArray().toList)

  def calculateEquation(
      operands: List[Int],
      operators: List[Char],
      acc: Long
  ): Long = {
    val currentAcc = operate(operands.head, operators.head, acc)

    if operands.length == 1 then currentAcc
    else calculateEquation(operands.tail, operators.tail, currentAcc)
  }

  def operate(operand: Int, op: Char, acc: Long): Long = {
    op match
      case '+' => acc + operand
      case '*' => acc * operand
      case '|' => (acc.toString + operand).toLong
  }

  val results =
    opComb.map(ops => calculateEquation(operands.tail, ops, operands.head))

  results.exists(_ == test)
}

eqs.filter(isEquation).map(_._1).sum
