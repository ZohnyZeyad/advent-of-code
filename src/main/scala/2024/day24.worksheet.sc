import scala.util.chaining.*

val file = scala.io.Source.fromResource("day24.input")
lazy val input = file.getLines.toList

lazy val initWires: State = Map.from[String, Boolean]:
  input.collect: // TODO normalize??
    case s"$name: 1" => name -> true
    case s"$name: 0" => name -> false

sealed trait Gate:
  def a: String
  def b: String
  def swap: Gate = this match
    case AND(a, b) => AND(b, a)
    case OR(a, b)  => OR(b, a)
    case XOR(a, b) => XOR(b, a)

case class AND(a: String, b: String) extends Gate
case class OR(a: String, b: String) extends Gate
case class XOR(a: String, b: String) extends Gate

lazy val initGates: Map[String, Gate] = input
  .collect:
    case s"$a AND $b -> $out" => out -> AND(a, b)
    case s"$a OR $b -> $out"  => out -> OR(a, b)
    case s"$a XOR $b -> $out" => out -> XOR(a, b)
  .toMap[String, Gate]

initGates.size

lazy val zGates: List[(String, Gate)] = initGates.toList
  .filter:
    case (out, gate) => out.startsWith("z")
  .sortBy(_._1)
  .reverse

type State = Map[String, Boolean]

def eval(wire: String, state: State): (Boolean, State) =
  if state.contains(wire) then state(wire) -> state
  else eval(initGates(wire), state)

def eval(gate: Gate, state: State): (Boolean, State) = gate match
  case AND(a, b) =>
    eval(a, state).pipe:
      case (va, state) =>
        eval(b, state).pipe:
          case (vb, state) => (va && vb) -> state
  case OR(a, b)  =>
    eval(a, state).pipe:
      case (va, state) =>
        eval(b, state).pipe:
          case (vb, state) => (va || vb) -> state
  case XOR(a, b) =>
    eval(a, state).pipe:
      case (va, state) =>
        eval(b, state).pipe:
          case (vb, state) => (va ^ vb) -> state

val (ans1, _) = zGates.foldLeft[(Long, State)](0L, initWires):
  case ((value, state), (out, gate)) =>
    val (v, s) = eval(gate, state)
    ((value << 1) | (if v then 1L else 0L)) -> s

file.close()
