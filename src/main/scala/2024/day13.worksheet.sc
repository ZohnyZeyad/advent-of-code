import scala.io.Source
import scala.annotation.tailrec

val file = Source.fromResource("day13.input")
lazy val lines = file.getLines.toList

case class Point(x: Long, y: Long) {
  def plus(p: Point) = Point(x + p.x, y + p.y)
  def minus(p: Point) = Point(x - p.x, y - p.y)

  infix def -(p: Point) = minus(p)
  infix def +(p: Point) = plus(p)
  infix def *(n: Long) = Point(x * n, y * n)
}

val aButtons = lines.collect {
  case s"Button A: X+$x, Y+$y" =>
    Point(x.toLong, y.toLong)
}

val bButtons = lines.collect {
  case s"Button B: X+$x, Y+$y" =>
    Point(x.toLong, y.toLong)
}

val prizes = lines.collect {
  case s"Prize: X=$x, Y=$y" =>
    Point(x.toLong, y.toLong)
}

case class Machine(a: Point, b: Point, prize: Point)

val machines = aButtons
  .zip(bButtons)
  .zip(prizes)
  .map:
    case ((a, b), prize) => Machine(a, b, prize)

def solveMachine(machine: Machine): Option[(Long, Long)] = {
  import machine.{ a, b, prize => p }
  for
    tb <- locally:
      val n = a.y * p.x - a.x * p.y
      val d = b.x * a.y - b.y * a.x
      Option.when(n % d == 0)(n / d)
    ta <- locally:
      val n = p.y - b.y * tb
      val d = a.y
      Option.when(n % d == 0)(n / d)
  yield
    assert(a * ta + b * tb == p)
    assert(ta > 0)
    assert(tb > 0)
    ta -> tb
}

def getPrices(btnCount: (Long, Long)): Long =
  (btnCount._1 * 3) + btnCount._2

val tokens = machines.flatMap(solveMachine).map(getPrices).sum

val newMachines = machines.map(m =>
  val off = 10000000000000L
  m.copy(prize = Point(m.prize.x + off, m.prize.y + off))
)

val newTokens = newMachines.flatMap(solveMachine).map(getPrices).sum

file.close()
