import scala.io.Source
import math.Integral.Implicits.infixIntegralOps
import math.Ordering.Implicits.infixOrderingOps

val file = Source.fromFile("day16.input")
lazy val maze = file.getLines.toVector

case class Point(x: Int, y: Int) extends Ordered[Point] {
  def n = copy(y = y - 1)
  def s = copy(y = y + 1)
  def e = copy(x = x + 1)
  def w = copy(x = x - 1)

  def ne = Point(x + 1, y - 1)
  def se = Point(x + 1, y + 1)
  def nw = Point(x - 1, y - 1)
  def sw = Point(x - 1, y + 1)

  def nRepeat(n: Int) = copy(y = y - n)
  def sRepeat(n: Int) = copy(y = y + n)
  def eRepeat(n: Int) = copy(x = x + n)
  def wRepeat(n: Int) = copy(x = x - n)

  def u = n
  def d = s
  def l = w
  def r = e

  def adjacents = Set(n, s, e, w)

  def inBounds(a: Area) = a.contains(this)

  def move(dir: Dir, times: Int = 1) = dir match
    case Dir.N => nRepeat(times)
    case Dir.S => sRepeat(times)
    case Dir.E => eRepeat(times)
    case Dir.W => wRepeat(times)

  def compare(that: Point): Int = {
    val colComparison = this.x.compare(that.x)
    if (colComparison != 0) colComparison else this.y.compare(that.y)
  }
}

object Point {
  val origin = Point(0, 0)

  extension [T](grid: IndexedSeq[IndexedSeq[T]])
    def apply(p: Point): T = grid(p.y)(p.x)

  extension (grid: IndexedSeq[String])
    def apply(p: Point): Char = grid(p.y)(p.x)
}

case class Area(xRange: Range, yRange: Range) {
  def left = xRange.min
  def right = xRange.max
  def top = yRange.min
  def bot = yRange.max

  def width = xRange.size
  def height = yRange.size
  def size = width * height
  def size[N: Integral] =
    Integral[N].fromInt(width) * Integral[N].fromInt(height)

  def topLeft = Point(left, top)
  def topRight = Point(right, top)
  def botLeft = Point(left, bot)
  def botRight = Point(right, bot)

  def apply(p: Point): Boolean = contains(p)

  def contains(p: Point) =
    xRange.contains(p.x) && yRange.contains(p.y)

  def expand(n: Int): Area =
    copy(left - n to right + n, top - n to bot + n)

  def pointsIterator = for
    y <- yRange.iterator
    x <- xRange
  yield Point(x, y)
}

object Area {
  def apply(grid: IndexedSeq[IndexedSeq[?]]): Area =
    Area(
      xRange = grid.headOption.fold(0 to 0)(_.indices),
      yRange = grid.indices
    )

  def apply(grid: IndexedSeq[String])(using
      wrap: String => collection.immutable.WrappedString
  ): Area =
    apply(grid.map(wrap))
  def bounding(p: Point, q: Point) =
    val dx = q.x - p.x
    val dy = q.y - p.y
    Area(
      xRange = p.x to q.x by (if dx == 0 then 1 else dx.sign),
      yRange = p.y to q.y by (if dy == 0 then 1 else dy.sign)
    )

  def bounding(points: Set[Point]): Area =
    val xs = points.map(_.x)
    val ys = points.map(_.y)
    Area(
      xRange = xs.min to xs.max,
      yRange = ys.min to ys.max
    )
}

enum Dir:
  case E, S, W, N

  def turnRight = this match
    case Dir.E => S
    case Dir.S => W
    case Dir.W => N
    case Dir.N => E

  def turnLeft = this match
    case Dir.E => N
    case Dir.S => E
    case Dir.W => S
    case Dir.N => W

  def reverse = this match
    case Dir.E => W
    case Dir.S => N
    case Dir.W => E
    case Dir.N => S

val area = Area(maze)
val startPos = area.pointsIterator.find(maze(_) == 'S').get
val endPos = area.pointsIterator.find(maze(_) == 'E').get
val walls = area.pointsIterator.filter(maze(_) == '#').toSet

case class State(pos: Point, dir: Dir):
  def next: List[(State, Int)] =
    List(dir, dir.turnLeft, dir.turnRight)
      .zip(List(1, 1001, 1001))
      .map((d, s) => ((pos.move(d), d, s)))
      .filter((p, _, _) => p.inBounds(area) && !walls(p))
      .map((p, d, s) => State(p, d) -> s)

def search: (Int, Int) =
  import collection.mutable.{PriorityQueue, Map}

  val start = State(startPos, Dir.E)
  val minCost = Map[State, Int](start -> 0)
  val queue = PriorityQueue.empty[State](Ordering.by(minCost).reverse)

  val pathsBack = Map
    .empty[(State, Int), List[(State, Int)]]
    .withDefaultValue(Nil)

  var visiting = start
  var endCost = Int.MaxValue

  while minCost(visiting) <= endCost do
    if visiting.pos == endPos then endCost = endCost min minCost(visiting)

    visiting.next
      .filterNot: (s, c) =>
        minCost.get(s).exists(_ < minCost(visiting) + c)
      .foreach: (s, c) =>
        minCost(s) = minCost(visiting) + c
        queue.enqueue(s)
        pathsBack(s -> minCost(s)) =
          (visiting, minCost(visiting)) :: pathsBack(s -> minCost(s))

    visiting = queue.dequeue

  val endStates = Dir.values.map(State(endPos, _) -> endCost).toSet
  val seats = Iterator
    .unfold(endStates): states =>
      val nextStates = states.flatMap(pathsBack)
      val nextPositions = nextStates.map(_._1.pos)
      Option.when(nextStates.nonEmpty)(nextPositions -> nextStates)
    .reduce(_ union _) + endPos

  // println:
  //   area.draw: p =>
  //     if seats.contains(p) then '█'
  //     else if walls(p) then '#'
  //     else ' '

  (endCost, seats.size)

val (ans1, ans2) = search

file.close()
