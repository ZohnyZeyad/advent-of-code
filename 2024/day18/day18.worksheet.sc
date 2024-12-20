import scala.io.Source
import math.Integral.Implicits.infixIntegralOps
import math.Ordering.Implicits.infixOrderingOps

val file = Source.fromFile("day18.input")
lazy val input = file.getLines.toList

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

  def isInBounds(a: Area) = a.contains(this)

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

val area = Area(0 to 70, 0 to 70)

val bytes: List[Point] = input.collect:
  case s"$x,$y" => Point(x.toInt, y.toInt)

val grid1024: Set[Point] = bytes.take(1024).toSet

val start = area.topLeft
val end = area.botRight

def floodFill(blocked: Set[Point]): Iterator[Set[Point]] =
  Iterator.unfold(Set.empty[Point] -> Set(start)):
    case (prev, curr) =>
      Option.when(curr.nonEmpty):
        val next = curr
          .flatMap(_.adjacents)
          .diff(prev)
          .diff(blocked)
          .filter(_.isInBounds(area))

        curr -> (curr -> next)

val ans1 = floodFill(grid1024).indexWhere(_.contains(end))

def reachable(n: Int): Boolean =
  val blocked = bytes.take(n + 1).toSet
  floodFill(blocked).exists(_.contains(end))

def bs(range: Range): Point =
  if range.size == 1 then bytes(range.start)
  else
    val (left, right) = range.splitAt(range.size / 2)
    if reachable(left.last) then bs(right) else bs(left)

val ans2 = bs(bytes.indices.drop(1024))

file.close()
