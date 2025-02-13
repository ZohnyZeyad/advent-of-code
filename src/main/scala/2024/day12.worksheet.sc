import scala.io.Source
import scala.annotation.tailrec
import math.Integral.Implicits.infixIntegralOps
import math.Ordering.Implicits.infixOrderingOps

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

  def isInBounds(area: Vector[String]): Boolean = {
    this.x >= 0 && this.y >= 0
    && this.x < area.length && this.y < area(0).length
  }

  def compare(that: Point): Int = {
    val colComparison = this.x.compare(that.x)
    if colComparison != 0 then colComparison else this.y.compare(that.y)
  }
}

object Point {
  val origin = Point(0, 0)

  extension [T](grid: IndexedSeq[IndexedSeq[T]]) def apply(p: Point): T = grid(p.y)(p.x)

  extension (grid: IndexedSeq[String]) def apply(p: Point): Char = grid(p.y)(p.x)
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

  def pointsIterator =
    for
      y <- yRange.iterator
      x <- xRange
    yield Point(x, y)
}

object Area {
  def apply(grid: IndexedSeq[IndexedSeq[?]]): Area =
    Area(xRange = grid.headOption.fold(0 to 0)(_.indices), yRange = grid.indices)

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
    Area(xRange = xs.min to xs.max, yRange = ys.min to ys.max)
}

val file = Source.fromResource("day12.input")
lazy val farm = file.getLines.toVector

lazy val points: Set[Point] =
  (for
    row <- farm.indices
    col <- farm(row).indices
  yield Point(row, col)).toSet

def region(unfenced: Set[Point]): Set[Point] = {
  val start = unfenced.head

  @tailrec
  def search(visiting: Set[Point], visited: Set[Point] = Set.empty): Set[Point] = {
    if visiting.isEmpty then visited
    else
      val next = visiting
        .flatMap(_.adjacents)
        .diff(visited)
        .filter(_.isInBounds(farm))
        .filter(np => farm(np.y)(np.x) == farm(start.y)(start.x))
      search(next, visited.union(visiting))
  }

  search(Set(start))
}

val allRegions = LazyList.unfold(points) { unfenced =>
  Option.when(unfenced.nonEmpty) {
    val r = region(unfenced)
    r -> unfenced.diff(r)
  }
}

def countPerimeter(region: Set[Point]): Long = {
  region.toList.view.map(p => p.adjacents.count(!region.contains(_))).sum
}

lazy val totalCost =
  allRegions.view.map(r => r.size.toLong * countPerimeter(r)).sum
println(totalCost)

lazy val area = Area(farm)

def countCorners(region: Set[Point]): Int = {
  val bound = Area.bounding(region).expand(1)
  val subAreas = bound.pointsIterator.toList.map(p => Area.bounding(p, p.d.r))

  val normalCorners = subAreas.count(a =>
    val c = a.pointsIterator.count(region)
    c == 1 || c == 3
  )

  val diagonalCorners = subAreas.count(a =>
    val c = a.pointsIterator.count(region)
    val inBounds = a.pointsIterator.forall(area.contains)
    c == 2
    && inBounds
    && (farm(a.topLeft) == farm(a.botRight)
      || farm(a.botLeft) == farm(a.topRight))
  )

  normalCorners + (diagonalCorners * 2)
}

def price(region: Set[Point]): Long =
  region.size.toLong * countCorners(region).toLong

lazy val discountedPrice = allRegions.map(price).sum
println(discountedPrice)

file.close()
