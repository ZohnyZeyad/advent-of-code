import scala.io.Source
import math.Integral.Implicits.infixIntegralOps
import math.Ordering.Implicits.infixOrderingOps

val file = Source.fromFile("day15.input")
lazy val input = file.getLines.toList

lazy val grid = input.takeWhile(_.nonEmpty).toVector

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

  def apply(left: Int, right: Int, top: Int, bot: Int): Area =
    Area(left to right, top to bot)
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

  def isHorizontal: Boolean = this == Dir.E || this == Dir.W
  def isVertical: Boolean = this == Dir.N || this == Dir.S

val area = Area(grid)

val movements: List[Dir] = input
  .drop(area.height + 1)
  .flatMap: line =>
    line.map:
      case '^' => Dir.N
      case 'v' => Dir.S
      case '<' => Dir.W
      case '>' => Dir.E

val startPos =
  val s = area.pointsIterator.find(grid(_) == '@').get
  s.copy(x = s.x * 2)

val startBoxes =
  area.pointsIterator
    .filter(grid(_) == 'O')
    .toSet
    .map(b => b.copy(x = b.x * 2))

val walls =
  area.pointsIterator
    .filter(grid(_) == '#')
    .toSet
    .map(b => b.copy(x = b.x * 2))

def occupied2(pos: Point, boxes: Set[Point]): Boolean =
  val left = pos.move(Dir.W)
  boxes(pos) || walls(pos) || boxes(left) || walls(left)

def connectedBoxes(pos: Point, dir: Dir, boxes: Set[Point]): Set[Point] =
  // assert(dir.isVertical)
  def search(p: Point): Set[Point] =
    val n = p.move(dir)
    val nw = n.move(Dir.W)
    val ne = n.move(Dir.E)
    if boxes(n) then search(n) union search(ne) + n
    else if boxes(nw) then search(nw) union search(n) + nw
    else Set.empty[Point]
  search(pos)

def farSideBoxes2(pos: Point, dir: Dir, boxes: Set[Point]): Point =
  // assert(dir.isHorizontal)
  val start = if dir == Dir.E then pos.move(Dir.W) else pos
  Iterator
    .iterate(start)(_.move(dir, 2))
    .drop(1)
    .dropWhile(boxes)
    .next

def horizontalBoxes(pos: Point, dir: Dir, boxes: Set[Point]): Set[Point] =
  // assert(dir.isHorizontal)
  val start = if dir == Dir.E then pos.move(Dir.W) else pos
  Iterator
    .iterate(start)(_.w.w)
    .drop(1)
    .takeWhile(boxes)
    .toSet

val states = movements.zipWithIndex.scanLeft((startPos, startBoxes)):
  case ((pos, boxes), (dir, i)) =>
    assert(!(boxes(pos) || walls(pos)))
    assert(!(boxes(pos.w) || walls(pos.w)))
    // assert(boxes.map(_.e).intersect(walls).isEmpty, s"box clips walls $i")
    // assert(boxes.intersect(walls.map(_.e)).isEmpty, s"box clips walls $i")
    // assert(boxes.intersect(walls).isEmpty, s"box overlaps walls $i")
    val nextPos = pos.move(dir)
    if !occupied2(nextPos, boxes) then nextPos -> boxes
    else if walls(nextPos) || walls(nextPos.move(Dir.W)) then pos -> boxes
    else if dir.isVertical then
      val connected = connectedBoxes(pos, dir, boxes)
      assert(
        connected.nonEmpty,
        s"no connected boxes: $pos, $dir, ${boxes(nextPos)}"
      )
      val noWalls = connected.forall: b =>
        val n = b.move(dir)
        !(walls(n) || walls(n.e) || walls(n.w))

      if noWalls then
        val nextBs = (boxes -- connected) ++ connected.map(_.move(dir))
        assert(nextBs != boxes)
        // assert(nextBs.intersect(walls).isEmpty, "connected")
        nextPos -> nextBs
      else // walls
        pos -> boxes
    else
      assert(
        boxes(nextPos) || boxes(nextPos.move(Dir.W)),
        s"no box in way: ${dir}"
      )
      val farSide = dir match
        case Dir.E =>
          Iterator.iterate(pos.e)(_.e.e).dropWhile(boxes).next
        case Dir.W =>
          Iterator.iterate(pos.w.w)(_.w.w).dropWhile(boxes).next
        case _ => ???

      if walls(farSide) then pos -> boxes
      else
        val boxesToMove = dir match
          case Dir.E =>
            Iterator.iterate(pos.e)(_.e.e).takeWhile(boxes).toSet
          case Dir.W =>
            Iterator.iterate(nextPos.w)(_.w.w).takeWhile(boxes).toSet
          case _ => ???

        assert(
          boxesToMove.nonEmpty,
          s"horizontal boxes nonempty: pos: $pos, dir: $dir, ${boxes(nextPos)}, ${boxes(nextPos.w)}"
        )
        assert(boxesToMove.map(_.y).sizeIs == 1, "boxes not horizontal")

        val nextBs = (boxes -- boxesToMove) ++ boxesToMove.map(_.move(dir))

        assert(!nextBs.contains(farSide))

        // assert(nextBs.intersect(walls).isEmpty,
        //   s"east west push: intersect: ${nextBs.intersect(walls)} pos: $pos, $dir, tomove: $boxesToMove, ${boxesToMove.map(_.move(dir))} $i")
        assert(nextBs != boxes)

        nextPos -> nextBs

lazy val (finalPos2, finalBoxes2) = states.last

lazy val (positions, boxStates) = states.unzip
assert(boxStates.forall(_.size == startBoxes.size))
assert(boxStates.distinct.sizeIs > 10)

val ans2 = finalBoxes2.toList
  .map:
    case Point(x, y) => 100 * y + x
  .sum

println(ans2)

// val area2 = Area(
//   top = area.top,
//   left = area.left,
//   right = area.right * 2,
//   bot = area.bot
// )
//
// assert(boxStates.forall(_.size == startBoxes.size))
// assert(boxStates.distinct.sizeIs > 10)
// assert(states.map(_._2).distinct.sizeIs > 10)
// states.zipWithIndex.foreach:
//   case ((pos, bs), i) =>
//     println(s"$i ${movements(i)}")
//     println:
//       area2.draw: p =>
//         if pos == p then '@'
//         else if bs(p) then '['
//         else if bs(p.move(Dir.W)) then ']'
//         else if walls(p) then '#'
//         else if walls(p.move(Dir.W)) then '#'
//         else '.'

// println(ans2)
