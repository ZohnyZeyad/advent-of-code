import scala.io.Source

val file = Source.fromFile("day19.input")
lazy val input = file.getLines.toList

val available = input(0).split(", ").toList
val desired = input.drop(2).toList

val memo = collection.mutable.Map("" -> 1L)

def possible(t: String): Long =
  lazy val recurse = available.collect:
    case a if t.startsWith(a) => possible(t.stripPrefix(a))

  memo.getOrElseUpdate(t, recurse.sum)

val ans1 = desired.count(possible(_) > 0)
val ans2 = desired.map(possible).sum
