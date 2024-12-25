import scala.io.Source

val file = Source.fromResource("day5.input")
val lines = file.getLines.toList

type RuleBook = Map[Int, List[Int]]
type PageUpdate = List[Int]

def readData(lines: List[String]): (RuleBook, List[PageUpdate]) = {
  val (rules, updates) = lines.filter(_.nonEmpty).span(_.contains("|"))

  val ruleTuples =
    rules.map(r => {
      r.split('|') match {
        case Array(p1, p2) => (p1.toInt, p2.toInt)
      }
    })

  val pageUpdates = updates.map(u => u.split(",").map(_.toInt).toList)

  def createRuleBook(tuples: List[(Int, Int)], acc: RuleBook): RuleBook = {
    if tuples.isEmpty then return acc
    else {
      val (p1, p2) = tuples.head
      createRuleBook(tuples.tail, acc.updated(p1, p2 :: acc.getOrElse(p1, Nil)))
    }
  }

  (createRuleBook(ruleTuples, Map.empty), pageUpdates)
}

val (rules, updatesList) = readData(lines)

def isCorrectOrder(update: PageUpdate): Boolean = {
  update
    .flatMap(page => {
      val rule = rules.getOrElse(page, Nil)
      rule.map(p2 => {
        val p2Idx = update.indexOf(p2)
        if p2Idx < 0 then true
        else update.indexOf(page) < p2Idx
      })
    })
    .contains(false)
}

val correctList: List[PageUpdate] = updatesList.filterNot(isCorrectOrder)

val sumOfCorrect = correctList.map(upd => upd(upd.length / 2)).sum
println(sumOfCorrect)

val incorrectList: List[PageUpdate] = updatesList.filter(isCorrectOrder)

def fixUpdateOrder(update: PageUpdate): PageUpdate = {
  def insertPage(correctedUpdate: PageUpdate, page: Int, ruleList: List[Int]): PageUpdate = {
    val insertIdx = ruleList.foldLeft(correctedUpdate.length) { (idx, rulePage) =>
      correctedUpdate.indexOf(rulePage) match {
        case -1    => idx
        case p2Idx => Math.min(idx, p2Idx)
      }
    }

    correctedUpdate.take(insertIdx) ::: (page :: correctedUpdate.drop(insertIdx))
  }

  def correctPages(remainingUpdate: PageUpdate, correctedUpdate: PageUpdate): PageUpdate = {
    remainingUpdate match {
      case Nil          => correctedUpdate
      case page :: rest =>
        val ruleList = rules.getOrElse(page, Nil)
        val newUpdates = insertPage(correctedUpdate, page, ruleList)
        correctPages(rest, newUpdates)
    }
  }

  correctPages(update, Nil)
}

val fixedList = incorrectList.map(fixUpdateOrder)
val sumOfFixed = fixedList.map(upd => upd(upd.length / 2)).sum
println(sumOfFixed)

file.close()
