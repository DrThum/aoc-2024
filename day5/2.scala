import scala.io.Source

case class PageOrderingRule(first: Int, second: Int)

@main def main = {
  val (rules, updates, _) = Source.fromFile("input").getLines.foldLeft[(List[PageOrderingRule], List[List[Int]], Boolean)]((Nil, Nil, false)) { case ((accRules, accUpdates, isRulesParsingDone), line) => {
    if (line.isEmpty) { // Rules and updates are separated by an empty line
      (accRules, accUpdates, true)
    } else if (isRulesParsingDone) {
      (accRules, accUpdates :+ (line.split(',').toList.map(_.toInt)), isRulesParsingDone)
    } else {
      val Array(first, second) = line.split('|').map(_.toInt)
      (accRules :+ PageOrderingRule(first, second), accUpdates, isRulesParsingDone)
    }
  }}

  // For each update, check each rule
  val invalidUpdates = updates.filterNot(update => {
    rules.forall(rule => checkRule(update, rule))
  })

  // Order each invalid update according to the rules
  val orderedUpdates = invalidUpdates.map(orderUpdate(_, rules))

  // Get the middle page of each newly ordered update
  val middlePages = orderedUpdates.map(getUpdateMiddlePage)

  println(middlePages.sum)
}

def checkRule(update: List[Int], rule: PageOrderingRule): Boolean = {
  val firstIndex = update.indexOf(rule.first)
  val secondIndex = update.indexOf(rule.second)

  (firstIndex, secondIndex) match {
    case (-1, _) | (_, -1) => true // the rule isn't applicable to the update
    case (f, s) => f < s
  }
}

def orderUpdate(update: List[Int], rules: List[PageOrderingRule]): List[Int] = {
  var orderedUpdate = update
  while (!rules.forall(checkRule(orderedUpdate, _))) {
    for (rule <- rules) {
      // Skip valid rules
      if (!checkRule(orderedUpdate, rule)) {
        val firstIndex = orderedUpdate.indexOf(rule.first)
        val secondIndex = orderedUpdate.indexOf(rule.second)

        assert(firstIndex > secondIndex)

        val firstValue = orderedUpdate(firstIndex)

        // Move firstIndex just before secondIndex
        orderedUpdate = orderedUpdate.take(secondIndex) ++ List(firstValue) ++ orderedUpdate.drop(secondIndex).filterNot(_ == firstValue)
      }
    }
  }
  orderedUpdate
}

def getUpdateMiddlePage(update: List[Int]): Int = {
  if (update.length % 2 == 0) {
    throw new RuntimeException("found update with even page number")
  }

  update(update.length / 2)
}
