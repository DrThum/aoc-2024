import scala.io.Source

@main def main = {
  val reports = Source.fromFile("input").getLines.map(line => line.split(" ").map(_.toInt))
  val (safeReports, _) = reports.partition(isOneVariantSafe)

  println(safeReports.length)
}


def isOneVariantSafe(report: Array[Int]): Boolean = {
  if (isSafe(report)) {
    true
  } else {
    for {
      index <- 0 to report.length
    } {
      if (isSafe(report.patch(index, Nil, 1))) {
        return true // It's AoC I do whatever I want
      }
    }

    false
  }
}

def isSafe(report: Array[Int]): Boolean = {
  val increasing = report.sorted
  val decreasing = increasing.reverse

  val isAllIncrOrDecr = report.sameElements(increasing) || report.sameElements(decreasing)
  val steps = report.sliding(2).toList.map { case Array(a, b) => Math.abs(a - b) }
  val isStepBetweenOneAndThree = steps.min >= 1 && steps.max <= 3

  isAllIncrOrDecr && isStepBetweenOneAndThree
}

