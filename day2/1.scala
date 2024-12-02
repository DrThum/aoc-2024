import scala.io.Source

@main def main = {
  val reports = Source.fromFile("input").getLines.map(line => line.split(" ").map(_.toInt))
  val (safeReports, _) = reports.partition(isSafe)

  println(safeReports.length)
}

def isSafe(report: Array[Int]): Boolean = {
  val increasing = report.sorted
  val decreasing = increasing.reverse

  val isAllIncrOrDecr = report.sameElements(increasing) || report.sameElements(decreasing)
  val steps = report.sliding(2).toList.map { case Array(a, b) => Math.abs(a - b) }
  val isStepBetweenOneAndThree = steps.min >= 1 && steps.max <= 3

  isAllIncrOrDecr && isStepBetweenOneAndThree
}
