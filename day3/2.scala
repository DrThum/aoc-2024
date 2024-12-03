import scala.io.Source

@main def main = {
  val regex = "mul\\((\\d+),(\\d+)\\)|don't\\(\\)|do\\(\\)".r
  val lines = Source.fromFile("input").getLines.toList.mkString(" ")
  val occurrences = regex.findAllIn(lines)

  val sum = occurrences.matchData.foldLeft((0, true)) { case ((accSum, enabled), m) => {
    m.group(0) match {
      case "do()" => (accSum, true) // Enable
      case "don't()" => (accSum, false) // Disable
      case token if token.startsWith("mul") => {
        if (enabled) { // Enabled: add the current match to the sum
          val matchValue = m.group(1).toInt * m.group(2).toInt
          (accSum + matchValue, enabled)
        } else { // Disabled: don't do anything
          (accSum, enabled)
        }
      }
    }
  }}

  println(sum._1)
}
