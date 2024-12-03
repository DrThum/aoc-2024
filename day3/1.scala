import scala.io.Source

@main def main = {
  val regex = "mul\\((\\d+),(\\d+)\\)".r
  val lines = Source.fromFile("input").getLines.toList.mkString(" ")
  val occurrences = regex.findAllIn(lines)

  val sum = occurrences.matchData.map(m => m.group(1).toInt * m.group(2).toInt).sum
  println(sum)
}
