import scala.io.Source

case class Antenna(identifier: Char, row: Int, col: Int)
case class Position(row: Int, col: Int)

@main def main = {
  var width = 0
  var height = 0

  val antennas = Source.fromFile("input").getLines.map(lines => { height = lines.length; lines }).zipWithIndex.map { case (line, rowIndex) => {
    width = line.length
    line.zipWithIndex.flatMap { case (char, colIndex) =>
      char match {
        case '.' => None
        case c => Some(Antenna(c, rowIndex, colIndex))
      }
    }
  }}.toList.flatten

  val antennasByFrequency = antennas.groupBy(_.identifier)
  val antinodes = antennasByFrequency.flatMap { case (_, antennas) => {
    antennas.combinations(2).toList.flatMap {
      case List(a1, a2) => getAntinodes(a1, a2, width, height)
      case _ => throw new RuntimeException("expected a list of 2 antennas")
    }
  }}.toSet

  println(antinodes.size)
}

def getAntinodes(antenna1: Antenna, antenna2: Antenna, width: Int, height: Int): List[Position] = {
  val horizontalOffset = antenna1.col - antenna2.col
  val verticalOffset = antenna1.row - antenna2.row

  val backwardFromAntenna1 = LazyList.from(0).map(multiplier => {
    Position(antenna1.row + verticalOffset * multiplier, antenna1.col + horizontalOffset * multiplier)
  }).takeWhile(position => {
    position.row >= 0 && position.row < height && position.col >= 0 && position.col < width
  }).toList

  val forwardFromAntenna2 = LazyList.from(0).map(multiplier => {
    Position(antenna2.row - verticalOffset * multiplier, antenna2.col - horizontalOffset * multiplier)
  }).takeWhile(position => {
    position.row >= 0 && position.row < height && position.col >= 0 && position.col < width
  }).toList

  backwardFromAntenna1 ++ forwardFromAntenna2
}
