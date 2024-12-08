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
      case List(a1, a2) => getAntinodes(a1, a2)
      case _ => throw new RuntimeException("expected a list of 2 antennas")
    }
  }}.toList

  val antinodesInGrid = antinodes.filter { position =>
    position.row >= 0 && position.row < height && position.col >= 0 && position.col < width
  }.toSet

  println(antinodesInGrid.size)
}

def getAntinodes(antenna1: Antenna, antenna2: Antenna): List[Position] = {
  val horizontalOffset = antenna1.col - antenna2.col
  val verticalOffset = antenna1.row - antenna2.row

  List(
    Position(antenna1.row + verticalOffset, antenna1.col + horizontalOffset),
    Position(antenna2.row - verticalOffset, antenna2.col - horizontalOffset)
  )
}
