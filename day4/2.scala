import scala.io.Source

type Grid = List[List[Char]]

@main def main = {
  val grid: Grid = Source.fromFile("input").getLines.toList.map(line => line.toList)
  val width = grid(0).length
  val height = grid.length

  val count = (for {
    x <- 0 until width
    y <- 0 until height
  } yield {
    isXMasAtPosition(grid, width, height, x, y)
  }).filter(identity).length

  println(count)
}

def isXMasAtPosition(grid: Grid, width: Int, height: Int, x: Int, y: Int): Boolean = {
  val debug = false // x == 3 && y == 1

  if (!isCharacterAtPosition(grid, width, height, x, y, 'A', debug)) { // We need an A at the center
    false
  } else {
    // We need M A S or S A M in the first diagonal (top-left - bottom-right)
    val diagonalBackSlashOk = {
      val MS_OK = isCharacterAtPosition(grid, width, height, x - 1, y - 1, 'M', debug) && isCharacterAtPosition(grid, width, height, x + 1, y + 1, 'S', debug)
      val SM_OK = isCharacterAtPosition(grid, width, height, x - 1, y - 1, 'S', debug) && isCharacterAtPosition(grid, width, height, x + 1, y + 1, 'M', debug)
      MS_OK || SM_OK
    }

    // We need M A S or S A M in the second diagonal (bottom-left - top-right)
    val diagonalSlashOk = {
      val MS_OK = isCharacterAtPosition(grid, width, height, x - 1, y + 1, 'M', debug) && isCharacterAtPosition(grid, width, height, x + 1, y - 1, 'S', debug)
      val SM_OK = isCharacterAtPosition(grid, width, height, x - 1, y + 1, 'S', debug) && isCharacterAtPosition(grid, width, height, x + 1, y - 1, 'M', debug)
      MS_OK || SM_OK
    }

    diagonalBackSlashOk && diagonalSlashOk
  }
}

def isCharacterAtPosition(grid: Grid, width: Int, height: Int, x: Int, y: Int, char: Char, debug: Boolean): Boolean = {
  if (x < 0 || x >= width || y < 0 || y >= height) {
    false
  } else {
    val found = grid(y)(x) == char
    if (debug) {
      println(s"isCharacterAtPosition x=$x y=$y char=$char -> $found")
    }
    found
  }
}
