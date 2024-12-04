import scala.io.Source

type Grid = List[List[Char]]
val XMAS = Array('X', 'M', 'A', 'S')

// (0, 0) is top left
enum Direction(val xOffset: Int, val yOffset: Int) {
  case TOP extends Direction(0, -1)
  case RIGHT extends Direction(1, 0)
  case BOTTOM extends Direction(0, 1)
  case LEFT extends Direction(-1, 0)

  case TOPRIGHT extends Direction(1, -1)
  case BOTTOMRIGHT extends Direction(1, 1)
  case BOTTOMLEFT extends Direction(-1, 1)
  case TOPLEFT extends Direction(-1, -1)
}

import Direction._

@main def main = {
  val grid: Grid = Source.fromFile("input").getLines.toList.map(line => line.toList)
  val width = grid(0).length
  val height = grid.length

  val count = (for {
    x <- 0 until width
    y <- 0 until height
    dir <- List(TOP, RIGHT, BOTTOM, LEFT, TOPRIGHT, BOTTOMRIGHT, BOTTOMLEFT, TOPLEFT)
  } yield {
    // println("---")
    isXmasAtPosition(grid, width, height, x, y, dir)
  }).filter(identity).length

  println(count)
}

def isXmasAtPosition(grid: Grid, width: Int, height: Int, x: Int, y: Int, direction: Direction): Boolean = {
  val debug = false // x == 4 && y == 0 && direction == BOTTOMRIGHT;

  // No short-circuit for now
  val result = (for {
    xmasIndex <- 0 until 4
  } yield {
    if (debug) {
      println(s"Searching for ${XMAS(xmasIndex)} at position (${x + (xmasIndex * direction.xOffset)}, ${y + (xmasIndex * direction.yOffset)}) in direction $direction")
    }
    val found = isCharacterAtPosition(grid, width, height, x + (xmasIndex * direction.xOffset), y + (xmasIndex * direction.yOffset), XMAS(xmasIndex), debug)
    // if (found) {
    //   println(s"XMAS character ${XMAS(xmasIndex)} found at position ($x, $y) in direction $direction")
    // }
    found
  }).forall(identity)

  if (debug) {
    println(s"XMAS ${if (result) "!!! " else "not "}found at position ($x, $y) in direction $direction")
  }

  result
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
