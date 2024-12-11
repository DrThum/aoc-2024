package day10_1

import scala.io.Source

type Grid = List[List[Int]]

enum Direction(val xOffset: Int, val yOffset: Int) {
  case TOP extends Direction(0, -1)
  case RIGHT extends Direction(1, 0)
  case BOTTOM extends Direction(0, 1)
  case LEFT extends Direction(-1, 0)
}

case class Neighbour(row: Int, col: Int, value: Int)

@main def main = {
  val grid = Source.fromFile("input").getLines.map(line => {
    line.map(_.asDigit).toList
  }).toList

  val width = grid(0).length
  val height = grid.length

  val scores = for {
    row <- 0 until height
    col <- 0 until width
  } yield getTrailScore(grid, width, height, row, col)

  println(scores.sum)
}

def getTrailScore(grid: Grid, width: Int, height: Int, row: Int, col: Int): Int = {
  getTrailEnds(grid, width, height, row, col, 0, Nil).toSet.size
}

def getTrailEnds(grid: Grid, width: Int, height: Int, row: Int, col: Int, step: Int, trail: List[Direction]): List[(Int, Int)] = {
  import Direction._

  if (grid(row)(col) != step) {
    Nil
  } else if (step == 9) {
    List((row, col))
  } else { // Recursive case: check the next step in all four directions
    val ends = List(TOP, RIGHT, BOTTOM, LEFT).flatMap(direction =>
      getNeighbour(grid, width, height, row, col, direction, step + 1).map(neighbour =>
        // println(s"going from ($col, $row) direction $direction, trail = ${updatedTrail.map(t => (t._1, t._2)).mkString(",")}")
        getTrailEnds(grid, width, height, neighbour.row, neighbour.col, step + 1, trail :+ direction)
      )
    ).flatten

    ends
  }
}

def getNeighbour(grid: Grid, width: Int, height: Int, row: Int, col: Int, direction: Direction, expectedValue: Int): Option[Neighbour] = {
  val neighbourRow = row + direction.yOffset
  val neighbourCol = col + direction.xOffset

  if (neighbourRow < 0 || neighbourRow >= height || neighbourCol < 0 || neighbourCol >= width) {
    None
  } else {
    val neighbourValue = grid(neighbourRow)(neighbourCol)
    if (neighbourValue == expectedValue) {
      Some(Neighbour(neighbourRow, neighbourCol, grid(neighbourRow)(neighbourCol)))
    } else None
  }
}
