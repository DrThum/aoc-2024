import scala.io.Source
import scala.collection.mutable.ListBuffer

// (0, 0) is top left
type Grid = List[List[Char]]
case class Position(x: Int, y: Int)

enum Direction(val xOffset: Int, val yOffset: Int) {
  case TOP extends Direction(0, -1)
  case RIGHT extends Direction(1, 0)
  case BOTTOM extends Direction(0, 1)
  case LEFT extends Direction(-1, 0)
}

@main def main = {
  val grid: Grid = Source.fromFile("input").getLines.toList.map(line => line.toList)
  val width = grid(0).length
  val height = grid.length

  var startPosition = Position(0, 0)
  val startDirection = Direction.TOP
  var obstaclesBuilder = new ListBuffer[Position]()

  for {
    row <- 0 until height
    col <- 0 until width
  } {
    grid(row)(col) match {
      case '#' => obstaclesBuilder += Position(row, col)
      case '^' => {
        assert(startPosition == Position(0, 0))
        startPosition = Position(col, row)
      }
      case _ =>
    }
  }

  val gridAfterGuardHasLeft = simulateUntilGuardLeaves(grid, startPosition, startDirection)
  // printGrid(gridAfterGuardHasLeft)

  val xCount = gridAfterGuardHasLeft.map(row => row.count(_ == 'X')).sum
  println(xCount)
}

def simulateUntilGuardLeaves(grid: Grid, startPosition: Position, startDirection: Direction): Grid = {
  var newData = simulateStep(grid, startPosition, startDirection)
  while (newData._2.isDefined) {
    // printGrid(newData._1)
    newData = simulateStep(newData._1, newData._2.get, newData._3)
  }
  newData._1
}

def simulateStep(grid: Grid, position: Position, direction: Direction): (Grid, Option[Position], Direction) = {
  val width = grid(0).length
  val height = grid.length

  // 1. leaves a X where we are
  var newGrid = grid
  newGrid = newGrid.updated(position.y, newGrid(position.y).updated(position.x, 'X'))

  // 2. check next cell
  val nextPosition = Position(position.x + direction.xOffset, position.y + direction.yOffset)
  // println(s"startPos $position, direction $direction, nextPos, $nextPosition")
  //   - if it's off-grid, return (None, direction)
  if (nextPosition.x < 0 || nextPosition.x >= width || nextPosition.y < 0 || nextPosition.y >= height) {
    (newGrid, None, direction)
  } else {
    val nextCell = newGrid(nextPosition.y)(nextPosition.x)
    nextCell match {
      //   - if it's empty, ^ (starting position) or X (already been there), move there and keep direction
      case '.' | 'X' => (newGrid, Some(nextPosition), direction)
      //   - if it's #, return (position, turnRight(direction))
      case '#' => (newGrid, Some(position), turnRight(direction))
      case c => throw new RuntimeException(s"unexpected char $c")
    }
  }
}

def turnRight(direction: Direction): Direction = {
  import Direction._

  direction match {
    case TOP => RIGHT
    case RIGHT => BOTTOM
    case BOTTOM => LEFT
    case LEFT => TOP
  }
}

def printGrid(grid: Grid) = {
  println(grid.map(_.mkString("")).mkString("\n"))
}
