import scala.io.Source

@main def main = {
  val (firstList, secondList) = Source.fromFile("input").getLines.foldLeft[(List[Int], List[Int])]((Nil, Nil)) { case ((fs, ss), elm) => {
      val numbers = elm.split("\\s+")
      if (numbers.length == 2) {
        (fs :+ numbers(0).toInt, ss :+ numbers(1).toInt)
      } else {
        (fs, ss)
      }
    }
  }

  val firstListSorted = firstList.sorted
  val secondListSorted = secondList.sorted

  var total = 0
  for {
    (one, two) <- firstListSorted.zip(secondListSorted)
  } {
    total += Math.abs(one - two)
  }

  println(total)
}
