package day10_2

import scala.io.Source

@main def main = {
  val stones = Source.fromFile("input").getLines.toList.head.split(" ").map(BigInt(_)).toList.groupBy(identity).view.mapValues(a => BigInt(a.length)).toMap

  val finalResult = (0 until 75).foldLeft(stones) { case (acc, _) =>
    blink(acc)
  }

  println(finalResult.values.sum)
}

def blink(stones: Map[BigInt, BigInt]): Map[BigInt, BigInt] = {
  stones.foldLeft(Map.empty) { case (acc, (key, value)) =>
    val transformedStones = transformStone(key)

    transformedStones.foldLeft(acc) { case (acc2, ts) =>
      acc2.get(ts) match {
        case None => acc2 + (ts -> value)
        case Some(v) => acc2 + (ts -> (value + v))
      }
    }
  }
}

def transformStone(stone: BigInt): List[BigInt] = {
  stone.toString match {
    case "0" => List(BigInt(1))
    case str if (str.length % 2 == 0) => str.grouped(str.length / 2).map(BigInt(_)).toList
    case str => List(BigInt(str) * 2024)
  }
}
