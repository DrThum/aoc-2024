package day10_1

import scala.io.Source

@main def main = {
  val stones = Source.fromFile("input").getLines.toList.head.split(" ").map(BigInt(_)).toList

  val finalResult = (0 until 25).foldLeft(stones) { case (acc, _) =>
    blink(acc)
  }

  println(finalResult.length)
}

def blink(stones: List[BigInt]): List[BigInt] = {
  stones.flatMap(transformStone)
}

def transformStone(stone: BigInt): List[BigInt] = {
  stone.toString match {
    case "0" => List(BigInt(1))
    case str if (str.length % 2 == 0) => str.grouped(str.length / 2).map(BigInt(_)).toList
    case str => List(BigInt(str) * 2024)
  }
}
