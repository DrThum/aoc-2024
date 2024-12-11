package day9

import scala.io.Source
import scala.collection.mutable.ListBuffer

trait Block
case object FreeSpace extends Block
case class FileBlock(id: BigInt) extends Block

@main def main = {
  val (blocks, _, _) = Source.fromFile("input").getLines.toList.head.foldLeft[(List[Block], BigInt, Boolean)]((Nil, 0, true)) { case ((blocks, nextFileId, isReadingFile), digit) => {
      val blockLength = digit.asDigit
      if (isReadingFile) {
        (blocks ++ List.fill(blockLength)(FileBlock(nextFileId)), nextFileId + 1, false)
      } else {
        (blocks ++ List.fill(blockLength)(FreeSpace), nextFileId, true)
      }
    }
  }

  val sortedBlocks = sortBlocks(blocks)
  val checksum = sortedBlocks.takeWhile(_ != FreeSpace).zipWithIndex.map { case (FileBlock(id), index) =>
    index * id
  }.sum

  println(checksum)
}

def sortBlocks(blocks: List[Block]): List[Block] = {
  blocks.zipWithIndex.foldRight[List[Block]](blocks) { case ((thisBlock, thisBlockIndex), acc) => {
    val firstFreeSpaceIndex = acc.indexOf(FreeSpace)

    // The list is packed, job's done
    if (firstFreeSpaceIndex > thisBlockIndex) {
      acc
    } else {
      acc.patch(firstFreeSpaceIndex, List(thisBlock), 1).patch(thisBlockIndex, List(FreeSpace), 1)
    }
  }}
}
