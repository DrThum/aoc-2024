package day9_2

import scala.io.Source
import scala.collection.mutable.ListBuffer

trait Block {
  def size: Int
}
case class FreeSpace(size: Int) extends Block
case class FileBlock(id: BigInt, size: Int) extends Block

@main def main = {
  val (blocks, _, _) = Source.fromFile("input").getLines.toList.head.foldLeft[(List[Block], BigInt, Boolean)]((Nil, 0, true)) { case ((blocks, nextFileId, isReadingFile), digit) => {
      val blockLength = digit.asDigit
      if (isReadingFile) {
        (blocks ++ List(FileBlock(nextFileId, blockLength)), nextFileId + 1, false)
      } else {
        (blocks ++ List(FreeSpace(blockLength)), nextFileId, true)
      }
    }
  }

  val sortedBlocks = sortBlocks(blocks)
  val checksum = sortedBlocks.foldLeft(BigInt(0), 0) { case ((sum, currentIndex), block) =>
    block match {
      case FreeSpace(size) => (sum, currentIndex + size)
      case FileBlock(id, size) => {
        val additional = (0 until size).map(i => (i + currentIndex) * id).sum
        (sum + additional, currentIndex + size)
      }
    }
  }._1

  println(checksum)
}

def sortBlocks(blocks: List[Block]): List[Block] = {
  blocks.foldRight[List[Block]](blocks) { case (thisBlock, acc) => {
    thisBlock match {
      case _: FreeSpace => {
        acc // Free space: nothing to do
      }
      case FileBlock(_, size) => {
        findFirstBigEnoughFreeSpaceIndex(acc, size) match {
          case None => acc
          case Some(firstFreeSpaceIndex) => {
            val thisBlockIndex = acc.indexOf(thisBlock)
            // The list is packed, job's done
            if (firstFreeSpaceIndex > thisBlockIndex) {
              acc
            } else {
              val replacedFreeSpace = acc(firstFreeSpaceIndex)
              val blocksToInsert = if (replacedFreeSpace.size > thisBlock.size) {
                List(thisBlock, FreeSpace(replacedFreeSpace.size - thisBlock.size))
              } else {
                List(thisBlock)
              }

              acc.patch(thisBlockIndex, List(FreeSpace(thisBlock.size)), 1).patch(firstFreeSpaceIndex, blocksToInsert, 1)
            }
          }
        }

      }
    }
  }}
}

def findFirstBigEnoughFreeSpaceIndex(blocks: List[Block], size: Int): Option[Int] = {
  blocks.zipWithIndex.filter {
    case (FreeSpace(freeSpaceSize), index) if freeSpaceSize >= size => true
    case _ => false
  }.headOption.map(_._2)
}

def blockToString(block: Block): String = block match {
  case FreeSpace(size) => "." * size
  case FileBlock(id, size) => id.toString() * size
}
