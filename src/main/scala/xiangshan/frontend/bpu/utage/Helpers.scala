package xiangshan.frontend.bpu.utage

import chisel3._
import chisel3.util._
import xiangshan.HasXSParameter
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.RotateHelper
import xiangshan.frontend.bpu.phr.PhrAllFoldedHistories

trait Helpers extends HasMicroTageParameters {
  private object TAGEHistoryType {
    val Short   = 0
    val Medium  = 1
    val Long    = 2
    val Unknown = 3
  }
  def getUnhashedIdx(pc: UInt): UInt = pc(VAddrBits - 1, instOffsetBits)
  def getUnhashedTag(pc: UInt): UInt = pc(VAddrBits - 1, log2Ceil(FetchBlockAlignSize))
  def connectPcTag(pc: UInt, tableId: Int): UInt = {
    require(tableId >= 0 && tableId <= 3, s"tableId must be in [0,3], got $tableId")
    def concatBits(bits: Seq[Bool]): UInt = if (bits.isEmpty) 0.U(1.W) else bits.foldLeft(0.U(0.W))(Cat(_, _))
    val tagPC = pc
    val hashPC = tableId match {
      case TAGEHistoryType.Short =>
        concatBits(PCTagHashBitsForShortHistory.map(tagPC(_)))

      case TAGEHistoryType.Medium =>
        concatBits(PCTagHashBitsForMediumHistory.map(tagPC(_)))

      case TAGEHistoryType.Long =>
        val xorBits = PCTagHashXorPairsForLongHistory.map { case (i, j) => tagPC(i) ^ tagPC(j) }
        concatBits(xorBits)

      case _ =>
        concatBits(PCTagHashBitsDefault.map(tagPC(_)))
    }

    hashPC
  }

  def selectWriteWayIdx(numWays: Int, valids: Vec[Bool], usefuls: Vec[Bool]): UInt = {
    val idx = Wire(UInt(log2Ceil(numWays).W))
    idx := 0.U
    if (numWays == 2) {
      when(!valids(0))(idx := 0.U)
        .elsewhen(!valids(1))(idx := 1.U)
        .otherwise {
          when(!usefuls(0) && usefuls(1))(idx := 0.U)
            .elsewhen(usefuls(0) && !usefuls(1))(idx := 1.U)
            .otherwise(idx := 0.U)
        }
    }
    idx
  }
}
