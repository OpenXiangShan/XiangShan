package xiangshan.backend.rename

import chipsalliance.rocketchip.config.Parameters
import chisel3.Bundle
import xiangshan.backend.Bundles.DecodedInst
import xiangshan.XSModule
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.DecodeLogic
import xiangshan._

class CompressUnit(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle() {
    val in = Vec(RenameWidth, Flipped(Valid(new DecodedInst)))
    val out = new Bundle(){
      val needRobFlags = Vec(RenameWidth, Output(Bool()))
      val instrSizes = Vec(RenameWidth, Output(UInt(log2Ceil(MaxCompressWidth + 1).W)))
    }
  })
  val enqHasExc = io.in.map(uop => !uop.bits.exceptionVec.asUInt.orR)
  val uopNoExcAfterIssue = io.in.map(uop => uop.bits.noExecException)
  val canCompress = io.in.zip(enqHasExc).zip(uopNoExcAfterIssue).map { case ((in, noExc), noExecExc) => in.valid && !CommitType.isFused(in.bits.commitType) && in.bits.lastUop && noExc && noExecExc }

  assert(MaxCompressWidth <= RenameWidth)
  val compressTable = (0 until 1 << RenameWidth).map {
    case keyCandidate =>
      val key = (0 until RenameWidth).map(idx => ((keyCandidate >> idx) & 1) == 1)
      var compressCnt = 0
      val (value, uopSizesPre) = (0 until RenameWidth).map {
        case idx =>
          if (!key(idx)) {
            compressCnt = 0
          }
          else if (key(idx) && compressCnt < MaxCompressWidth) {
            compressCnt = compressCnt + 1
          }
          else {
            compressCnt = 1
          }
          val v = if (!key(idx) || idx == RenameWidth - 1 || compressCnt == MaxCompressWidth || !key(idx + 1)) 1 else 0
          (v, if (compressCnt == 0) 1 else compressCnt)
      }.unzip

      val uopSizes = (0 until RenameWidth).map {
        case idx =>
          uopSizesPre.zip(value).drop(idx).filter(x => x._2 == 1).map(_._1).head
      }

      println("[Rename.Compress] i: " + keyCandidate + " key: " + key + " value: " + value + " uopSizes: " + uopSizes + " mask: " + mask.map(_.map(_.toBinaryString).reduce(_ + _)))

      val keyBitPat = BitPat("b" + keyCandidate.toBinaryString)
      val valueBitPat = BitPat("b" + value.map(_.toBinaryString).reverse.reduce(_ + _))
      val uopSizeBitPats = uopSizes.map(x => BitPat("b" + x.toBinaryString))

      (keyBitPat -> (valueBitPat +: uopSizeBitPats))
  }

  def X = BitPat("b0")

  val default = List.fill(RenameWidth + 1)(X)
  val decoder = DecodeLogic(VecInit(canCompress).asUInt, default, compressTable)
  val needRobFlags = Wire(UInt(RenameWidth.W))
  (valueBitPat +: uopSizeBitPats).zip(decoder).foreach {
    case (sink, source) =>
      sink := source
  }
  io.out.needRobFlags := needRobFlags.asBools
}
