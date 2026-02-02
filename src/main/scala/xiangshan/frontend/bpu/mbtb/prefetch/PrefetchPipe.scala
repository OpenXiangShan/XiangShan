package xiangshan.frontend.bpu.mbtb.prefetch

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.SeqBoolBitwiseOps
import freechips.rocketchip.util.SeqToAugmentedSeq
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.mbtb.prefetch.PrefetchBtbModule
import xiangshan.frontend.ftq.FtqEntry
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.frontend.icache.BtbPrefetchBundle

class PrefetchPipe(implicit p: Parameters) extends PrefetchBtbModule with Helpers {
  class PrefetchIO(implicit p: Parameters) extends PrefetchBtbBundle {
    val flush:             Bool                     = Input(Bool())
    val prefetchData:      Valid[BtbPrefetchBundle] = Flipped(Valid(new BtbPrefetchBundle))
    val prefetchBtbFtqPtr: ValidIO[FtqPtr]          = Valid(new FtqPtr)
//    val ifuPtr:            FtqPtr                    = Input(new FtqPtr)
    val ftqEntry:      FtqEntry                  = Input(new FtqEntry())
    val prefetchWrite: ValidIO[PrefetchWriteReq] = Valid(new PrefetchWriteReq)

  }

  val io: PrefetchIO = IO(new PrefetchIO)

  private val preDecoder                   = Module(new SBD)
  private val prefetchWrite                = io.prefetchWrite
  private val s0_valid, s1_valid, s2_valid = Wire(Bool())
  private val s0_isNextLine                = io.prefetchData.bits.isNextLine
  // if this prefetch valid
  s0_valid := io.prefetchData.valid
  s1_valid := RegNext(s0_valid)
  s2_valid := RegNext(s1_valid)

  // Get ftq entry
  io.prefetchBtbFtqPtr.valid := io.prefetchData.valid
  io.prefetchBtbFtqPtr.bits  := io.prefetchData.bits.ftqIdx

  /* S1:Get pc and decode data
   *  */
  private val s1_isNextLine = RegEnable(s0_isNextLine, s0_valid)
  private val s1_startPc =
    Mux(s1_isNextLine, getBlockPc(io.ftqEntry.startPc + (CacheLineSize / 8).U), io.ftqEntry.startPc)
  private val s1_cfiOffset = WireInit(io.ftqEntry.takenCfiOffset)
  private val s1_data      = RegEnable(io.prefetchData.bits.data, s0_valid)
  private val s1_isRVC     = RegEnable(io.prefetchData.bits.maybeRvcMap, s0_valid).asBools

  // default at most 32 inst per cacheline
  private val s1_cutInst       = Wire(Vec(ICacheLineBytes / 2, UInt(32.W)))
  private val s1_dataVec       = s1_data.asTypeOf(Vec(ICacheLineBytes / 2, UInt(16.W)))
  private val s1_finalInst     = s1_cutInst
  private val s1_instValidOff0 = Wire(Vec(ICacheLineBytes / 2, Bool()))
  private val s1_instValidOff2 = Wire(Vec(ICacheLineBytes / 2, Bool()))
  private val s1_instRange     = Wire(Vec(ICacheLineBytes / 2, Bool()))
  private val s1_startRange    = Wire(Vec(ICacheLineBytes / 2, Bool()))
  private val s1_endRange      = Wire(Vec(ICacheLineBytes / 2, Bool()))
  // invalid inst range which need to decode
  private val s1_shadowRange = Wire(Vec(ICacheLineBytes / 2, Bool()))
  // assume inst 0 is valid
  private val s1_finalInstValidOff0 = Wire(Vec(ICacheLineBytes / 2, Bool()))
  // assume inst 0 is invalid,which means inst0 is a cut rvi inst
  private val s1_finalInstValidOff2 = Wire(Vec(ICacheLineBytes / 2, Bool()))
  private val s1_finalInstValid     = Wire(Vec(ICacheLineBytes / 2, Bool()))

  s1_cfiOffset.bits := Mux(
    io.ftqEntry.takenCfiOffset.valid && !s1_isNextLine,
    io.ftqEntry.takenCfiOffset.bits,
    ((1 << s1_cfiOffset.bits.getWidth) - 1).U
  )
  s1_startRange := (Fill(FetchBlockInstNum, 1.U(1.W)) << getPosition(s1_startPc))(FetchBlockInstNum - 1, 0).asBools
  s1_endRange   := (Fill(FetchBlockInstNum, 1.U(1.W)) >> (~s1_cfiOffset.bits).asUInt).asBools
  // TODO:simplify logic
  s1_instRange   := (s1_startRange.asUInt & s1_endRange.asUInt).asBools
  s1_shadowRange := s1_instRange.map(!_)
  (0 until ICacheLineBytes / 2).foreach { i =>
    if (i == 0) {
      s1_instValidOff0(i) := true.B
      s1_instValidOff2(i) := false.B
    } else {
      s1_instValidOff0(i) := !s1_instValidOff0(i - 1) || s1_isRVC(i - 1)
      s1_instValidOff2(i) := !s1_instValidOff2(i - 1) || s1_isRVC(i - 1)
    }
    // TODO:CHECK LOGIC
    if (i == 0) {
      s1_finalInstValidOff0(i) := s1_shadowRange(i)
    } else if (i == (ICacheLineBytes / 2 - 1)) {
      s1_finalInstValidOff0(i) := false.B
    } else {
      s1_finalInstValidOff0(i) := s1_instValidOff0(i) && s1_shadowRange(i)
    }
    if (i == 0) {
      s1_finalInstValidOff2(i) := false.B
    } else if (i == (ICacheLineBytes / 2 - 1)) {
      s1_finalInstValidOff2(i) := false.B
    } else {
      s1_finalInstValidOff2(i) := s1_instValidOff2(i) && s1_shadowRange(i)
    }
  }

  (0 until ICacheLineBytes / 2 - 1).foreach(i =>
    s1_cutInst(i) := Cat(s1_dataVec(i + 1), s1_dataVec(i))
  )
  s1_cutInst(ICacheLineBytes / 2 - 1) := Cat(s1_dataVec(0), s1_dataVec(ICacheLineBytes / 2 - 1))

  preDecoder.io.req.valid               := s1_valid
  preDecoder.io.req.bits.isRvc          := s1_isRVC
  preDecoder.io.req.bits.data           := s1_finalInst
  preDecoder.io.req.bits.instrValidOff0 := s1_finalInstValidOff0
  preDecoder.io.req.bits.instrValidOff2 := s1_finalInstValidOff2
  s1_finalInstValid                     := preDecoder.io.resp.instrValid
  private val s1_branchInfo = preDecoder.io.resp.pd
  private val s1_jumpOffset = preDecoder.io.resp.jumpOffset

  /* S2:Find first 4 valid branch and Build entry
   * */
  private val s2_finalInstrValid = RegEnable(s1_finalInstValid, s1_valid)
  private val s2_branchInfo      = RegEnable(s1_branchInfo, s1_valid)
  private val s2_jumpOffset      = RegEnable(s1_jumpOffset, s1_valid)
  private val s2_startPc         = RegEnable(s1_startPc, s1_valid)

  private val s2_shadowBranchMask = (s2_branchInfo zip s2_finalInstrValid).map { case (info, valid) =>
    info.valid && (info.brAttribute.isDirect) && valid // only use direct branch
  }
  private val s2_revBranchInfo = s2_branchInfo.reverse

  private val s2_ranks = Wire(Vec(ICacheLineBytes / 2, UInt(log2Ceil(ICacheLineBytes / 2).W)))
  for (i <- 0 until ICacheLineBytes / 2) {
    s2_ranks(i) := PopCount(s2_shadowBranchMask.reverse.asUInt(i, 0))
  }
  private val debug_s1_startRange       = Wire(UInt((ICacheLineBytes / 2).W))
  private val debug_s1_endRange         = Wire(UInt((ICacheLineBytes / 2).W))
  private val debug_s1InstValidOff0     = Wire(UInt((ICacheLineBytes / 2).W))
  private val debug_s1InstValidOff2     = Wire(UInt((ICacheLineBytes / 2).W))
  private val debug_s1InstRange         = Wire(UInt((ICacheLineBytes / 2).W))
  private val debug_s1ShadowRange       = Wire(UInt((ICacheLineBytes / 2).W))
  private val debug_s1FinalInstValid    = Wire(UInt((ICacheLineBytes / 2).W))
  private val debug_s2_shadowBranchMask = Wire(UInt((ICacheLineBytes / 2).W))
  private val debug_s2_writeCfiPc       = Wire(Vec(NumWay, PrunedAddr(VAddrBits)))
  // TODO:CHECK timing
  for (i <- 1 to NumWay) {
    val matchOH = (0 until ICacheLineBytes / 2).map(n => s2_shadowBranchMask.reverse(n) && s2_ranks(n) === i.U)
    val idx     = Wire(UInt(CfiPositionWidth.W)).suggestName(s"s2_reverseIdx${i - 1}")
    idx := OHToUInt(matchOH)
    val buildValid = matchOH.reduce(_ || _)
    prefetchWrite.bits.entries(i - 1).valid                   := s2_revBranchInfo(idx).valid && buildValid
    prefetchWrite.bits.entries(i - 1).bits.victim             := false.B
    prefetchWrite.bits.entries(i - 1).bits.valid              := s2_revBranchInfo(idx).valid && buildValid
    prefetchWrite.bits.entries(i - 1).bits.sramData.position  := ~idx
    prefetchWrite.bits.entries(i - 1).bits.sramData.attribute := s2_revBranchInfo(idx).brAttribute
    prefetchWrite.bits.entries(i - 1).bits.sramData.target    := s2_jumpOffset.reverse(idx).asUInt
    prefetchWrite.bits.entries(i - 1).bits.sramData.tag       := getTag(s2_startPc)
    debug_s2_writeCfiPc(i - 1) := s2_jumpOffset.reverse(idx) + getCfiPc(getBlockPc(s2_startPc), ~idx)
  }

  prefetchWrite.valid        := s2_valid && s2_shadowBranchMask.reduce(_ || _)
  prefetchWrite.bits.bankIdx := getBankIndex(s2_startPc)
  prefetchWrite.bits.setIdx  := getSetIndex(s2_startPc)

  debug_s1_startRange       := s1_startRange.asUInt
  debug_s1_endRange         := s1_endRange.asUInt
  debug_s1InstValidOff0     := s1_instValidOff0.asUInt
  debug_s1InstValidOff2     := s1_instValidOff2.asUInt
  debug_s1InstRange         := s1_instRange.asUInt
  debug_s1ShadowRange       := s1_shadowRange.asUInt
  debug_s1FinalInstValid    := s1_finalInstValid.asUInt
  debug_s2_shadowBranchMask := s2_shadowBranchMask.asUInt
  dontTouch(debug_s1_startRange)
  dontTouch(debug_s1_endRange)
  dontTouch(debug_s1InstValidOff0)
  dontTouch(debug_s1InstValidOff2)
  dontTouch(debug_s1InstRange)
  dontTouch(debug_s1ShadowRange)
  dontTouch(debug_s1FinalInstValid)
  dontTouch(s1_startPc)
  dontTouch(s1_isNextLine)
  dontTouch(s1_instRange)
  dontTouch(debug_s2_shadowBranchMask)
  dontTouch(debug_s2_writeCfiPc)
  dontTouch(s2_ranks)

  XSPerfAccumulate(
    "discovered_branch",
    prefetchWrite.valid,
    Seq(
      ("total", true.B, PopCount(s2_shadowBranchMask)),
      ("direct", true.B, PopCount(prefetchWrite.bits.entries.map(b => b.valid && b.bits.sramData.attribute.isDirect))),
      (
        "otherIndirect",
        true.B,
        PopCount(prefetchWrite.bits.entries.map(b => b.valid && b.bits.sramData.attribute.isOtherIndirect))
      ),
      ("call", true.B, PopCount(prefetchWrite.bits.entries.map(b => b.valid && b.bits.sramData.attribute.isCall))),
      ("return", true.B, PopCount(prefetchWrite.bits.entries.map(b => b.valid && b.bits.sramData.attribute.isReturn))),
      (
        "conditional",
        true.B,
        PopCount(prefetchWrite.bits.entries.map(b => b.valid && b.bits.sramData.attribute.isConditional))
      )
    )
  )

}
