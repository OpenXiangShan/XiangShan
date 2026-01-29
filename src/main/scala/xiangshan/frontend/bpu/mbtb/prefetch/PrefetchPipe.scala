package xiangshan.frontend.bpu.mbtb.prefetch

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.SeqBoolBitwiseOps
import freechips.rocketchip.util.SeqToAugmentedSeq
import org.chipsalliance.cde.config.Parameters
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

  private val preDecoder = Module(new SBD)

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
  private val s1_cfiOffset              = io.ftqEntry.takenCfiOffset
  private val s1_data                   = RegEnable(io.prefetchData.bits.data, s0_valid)
  private val s1_isRVC                  = RegEnable(io.prefetchData.bits.maybeRvcMap, s0_valid).asBools
  private val s1_prevLastHalfData       = RegInit(0.U(16.W))
  private val s1_prevLastInstrIsHalfRvi = RegInit(false.B)

  // default at most 32 inst per cacheline
  private val s1_cutInst    = Wire(Vec(ICacheLineBytes / 2, UInt(32.W)))
  private val s1_dataVec    = s1_data.asTypeOf(Vec(ICacheLineBytes / 2, UInt(16.W)))
  private val s1_finalInst  = s1_cutInst
  private val s1_instValid  = Wire(Vec(ICacheLineBytes / 2, Bool()))
  private val s1_instRange  = Wire(Vec(ICacheLineBytes / 2, Bool()))
  private val s1_startRange = Wire(Vec(ICacheLineBytes / 2, Bool()))
  private val s1_endRange   = Wire(Vec(ICacheLineBytes / 2, Bool()))
  // invalid inst range which need to decode
  private val s1_shadowRange    = Wire(Vec(ICacheLineBytes / 2, Bool()))
  private val s1_finalInstValid = Wire(Vec(ICacheLineBytes / 2, Bool()))

  private val debug_s1_startRange    = Wire(UInt((ICacheLineBytes / 2).W))
  private val debug_s1_endRange      = Wire(UInt((ICacheLineBytes / 2).W))
  private val debug_s1InstValid      = Wire(UInt((ICacheLineBytes / 2).W))
  private val debug_s1InstRange      = Wire(UInt((ICacheLineBytes / 2).W))
  private val debug_s1ShadowRange    = Wire(UInt((ICacheLineBytes / 2).W))
  private val debug_s1FinalInstValid = Wire(UInt((ICacheLineBytes / 2).W))
  debug_s1_startRange    := s1_startRange.asUInt
  debug_s1_endRange      := s1_endRange.asUInt
  debug_s1InstValid      := s1_instValid.asUInt
  debug_s1InstRange      := s1_instRange.asUInt
  debug_s1ShadowRange    := s1_shadowRange.asUInt
  debug_s1FinalInstValid := s1_finalInstValid.asUInt
  dontTouch(debug_s1_startRange)
  dontTouch(debug_s1_endRange)
  dontTouch(debug_s1InstValid)
  dontTouch(debug_s1InstRange)
  dontTouch(debug_s1ShadowRange)
  dontTouch(debug_s1FinalInstValid)
  dontTouch(s1_startPc)
  dontTouch(s1_isNextLine)

  s1_cfiOffset.bits := Mux(
    io.ftqEntry.takenCfiOffset.valid,
    io.ftqEntry.takenCfiOffset.bits,
    (1 << s1_cfiOffset.bits.getWidth - 1).U
  )
  s1_startRange := (Fill(FetchBlockInstNum, 1.U(1.W)) << getPosition(s1_startPc))(FetchBlockInstNum - 1, 0).asBools
  s1_endRange   := (Fill(FetchBlockInstNum, 1.U(1.W)) >> (~s1_cfiOffset.bits).asUInt).asBools
  // TODO:simplify logic
  s1_instRange   := (s1_startRange.asUInt & s1_endRange.asUInt).asBools
  s1_shadowRange := s1_instRange.map(!_)
  (0 until ICacheLineBytes / 2).foreach { i =>
    if (i == 0) {
      s1_instValid(i) := !s1_prevLastInstrIsHalfRvi
    } else {
      s1_instValid(i) := !s1_instValid(i - 1) || s1_isRVC(i - 1)
    }
    // TODO:CHECK LOGIC
    if (i == 0) {
      s1_finalInstValid(i) := (s1_instValid(0) | s1_prevLastInstrIsHalfRvi) && s1_shadowRange(i)
    } else if (i == (ICacheLineBytes / 2 - 1)) {
      s1_finalInstValid(i) := s1_isRVC(i) && s1_shadowRange(i)
    } else {
      s1_finalInstValid(i) := s1_instValid(i) && s1_shadowRange(i)
    }
  }

  (0 until ICacheLineBytes / 2 - 1).foreach(i =>
    if (i == 0) {
      s1_cutInst(i) := Mux(
        s1_prevLastInstrIsHalfRvi,
        Cat(s1_dataVec(0), s1_prevLastHalfData),
        Cat(s1_dataVec(i + 1), s1_dataVec(i))
      )
    } else {
      s1_cutInst(i) := Cat(s1_dataVec(i + 1), s1_dataVec(i))
    }
  )
  s1_cutInst(ICacheLineBytes / 2 - 1) := Cat(s1_dataVec(0), s1_dataVec(ICacheLineBytes / 2 - 1))
  when(io.flush) {
    s1_prevLastInstrIsHalfRvi := false.B
    s1_prevLastHalfData       := 0.U
  }.elsewhen(s1_valid) {
    s1_prevLastInstrIsHalfRvi := s1_instValid(s1_cfiOffset.bits) && !s1_isRVC(s1_cfiOffset.bits)
    s1_prevLastHalfData       := s1_dataVec(s1_cfiOffset.bits)
  }
  preDecoder.io.req.valid           := s1_valid
  preDecoder.io.req.bits.isRvc      := s1_isRVC
  preDecoder.io.req.bits.data       := s1_finalInst
  preDecoder.io.req.bits.instrValid := s1_finalInstValid
  private val s1_branchInfo = preDecoder.io.resp.pd
  private val s1_jumpOffset = preDecoder.io.resp.jumpOffset

  /* S2:Find first 4 valid branch and Build entry
   * */
  private val s2_branchInfo = RegEnable(s1_branchInfo, s1_valid)
  private val s2_jumpOffset = RegEnable(s1_jumpOffset, s1_valid)
  private val s2_startPc    = RegEnable(s1_startPc, s1_valid)

  private val s2_shadowBranchMask = WireInit(VecInit(s2_branchInfo.map { info =>
    info.valid && (info.brAttribute.isConditional || info.brAttribute.isDirect) // only use direct branch
  }))
  private val s2_revBranchInfo = s2_branchInfo.reverse

  dontTouch(s2_shadowBranchMask)
//  dontTouch(s2_revBranchInfo)
  dontTouch(s1_instRange)
  private val s2_ranks = Wire(Vec(ICacheLineBytes / 2, UInt(log2Ceil(ICacheLineBytes / 2).W)))
  for (i <- 0 until ICacheLineBytes / 2) {
    s2_ranks(i) := PopCount(s2_shadowBranchMask.reverse.asUInt(i, 0))
  }
  // TODO:CHECK timing
  for (i <- 1 to NumWay) {
    val matchOH    = (0 until ICacheLineBytes / 2).map(n => s2_shadowBranchMask.reverse(i) && s2_ranks(i) === i.U)
    val idx        = OHToUInt(matchOH)
    val buildValid = matchOH.reduce(_ || _)
    prefetchWrite.bits.entries(i - 1).valid                   := s2_revBranchInfo(idx).valid && buildValid
    prefetchWrite.bits.entries(i - 1).bits.victim             := false.B
    prefetchWrite.bits.entries(i - 1).bits.valid              := s2_revBranchInfo(idx).valid && buildValid
    prefetchWrite.bits.entries(i - 1).bits.sramData.position  := idx
    prefetchWrite.bits.entries(i - 1).bits.sramData.attribute := s2_revBranchInfo(idx).brAttribute
    prefetchWrite.bits.entries(i - 1).bits.sramData.target    := s2_jumpOffset(idx).asUInt
    prefetchWrite.bits.entries(i - 1).bits.sramData.tag       := getTag(s2_startPc)
  }
  prefetchWrite.valid        := s2_valid
  prefetchWrite.bits.bankIdx := getBankIndex(s2_startPc)
  prefetchWrite.bits.setIdx  := getSetIndex(s2_startPc)
}
