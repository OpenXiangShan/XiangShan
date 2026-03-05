package xiangshan.frontend.bpu.mbtb.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSError
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.BpuRedirect
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.mbtb.MainBtbMeta
import xiangshan.frontend.bpu.mbtb.TakenCounter
import xiangshan.frontend.bpu.mbtb.prefetch._
import xiangshan.frontend.ftq.FtqEntry
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.frontend.icache.BtbPrefetchBundle

class PrefetchBtb(implicit p: Parameters) extends BasePredictor with Helpers {
  class PrefetchBtbIO(implicit p: Parameters) extends BasePredictorIO {
    val redirect:                ValidIO[BpuRedirect]   = Flipped(Valid(new BpuRedirect))
    val redirectFromIfu:         Bool                   = Input(Bool())
    val redirectPrefetchBtbMeta: PrefetchBtbMeta        = Input(new PrefetchBtbMeta)
    val result:                  Vec[Valid[Prediction]] = Output(Vec(NumWay, Valid(new Prediction)))
    val meta:                    PrefetchBtbMeta        = Output(new PrefetchBtbMeta)
    val used:                    Vec[Bool]              = Output(Vec(NumWay, Bool()))

    // prefetch data
    val prefetchData: Valid[BtbPrefetchBundle] = Flipped(Valid(new BtbPrefetchBundle))
    // get pc from ftq
    val prefetchBtbFtqPtr: ValidIO[FtqPtr] = Valid(new FtqPtr)
    val ftqEntry:          FtqEntry        = Input(new FtqEntry())

    val s3_inValid:   Vec[Bool] = Input(Vec(NumWay, Bool()))
    val s3_takenMask: Vec[Bool] = Input(Vec(NumBtbResultEntries, Bool()))
  }

  val io: PrefetchBtbIO = IO(new PrefetchBtbIO)

//  io.victimWrite.ready = true.B

  val banks:        Seq[PrefetchBtbBank] = Seq.tabulate(NumBanks)(bankIdx => Module(new PrefetchBtbBank(bankIdx)))
  val prefetchPipe: PrefetchPipe         = Module(new PrefetchPipe)
  val replacer:     PrefetchBtbReplacer  = Module(new PrefetchBtbReplacer)
  io.resetDone  := banks.map(_.io.resetDone).reduce(_ && _)
  io.trainReady := true.B

  // Predict pipe
  private val s0_fire, s1_fire, s2_fire = Wire(Bool())

  /* *** s0 ***
   * calculate set_idx,bank_idx and tag
   * send read request to Banks
   */
  s0_fire := io.stageCtrl.s0_fire && io.enable
  private val s0_startPc  = io.startPc
  private val s0_setIdx   = getSetIndex(s0_startPc)
  private val s0_bankIdx  = getBankIndex(s0_startPc)
  private val s0_bankMask = UIntToOH(s0_bankIdx)
  banks.zipWithIndex.foreach { case (bank, idx) =>
    bank.io.readReq.valid       := s0_fire && s0_bankMask(idx)
    bank.io.readReq.bits.setIdx := s0_setIdx
  }

  /* *** s1 ***
   * just wait bank resp
   */
  s1_fire := io.stageCtrl.s1_fire && io.enable
  private val s1_startPc  = RegEnable(s0_startPc, s0_fire)
  private val s1_bankMask = RegEnable(s0_bankMask, s0_fire)
  private val s1_entries  = Mux1H(s1_bankMask, banks.map(_.io.readResp.entries))
  /* *** s2 ***
   * receive read response from Banks
   * send out prediction result and meta info
   */
  private val s2_startPc    = RegEnable(s1_startPc, s1_fire)
  private val s2_tag        = getTag(s2_startPc)
  private val s2_entries    = RegEnable(s1_entries, s1_fire)
  private val s2_InstOffset = getPosition(s2_startPc)
  private val s2_setIdx     = getSetIndex(s2_startPc)
  private val s2_bankMask   = UIntToOH(getBankIndex(s2_startPc))
  private val s2_halfAlign  = s2_startPc(FetchBlockSizeWidth - 1).asBool
  dontTouch(s2_halfAlign)
  s2_fire := io.stageCtrl.s2_fire && io.enable
  (io.result zip s2_entries zip io.meta.entries zip io.used).foreach { case (((result, entry), meta), used) =>
    /*prefetch btb pos is 64B ALIGN
    e.g.
    startPC:0x10, postion = entry.sramData.position
    startPC:0x30, postion = entry.sramData.position - 16 =
    Cat(0,entry.sramData.position(CfiAlignedPositionWidth-1,0)
    TODO: add align bank to increase pred res
     */
    val hit = entry.valid && entry.sramData.tag === s2_tag && entry.sramData.position >= s2_InstOffset
    val position = Mux(
      s2_halfAlign,
      Cat(~s2_halfAlign, entry.sramData.position(CfiAlignedPositionWidth - 1, 0)),
      entry.sramData.position
    )
    result.valid            := s2_fire && hit
    result.bits.taken       := meta.counter.isPositive
    result.bits.target      := getFullTarget(s2_startPc, entry.sramData.target, None)
    result.bits.attribute   := entry.sramData.attribute
    result.bits.cfiPosition := position
    meta.rawHit             := hit
    meta.position           := position
    meta.counter            := Mux(entry.used, entry.counter, TakenCounter.WeakPositive)
    meta.attribute          := entry.sramData.attribute
    used                    := s2_fire && hit && entry.used

  }

  private val s3_takenMask      = VecInit(io.s3_takenMask.slice(NumMBtbResultEntries, NumBtbResultEntries))
  private val s3_fire           = io.stageCtrl.s3_fire
  private val s3_replacerSetIdx = RegEnable(getReplacerSetIndex(s2_startPc), s2_fire)
  private val s3_setIdx         = RegEnable(s2_setIdx, s2_fire)
  private val s3_bankMask       = RegEnable(s2_bankMask, s2_fire)

  // touch taken entries only: not-taken conditional entries are considered not very useful and should be killed first
  replacer.io.predictTouch.valid        := s3_fire && s3_takenMask.reduce(_ || _)
  replacer.io.predictTouch.bits.setIdx  := s3_replacerSetIdx
  replacer.io.predictTouch.bits.wayMask := s3_takenMask.asUInt

  // Invalid pipe: invalid illegal branch inst

  private val i0_fire     = io.redirect.valid && io.redirectFromIfu && io.enable
  private val i0_setIdx   = getSetIndex(io.redirect.bits.cfiPc)
  private val i0_bankMask = UIntToOH(getBankIndex(io.redirect.bits.cfiPc))
  private val i0_position = getPosition(io.redirect.bits.cfiPc)
  private val i0_needValid = io.redirectPrefetchBtbMeta.entries.map { entry =>
    entry.rawHit && entry.position === i0_position
  }
  banks.zipWithIndex.foreach { case (bank, idx) =>
    bank.io.ifuInvalidReq.valid            := i0_fire && i0_bankMask(idx).asBool
    bank.io.ifuInvalidReq.bits.setIdx      := i0_setIdx
    bank.io.ifuInvalidReq.bits.needInvalid := i0_needValid
  }

  // Train pipe :aim to avoid multi hit TODO: invalid earlier
  private val t0_fire  = io.stageCtrl.t0_fire && io.enable
  private val t0_train = io.train

  private val t1_fire           = RegNext(t0_fire, init = false.B) && io.enable
  private val t1_train          = RegEnable(t0_train, t0_fire)
  private val t1_meta           = t1_train.meta.prefetchBtb.entries
  private val t1_branches       = t1_train.branches
  private val t1_startPc        = t1_train.startPc
  private val t1_setIdx         = getSetIndex(t1_startPc)
  private val t1_bankMask       = UIntToOH(getBankIndex(t1_startPc))
  private val t1_newCounters    = Wire(Vec(NumWay, TakenCounter()))
  private val t1_counterWayMask = Wire(Vec(NumWay, Bool()))
  private val t1_usedWayMask    = Wire(Vec(NumWay, Bool()))
  private val t1_used           = Wire(Vec(NumWay, Bool()))
  private val t1_needInvalid = t1_train.meta.prefetchBtb.entries.map { pbtb =>
    val isHit = t1_train.meta.mbtb.entries.flatten.map { mbtb =>
      mbtb.rawHit && pbtb.rawHit && mbtb.position === pbtb.position
    }.reduce(_ || _)
    isHit && t1_fire
  }
  XSError(
    PopCount(t1_needInvalid) >= 1.U,
    "train stage pbtb meta error,maybe s3 stage do not correct meta"
  )
  t1_meta.zipWithIndex.foreach { case (meta, i) =>
    val hitMask = t1_branches.map { branch =>
      branch.valid && branch.bits.attribute.isConditional && meta.position === branch.bits.cfiPosition &&
      meta.rawHit
    }
    val usedMask = t1_branches.map { branch =>
      branch.valid && meta.position === branch.bits.cfiPosition &&
      meta.rawHit
    }
    val actualTaken = Mux1H(hitMask, t1_branches.map(_.bits.taken))

    t1_counterWayMask(i) := hitMask.reduce(_ || _)
    t1_usedWayMask(i)    := usedMask.reduce(_ || _)
    t1_newCounters(i)    := meta.counter.getUpdate(actualTaken)
    t1_used(i)           := usedMask(i)
  }
  // train write to invalid entry
  // TODO: remove used signals
  banks.zipWithIndex.foreach { case (bank, idx) =>
    // invalid in s3 stage
    bank.io.trainInvalidReq.valid            := s3_fire && s3_bankMask(idx).asBool && io.s3_inValid.reduce(_ || _)
    bank.io.trainInvalidReq.bits.setIdx      := s3_setIdx
    bank.io.trainInvalidReq.bits.needInvalid := io.s3_inValid

    bank.io.trainCounterReq.valid         := t1_fire && t1_bankMask(idx).asBool && t1_counterWayMask.reduce(_ || _)
    bank.io.trainCounterReq.bits.setIdx   := t1_setIdx
    bank.io.trainCounterReq.bits.wayMask  := t1_counterWayMask.asUInt
    bank.io.trainCounterReq.bits.counters := t1_newCounters
    bank.io.trainUsedReq.valid            := t1_fire && t1_bankMask(idx).asBool && t1_usedWayMask.reduce(_ || _)
    bank.io.trainUsedReq.bits.setIdx      := t1_setIdx
    bank.io.trainUsedReq.bits.wayMask     := t1_usedWayMask.asUInt
    bank.io.trainUsedReq.bits.used        := t1_used
  }
  // Prefetch pipe
  prefetchPipe.io.flush := io.redirect.valid
  prefetchPipe.io.prefetchBtbFtqPtr <> io.prefetchBtbFtqPtr
  prefetchPipe.io.prefetchData <> io.prefetchData
  prefetchPipe.io.ftqEntry <> io.ftqEntry

//  prefetchPipe.io.ifuPtr := io.ifuPtr
  private val w0_prefetchWrite    = prefetchPipe.io.prefetchWrite
  private val w0_prefetchBankMask = UIntToOH(w0_prefetchWrite.bits.bankIdx)
  private val w0_prefetchWayMask  = WireInit(VecInit(w0_prefetchWrite.bits.entries.map(_.valid)))
  private val w0_prefetchEntries  = WireInit(VecInit(w0_prefetchWrite.bits.entries.map(_.bits)))

  replacer.io.writeMask.valid := w0_prefetchWrite.valid
  replacer.io.writeMask.bits  := w0_prefetchWayMask.asUInt
  private val victimWayMask = replacer.io.victim.wayMask

  private val w1_prefetchWrite    = RegNext(w0_prefetchWrite)
  private val w1_prefetchBankMask = RegNext(w0_prefetchBankMask)
  private val w1_prefetchWayMask  = RegNext(w0_prefetchWayMask)
  private val w1_prefetchEntries  = RegNext(w0_prefetchEntries)
  private val w1_victimWayMask    = RegNext(victimWayMask)
  private val w1_writeEntries     = Wire(Vec(NumWay, new PrefetchBtbEntry))

  for (i <- 0 until NumWay) {
    val writeIdx = if (i == 0) 0.U else PopCount(w1_victimWayMask(i - 1, 0))
    w1_writeEntries(i) := w1_prefetchEntries(writeIdx)
  }
  banks.zipWithIndex.foreach { case (bank, idx) =>
    bank.io.writeEntryReq.valid        := w1_prefetchWrite.valid && w1_prefetchBankMask(idx).asBool
    bank.io.writeEntryReq.bits.setIdx  := w1_prefetchWrite.bits.setIdx
    bank.io.writeEntryReq.bits.wayMask := w1_victimWayMask.asUInt
    bank.io.writeEntryReq.bits.entry   := w1_writeEntries
  }

  replacer.io.writeTouch.valid        := w1_prefetchWrite.valid
  replacer.io.writeTouch.bits.setIdx  := w1_prefetchWrite.bits.replacerSetIdx
  replacer.io.writeTouch.bits.wayMask := w1_victimWayMask

  dontTouch(w1_writeEntries)
//  dontTouch(w0_prefetchWayMask)
//  private val prefetchBtbTrace         = Wire(new PrefetchBtbBtbTrace)
//
//  prefetchBtbTrace.startPc      :=
//  prefetchBtbTrace.setIdx       := w1_prefetchWrite.bits.setIdx
//  prefetchBtbTrace.bankIdx      := OHToUInt(w1_prefetchBankMask)
//  prefetchBtbTrace.wayIdx       := finalTrace.wayIdx
//  prefetchBtbTrace.attribute    := finalTrace.entry.attribute
//  prefetchBtbTrace.cfiPosition  := finalTrace.entry.position
//
//  private val mbtbTraceDBTable = ChiselDB.createTable("MBTBTrace", new MainBtbTrace(), EnableMainbtbTrace)
//  mbtbTraceDBTable.log(
//    data = mbtbTrace,
//    en = t1_fire && finalTrace.needWrite,
//    clock = clock,
//    reset = reset
//  )

  val debug_prefetchMispred = t1_train.meta.prefetchBtb.entries.map { pbtb =>
    val isprefetchBtb = t1_train.meta.mbtb.entries.flatten.map { mbtb =>
      !mbtb.rawHit && pbtb.rawHit && pbtb.position === t1_train.mispredictBranch.bits.cfiPosition &&
      t1_train.mispredictBranch.valid
    }.reduce(_ || _)
    isprefetchBtb
  }.reduce(_ || _)
  XSPerfAccumulate(
    "prefetch_hit_mbtb",
    PopCount(io.s3_inValid.map(_ && s3_fire))
  )
  XSPerfAccumulate(
    "prefetch_invalid",
    PopCount(i0_needValid.map(_ && i0_fire))
  )
  XSPerfHistogram(
    "predict_valid",
    PopCount(io.result.map(_.valid)),
    io.result.map(_.valid).reduce(_ || _),
    0,
    NumWay
  )
  XSPerfAccumulate(
    "predict_branch",
    io.result.map(_.valid).reduce(_ || _),
    Seq(
      ("total", true.B, PopCount(io.result.map(res => res.valid))),
      ("direct", true.B, PopCount(io.result.map(res => res.valid && res.bits.attribute.isDirect))),
      (
        "otherIndirect",
        true.B,
        PopCount(io.result.map(res => res.valid && res.bits.attribute.isOtherIndirect))
      ),
      ("call", true.B, PopCount(io.result.map(res => res.valid && res.bits.attribute.isCall))),
      ("return", true.B, PopCount(io.result.map(res => res.valid && res.bits.attribute.isReturn))),
      (
        "conditional",
        true.B,
        PopCount(io.result.map(res => res.valid && res.bits.attribute.isConditional))
      )
    )
  )
  XSPerfAccumulate(
    "mispredReasonPrefetchBtb",
    debug_prefetchMispred && t1_fire,
    Seq(
      ("total", true.B),
      ("direct", t1_train.mispredictBranch.bits.attribute.isDirect),
      ("otherIndirect", t1_train.mispredictBranch.bits.attribute.isOtherIndirect),
      ("call", t1_train.mispredictBranch.bits.attribute.isCall),
      ("return", t1_train.mispredictBranch.bits.attribute.isReturn),
      ("conditional", t1_train.mispredictBranch.bits.attribute.isConditional)
    )
  )
}
