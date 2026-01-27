package xiangshan.frontend.bpu.mbtb.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.mbtb.prefetch._
import xiangshan.frontend.ftq.FtqEntry
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.frontend.icache.BtbPrefetchBundle

class PrefetchBtb(implicit p: Parameters) extends BasePredictor with Helpers {
  class PrefetchBtbIO(implicit p: Parameters) extends BasePredictorIO {
    val flush:  Bool                   = Input(Bool())
    val result: Vec[Valid[Prediction]] = Output(Vec(NumWay, Valid(new Prediction)))
    val meta:   PrefetchBtbMeta        = Output(new PrefetchBtbMeta)
    // prefetch data
    val prefetchData: Valid[BtbPrefetchBundle] = Flipped(Valid(new BtbPrefetchBundle))
    // get pc from ftq
    val prefetchBtbFtqPtr: ValidIO[FtqPtr] = Valid(new FtqPtr)
    val ftqEntry:          FtqEntry        = Input(new FtqEntry())
    val ifuPtr:            FtqPtr          = Input(new FtqPtr)

//    val victimWrite: DecoupledIO[VictimWriteReq] = Flipped(DecoupledIO(new VictimWriteReq))
  }

  val io: PrefetchBtbIO = IO(new PrefetchBtbIO)

//  io.victimWrite.ready = true.B

  val banks:        Seq[PrefetchBtbBank] = Seq.tabulate(NumBanks)(bankIdx => Module(new PrefetchBtbBank(bankIdx)))
  val prefetchPipe: PrefetchPipe         = Module(new PrefetchPipe)
  io.resetDone  := banks.map(_.io.resetDone).reduce(_ && _)
  io.trainReady := true.B
  io.meta       := DontCare
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
  private val s2_startPc = RegEnable(s1_startPc, s1_fire)
  private val s2_tag     = getTag(s2_startPc)
  private val s2_entries = RegEnable(s1_entries, s1_fire)
  s2_fire := io.stageCtrl.s2_fire && io.enable
  (io.result zip s2_entries).foreach { case (result, entry) =>
    val hit = entry.sramData.tag === s2_tag
    result.valid            := entry.valid && s2_fire
    result.bits.taken       := false.B
    result.bits.target      := getFullTarget(s2_startPc, entry.sramData.target, None)
    result.bits.attribute   := entry.sramData.attribute
    result.bits.cfiPosition := entry.sramData.position
  }

  // Train pipe
  private val t0_fire  = io.stageCtrl.t0_fire && io.enable
  private val t0_train = io.train

  private val t1_fire     = RegNext(t0_fire, init = false.B) && io.enable
  private val t1_train    = RegEnable(t0_train, t0_fire)
  private val t1_startPc  = t1_train.startPc
  private val t1_setIdx   = getSetIndex(t1_startPc)
  private val t1_bankMask = UIntToOH(getBankIndex(t1_startPc))
  private val needInvalid = t1_train.meta.prefetchBtb.entries.map { pbtb =>
    val isHit = t1_train.meta.mbtb.entries.flatten.map { mbtb =>
      mbtb.rawHit && pbtb.rawHit && mbtb.position === pbtb.position
    }.reduce(_ || _)
    isHit
  }
  // train write to invalid entry
  banks.zipWithIndex.foreach { case (bank, idx) =>
    bank.io.TrainWriteReq.valid            := t1_fire && t1_bankMask(idx).asBool
    bank.io.TrainWriteReq.bits.setIdx      := t1_setIdx
    bank.io.TrainWriteReq.bits.needInvalid := needInvalid
  }

  // Prefetch pipe
  prefetchPipe.io.flush := io.flush
  prefetchPipe.io.prefetchBtbFtqPtr <> io.prefetchBtbFtqPtr
  prefetchPipe.io.prefetchData <> io.prefetchData
  prefetchPipe.io.ftqEntry <> io.ftqEntry
  prefetchPipe.io.ifuPtr := io.ifuPtr
  private val prefetchWrite    = prefetchPipe.io.prefetchWrite
  private val prefetchBankMask = UIntToOH(prefetchWrite.bits.bankIdx)
  private val prefetchWayMask  = WireInit(VecInit(prefetchWrite.bits.entries.map(_.valid)))
  private val prefetchEntries  = WireInit(VecInit(prefetchWrite.bits.entries.map(_.bits)))
  banks.zipWithIndex.foreach { case (bank, idx) =>
    bank.io.writeReq.valid        := prefetchWrite.valid && prefetchBankMask(idx).asBool
    bank.io.writeReq.bits.setIdx  := prefetchWrite.bits.setIdx
    bank.io.writeReq.bits.wayMask := prefetchWayMask.asUInt
    bank.io.writeReq.bits.entry   := prefetchEntries
  }
}
