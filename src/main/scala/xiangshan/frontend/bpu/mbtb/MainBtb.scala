// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu.mbtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ChiselDB
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import utils.VecRotate
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.mbtb.prefetch.PrefetchBtb
import xiangshan.frontend.bpu.mbtb.prefetch.PrefetchBtbMeta
import xiangshan.frontend.ftq.FtqEntry
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.frontend.icache.BtbPrefetchBundle

class MainBtb(implicit p: Parameters) extends BasePredictor with HasMainBtbParameters with Helpers {
  class MainBtbIO(implicit p: Parameters) extends BasePredictorIO {
    // prediction specific bundle
    val result:          Vec[Valid[Prediction]] = Output(Vec(NumBtbResultEntries, Valid(new Prediction)))
    val meta:            MainBtbMeta            = Output(new MainBtbMeta)
    val prefetchBbtMeta: PrefetchBtbMeta        = Output(new PrefetchBtbMeta)
    // final s3_takenMask (mbtb + tage + sc), used to touch replacer accurately
    val s3_takenMask: Vec[Bool] = Input(Vec(NumBtbResultEntries, Bool()))
    // prefecth io
    val flush: Bool = Input(Bool())
    // prefetch data
    val prefetchData: Valid[BtbPrefetchBundle] = Flipped(Valid(new BtbPrefetchBundle))
    // get pc from ftq
    val prefetchBtbFtqPtr: ValidIO[FtqPtr] = Valid(new FtqPtr)
    val ftqEntry:          FtqEntry        = Input(new FtqEntry())
//    val ifuPtr:            FtqPtr          = Input(new FtqPtr)

  }

  val io: MainBtbIO = IO(new MainBtbIO)

  // print params
  println(f"MainBtb:")
  println(f"  Size(set, way, align, internal): $NumSets * $NumWay * $NumAlignBanks * $NumInternalBanks = $NumEntries")
  println(f"  Address fields:")
  addrFields.show(indent = 4)

  /* *** submodules *** */
  private val prefetchBtb = Module(new PrefetchBtb)
  private val alignBanks  = Seq.tabulate(NumAlignBanks)(alignIdx => Module(new MainBtbAlignBank(alignIdx)))

  prefetchBtb.io.enable := true.B
  prefetchBtb.io.prefetchData <> io.prefetchData
  prefetchBtb.io.prefetchBtbFtqPtr <> io.prefetchBtbFtqPtr
  prefetchBtb.io.ftqEntry <> io.ftqEntry
  prefetchBtb.io.flush := io.flush
//  prefetchBtb.io.ifuPtr    := io.ifuPtr
  prefetchBtb.io.startPc   := io.startPc
  prefetchBtb.io.stageCtrl := io.stageCtrl
  prefetchBtb.io.train     := io.train

  io.prefetchBbtMeta := prefetchBtb.io.meta

  io.resetDone := alignBanks.map(_.io.resetDone).reduce(_ && _)

  io.trainReady := true.B

  private val s0_fire, s1_fire, s2_fire, s3_fire = Wire(Bool())
  alignBanks.foreach { b =>
    b.io.stageCtrl.s0_fire := s0_fire
    b.io.stageCtrl.s1_fire := s1_fire
    b.io.stageCtrl.s2_fire := s2_fire
    b.io.stageCtrl.s3_fire := s3_fire
    // alignBank does not care t0, it's using t1 only
    b.io.stageCtrl.t0_fire := false.B
  }

  /* *** s0 ***
   * calculate per-bank startPc and posHigherBits
   * send read request to alignBanks
   */
  s0_fire := io.stageCtrl.s0_fire && io.enable
  private val s0_startPc = io.startPc
  // rotate read addresses according to the first align bank index
  // e.g. if NumAlignBanks = 4, startPc locates in alignBank 1,
  // startPc + (i << FetchBlockAlignWidth) will be located in alignBank (1 + i) % 4,
  // i.e. we have VecInit.tabulate(...)'s alignBankIdx = (1, 2, 3, 0),
  // they always needs to goes to physical alignBank (0, 1, 2, 3),
  // so we need to rotate it right by 1.
  private val s0_rotator = VecRotate(getAlignBankIndex(s0_startPc))
  private val s0_startPcVec = s0_rotator.rotate(
    VecInit.tabulate(NumAlignBanks) { i =>
      if (i == 0)
        s0_startPc // keep lower bits for the first one
      else
        getAlignedPc(s0_startPc + (i << FetchBlockAlignWidth).U) // use aligned for others
    }
  )
  private val s0_posHigherBitsVec = s0_rotator.rotate(VecInit.tabulate(NumAlignBanks)(_.U(AlignBankIdxLen.W)))

  alignBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.read.req.startPc       := s0_startPcVec(i)
    b.io.read.req.posHigherBits := s0_posHigherBitsVec(i)
    b.io.read.req.crossPage     := isCrossPage(s0_startPcVec(i), s0_startPc)
  }

  /* *** s1 ***
   * just wait alignBanks
   */
  s1_fire := io.stageCtrl.s1_fire && io.enable

  /* *** s2 ***
   * receive read response from alignBanks
   * send out prediction result and meta info
   */
  s2_fire := io.stageCtrl.s2_fire && io.enable

  // we don't care about the order of alignBanks' responses,
  // (as s0_posHigherBitsVec is already computed and concatenated to each entry's posLowerBits)
  // (and we care about the full position when searching for a matching entry, not the bank it comes from)
  // so here we just flatten them, without rotating them back to the original order
  io.result := VecInit(alignBanks.flatMap(_.io.read.resp.predictions) ++ prefetchBtb.io.result)
  // we don't need to flatten meta entries, keep the alignBank structure, anyway we just use them per alignBank
  io.meta.entries := VecInit(alignBanks.map(_.io.read.resp.metas))

  /* *** s3 ***
   * touch replacer using final takenMask (mbtb + tage + sc)
   */
  s3_fire := io.enable && io.stageCtrl.s3_fire
  // io.result is flattened, so is s3_takenMask from Bpu top, here we need to slice it back to alignBank structure
  alignBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.s3_takenMask := io.s3_takenMask.slice(i * NumWay, (i + 1) * NumWay)
  }

  /* *** t0 ***
   * receive training data and latch
   */
  private val t0_fire  = io.stageCtrl.t0_fire && io.enable
  private val t0_train = io.train

  /* *** t1 ***
   * calculate write data and write to alignBanks
   */
  private val t1_fire  = RegNext(t0_fire, init = false.B) && io.enable
  private val t1_train = RegEnable(t0_train, t0_fire)

  private val t1_startPc = t1_train.startPc
  private val t1_rotator = VecRotate(getAlignBankIndex(t1_startPc))
  private val t1_startPcVec = t1_rotator.rotate(
    VecInit.tabulate(NumAlignBanks)(i => getAlignedPc(t1_startPc + (i << FetchBlockAlignWidth).U))
  )
  private val t1_meta           = t1_train.meta.mbtb
  private val t1_mispredictInfo = t1_train.mispredictBranch

  private val t1_writeAlignBankIdx  = getAlignBankIndexFromPosition(t1_mispredictInfo.bits.cfiPosition)
  private val t1_writeAlignBankMask = t1_rotator.rotate(VecInit(UIntToOH(t1_writeAlignBankIdx).asBools))

  alignBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.write.req.valid         := t1_fire && t1_writeAlignBankMask(i)
    b.io.write.req.bits.startPc  := t1_startPcVec(i)
    b.io.write.req.bits.branches := t1_train.branches
    b.io.write.req.bits.meta     := t1_meta.entries(i)
    // see comments in MainBtbAlignBank.scala
    b.io.write.req.bits.mispredictInfo := t1_mispredictInfo
  }
  /* --------------------------------------------------------------------------------------------------------------
     MainBTB Trace
     -------------------------------------------------------------------------------------------------------------- */
  private val alignBankTraceVec = alignBanks.map(_.io.trace)
  private val finalTrace        = Mux1H(t1_writeAlignBankMask, alignBankTraceVec)
  private val finalTraceStartPc = Mux1H(t1_writeAlignBankMask, t1_startPcVec)
  private val mbtbTrace         = Wire(new MainBtbTrace)

  mbtbTrace.startPc      := finalTraceStartPc
  mbtbTrace.setIdx       := finalTrace.setIdx
  mbtbTrace.internalIdx  := finalTrace.bankIdx
  mbtbTrace.alignBankIdx := PriorityEncoder(t1_writeAlignBankMask)
  mbtbTrace.wayIdx       := finalTrace.wayIdx
  mbtbTrace.attribute    := finalTrace.entry.attribute
  mbtbTrace.cfiPosition  := finalTrace.entry.position

  private val mbtbTraceDBTable = ChiselDB.createTable("MBTBTrace", new MainBtbTrace(), EnableMainbtbTrace)
  mbtbTraceDBTable.log(
    data = mbtbTrace,
    en = t1_fire && finalTrace.needWrite,
    clock = clock,
    reset = reset
  )

  /* *** statistics *** */
  private val perf_s2HitMask             = VecInit(alignBanks.flatMap(_.io.read.resp.predictions.map(_.valid)))
  private val perf_t1HitMispredictBranch = t1_meta.entries.flatten.map(_.hit(t1_mispredictInfo.bits)).reduce(_ || _)

  XSPerfAccumulate("total_train", t1_fire)
  XSPerfAccumulate("pred_hit", s2_fire && perf_s2HitMask.reduce(_ || _))
  XSPerfHistogram("pred_hit_count", PopCount(perf_s2HitMask), s2_fire, 0, NumWay * NumAlignBanks + 1)
  XSPerfAccumulate("train_has_mispredict", t1_fire && t1_mispredictInfo.valid)
  XSPerfAccumulate("train_hit_mispredict", t1_fire && t1_mispredictInfo.valid && perf_t1HitMispredictBranch)
  XSPerfAccumulate("pred_miss", s2_fire && perf_s2HitMask.reduce(!_ && !_))
}
