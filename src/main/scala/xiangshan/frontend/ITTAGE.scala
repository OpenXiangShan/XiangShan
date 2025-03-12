/***************************************************************************************
 * Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 *
 *
 * Acknowledgement
 *
 * This implementation is inspired by several key papers:
 * [1] André Seznec. "[A 64-Kbytes ITTAGE indirect branch predictor.](https://inria.hal.science/hal-00639041)" The
 * Journal of Instruction-Level Parallelism (JILP) 2nd JILP Workshop on Computer Architecture Competitions (JWAC):
 * Championship Branch Prediction (CBP). 2011.
 ***************************************************************************************/

package xiangshan.frontend

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.SeqBoolBitwiseOps
import org.chipsalliance.cde.config.Parameters
import scala.{Tuple2 => &}
import scala.math.min
import utility._
import utility.mbist.MbistPipeline
import utility.sram.FoldedSRAMTemplate
import xiangshan._

trait ITTageParams extends HasXSParameter with HasBPUParameter {

  val ITTageNTables    = ITTageTableInfos.size // Number of tage tables
  val ITTageCtrBits    = 2
  val uFoldedWidth     = 16
  val TickWidth        = 8
  val ITTageUsBits     = 1
  val TargetOffsetBits = 20
  val RegionNums       = 16
  val RegionBits       = VAddrBits - TargetOffsetBits
  val RegionPorts      = 2
  def ctrNull(ctr: UInt, ctrBits: Int = ITTageCtrBits): Bool =
    ctr === 0.U
  def ctrUnconf(ctr: UInt, ctrBits: Int = ITTageCtrBits): Bool =
    ctr < (1 << (ctrBits - 1)).U
  val UAONA_bits = 4

  def targetGetRegion(target: UInt): UInt = target(VAddrBits - 1, TargetOffsetBits)
  def targetGetOffset(target: UInt): UInt = target(TargetOffsetBits - 1, 0)

  lazy val TotalBits: Int = ITTageTableInfos.map {
    case (s, h, t) => {
      s * (1 + t + ITTageCtrBits + ITTageUsBits + VAddrBits)
    }
  }.sum
}

abstract class ITTageBundle(implicit p: Parameters)
    extends XSBundle with ITTageParams with BPUUtils

abstract class ITTageModule(implicit p: Parameters)
    extends XSModule with ITTageParams with BPUUtils {}

class ITTageReq(implicit p: Parameters) extends ITTageBundle {
  val pc          = UInt(VAddrBits.W)
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
}

class ITTageOffset(implicit p: Parameters) extends ITTageBundle {
  val offset      = UInt(TargetOffsetBits.W)
  val pointer     = UInt(log2Ceil(RegionNums).W)
  val usePCRegion = Bool()
}

class ITTageResp(implicit p: Parameters) extends ITTageBundle {
  val ctr           = UInt(ITTageCtrBits.W)
  val u             = UInt(ITTageUsBits.W)
  val target_offset = new ITTageOffset()
}

class ITTageUpdate(implicit p: Parameters) extends ITTageBundle {
  val pc    = UInt(VAddrBits.W)
  val ghist = UInt(HistoryLength.W)
  // update tag and ctr
  val valid   = Bool()
  val correct = Bool()
  val alloc   = Bool()
  val oldCtr  = UInt(ITTageCtrBits.W)
  // update u
  val uValid  = Bool()
  val u       = Bool()
  val reset_u = Bool()
  // target
  val target_offset     = new ITTageOffset()
  val old_target_offset = new ITTageOffset()
}

class ITTageMeta(implicit p: Parameters) extends XSBundle with ITTageParams {
  val provider          = ValidUndirectioned(UInt(log2Ceil(ITTageNTables).W))
  val altProvider       = ValidUndirectioned(UInt(log2Ceil(ITTageNTables).W))
  val altDiffers        = Bool()
  val providerU         = Bool()
  val providerCtr       = UInt(ITTageCtrBits.W)
  val altProviderCtr    = UInt(ITTageCtrBits.W)
  val allocate          = ValidUndirectioned(UInt(log2Ceil(ITTageNTables).W))
  val providerTarget    = UInt(VAddrBits.W)
  val altProviderTarget = UInt(VAddrBits.W)
  val pred_cycle        = if (!env.FPGAPlatform) Some(UInt(64.W)) else None

  override def toPrintable =
    p"pvdr(v:${provider.valid} num:${provider.bits} ctr:$providerCtr u:$providerU tar:${Hexadecimal(providerTarget)}), " +
      p"altpvdr(v:${altProvider.valid} num:${altProvider.bits}, ctr:$altProviderCtr, tar:${Hexadecimal(altProviderTarget)})"
}

class FakeITTageTable()(implicit p: Parameters) extends ITTageModule {
  val io = IO(new Bundle() {
    val req    = Input(Valid(new ITTageReq))
    val resp   = Output(Valid(new ITTageResp))
    val update = Input(new ITTageUpdate)
  })
  io.resp := DontCare

}

class RegionEntry(implicit p: Parameters) extends ITTageBundle {
  val valid  = Bool()
  val region = UInt(RegionBits.W)
}

class RegionWays()(implicit p: Parameters) extends XSModule with ITTageParams {
  val io = IO(new Bundle {
    val req_pointer = Input(Vec(ITTageNTables, UInt(log2Ceil(RegionNums).W)))
    val resp_hit    = Output(Vec(ITTageNTables, Bool()))
    val resp_region = Output(Vec(ITTageNTables, UInt(RegionBits.W)))

    val update_region  = Input(Vec(RegionPorts, UInt(RegionBits.W)))
    val update_hit     = Output(Vec(RegionPorts, Bool()))
    val update_pointer = Output(Vec(RegionPorts, UInt(log2Ceil(RegionNums).W)))

    val write_valid   = Input(Bool())
    val write_region  = Input(UInt(RegionBits.W))
    val write_pointer = Output(UInt(log2Ceil(RegionNums).W))
  })

  val regions             = RegInit(VecInit(Seq.fill(RegionNums)(0.U.asTypeOf(new RegionEntry()))))
  val replacer            = ReplacementPolicy.fromString("plru", RegionNums)
  val replacer_touch_ways = Wire(Vec(1, Valid(UInt(log2Ceil(RegionNums).W))))

  val valids = VecInit((0 until RegionNums).map(w => regions(w).valid))
  val valid  = WireInit(valids.andR)
  // write data
  val w_total_hits = VecInit((0 until RegionNums).map(w => regions(w).region === io.write_region && regions(w).valid))
  val w_hit        = w_total_hits.reduce(_ || _)
  val w_pointer    = Mux(w_hit, OHToUInt(w_total_hits), Mux(!valid, PriorityEncoder(~valids), replacer.way))
  XSError(PopCount(w_total_hits) > 1.U, "region has multiple hits!\n")
  XSPerfAccumulate("Region_entry_replace", !w_hit && valid && io.write_valid)

  io.write_pointer := w_pointer
  // read and metaTarget update read ports
  for (i <- 0 until ITTageNTables) {
    // read region use pointer
    io.resp_hit(i)    := regions(io.req_pointer(i)).valid
    io.resp_region(i) := regions(io.req_pointer(i)).region
  }

  for (i <- 0 until RegionPorts) {
    // When using metaTarget for updates, redefine the pointer
    val u_total_hits =
      VecInit((0 until RegionNums).map(w => regions(w).region === io.update_region(i) && regions(w).valid))
    val u_bypass  = (io.update_region(i) === io.write_region) && io.write_valid
    val u_hit     = u_total_hits.reduce(_ || _) || u_bypass
    val u_pointer = Mux(u_bypass, w_pointer, OHToUInt(u_total_hits))
    io.update_hit(i)     := u_hit
    io.update_pointer(i) := u_pointer
    XSError(PopCount(u_total_hits) > 1.U, "region has multiple hits!\n")
  }
  // write
  when(io.write_valid) {
    when(!regions(w_pointer).valid) {
      regions(w_pointer).valid := true.B
    }
    regions(w_pointer).region := io.write_region
  }
  replacer_touch_ways(0).valid := io.write_valid
  replacer_touch_ways(0).bits  := w_pointer
  replacer.access(replacer_touch_ways)
}

class ITTageTable(
    val nRows:    Int,
    val histLen:  Int,
    val tagLen:   Int,
    val tableIdx: Int
)(implicit p: Parameters)
    extends ITTageModule with HasFoldedHistory {
  val io = IO(new Bundle() {
    val req    = Flipped(DecoupledIO(new ITTageReq))
    val resp   = Output(Valid(new ITTageResp))
    val update = Input(new ITTageUpdate)
  })

  val SRAM_SIZE = 128

  val foldedWidth = if (nRows >= SRAM_SIZE) nRows / SRAM_SIZE else 1
  val dataSplit   = if (nRows <= 2 * SRAM_SIZE) 1 else 2

  if (nRows < SRAM_SIZE) {
    println(f"warning: ittage table $tableIdx has small sram depth of $nRows")
  }

  // override val debug = true
  // bypass entries for tage update
  val wrBypassEntries = 4

  require(histLen == 0 && tagLen == 0 || histLen != 0 && tagLen != 0)
  val idxFhInfo    = (histLen, min(log2Ceil(nRows), histLen))
  val tagFhInfo    = (histLen, min(histLen, tagLen))
  val altTagFhInfo = (histLen, min(histLen, tagLen - 1))
  val allFhInfos   = Seq(idxFhInfo, tagFhInfo, altTagFhInfo)

  def getFoldedHistoryInfo: Set[(Int, Int)] = allFhInfos.filter(_._1 > 0).toSet

  private def computeTagAndHash(unhashed_idx: UInt, allFh: AllFoldedHistories): (UInt, UInt) =
    if (histLen > 0) {
      val idx_fh     = allFh.getHistWithInfo(idxFhInfo).folded_hist
      val tag_fh     = allFh.getHistWithInfo(tagFhInfo).folded_hist
      val alt_tag_fh = allFh.getHistWithInfo(altTagFhInfo).folded_hist
      // require(idx_fh.getWidth == log2Ceil(nRows))
      val idx = (unhashed_idx ^ idx_fh)(log2Ceil(nRows) - 1, 0)
      val tag = ((unhashed_idx >> log2Ceil(nRows)).asUInt ^ tag_fh ^ (alt_tag_fh << 1).asUInt)(tagLen - 1, 0)
      (idx, tag)
    } else {
      require(tagLen == 0)
      (unhashed_idx(log2Ceil(nRows) - 1, 0), 0.U)
    }

  def incCtr(ctr: UInt, taken: Bool): UInt = satUpdate(ctr, ITTageCtrBits, taken)

  class ITTageEntry extends ITTageBundle {
    val valid         = Bool()
    val tag           = UInt(tagLen.W)
    val ctr           = UInt(ITTageCtrBits.W)
    val target_offset = new ITTageOffset()
    val useful        = Bool() // Due to the bitMask the useful bit needs to be at the lowest bit
  }

  val ittageEntrySz = 1 + tagLen + ITTageCtrBits + ITTageUsBits + TargetOffsetBits + log2Ceil(RegionNums) + 1
  require(ittageEntrySz == (new ITTageEntry).getWidth)

  // pc is start address of basic block, most 2 branch inst in block
  def getUnhashedIdx(pc: UInt): UInt = (pc >> instOffsetBits).asUInt

  val s0_valid        = io.req.valid
  val s0_pc           = io.req.bits.pc
  val s0_unhashed_idx = getUnhashedIdx(io.req.bits.pc)

  val (s0_idx, s0_tag) = computeTagAndHash(s0_unhashed_idx, io.req.bits.folded_hist)
  val (s1_idx, s1_tag) = (RegEnable(s0_idx, io.req.fire), RegEnable(s0_tag, io.req.fire))
  val s1_valid         = RegNext(s0_valid)

  val table = Module(new FoldedSRAMTemplate(
    new ITTageEntry,
    setSplit = 1,
    waySplit = 1,
    dataSplit = dataSplit,
    set = nRows,
    width = foldedWidth,
    shouldReset = true,
    holdRead = true,
    singlePort = true,
    useBitmask = true,
    withClockGate = true,
    hasMbist = hasMbist
  ))
  private val mbistPl = MbistPipeline.PlaceMbistPipeline(1, "MbistPipeIttage", hasMbist)
  table.io.r.req.valid       := io.req.fire
  table.io.r.req.bits.setIdx := s0_idx

  val table_read_data = table.io.r.resp.data(0)

  val s1_req_rhit = table_read_data.valid && table_read_data.tag === s1_tag

  val read_write_conflict    = io.update.valid && io.req.valid
  val s1_read_write_conflict = RegEnable(read_write_conflict, io.req.valid)

  io.resp.valid    := (if (tagLen != 0) s1_req_rhit && !s1_read_write_conflict else true.B) && s1_valid // && s1_mask(b)
  io.resp.bits.ctr := table_read_data.ctr
  io.resp.bits.u   := table_read_data.useful
  io.resp.bits.target_offset := table_read_data.target_offset

  // Use fetchpc to compute hash
  val update_folded_hist = WireInit(0.U.asTypeOf(new AllFoldedHistories(foldedGHistInfos)))

  update_folded_hist.getHistWithInfo(idxFhInfo).folded_hist    := compute_folded_ghist(io.update.ghist, log2Ceil(nRows))
  update_folded_hist.getHistWithInfo(tagFhInfo).folded_hist    := compute_folded_ghist(io.update.ghist, tagLen)
  update_folded_hist.getHistWithInfo(altTagFhInfo).folded_hist := compute_folded_ghist(io.update.ghist, tagLen - 1)
  val (update_idx, update_tag) = computeTagAndHash(getUnhashedIdx(io.update.pc), update_folded_hist)
  val update_wdata             = Wire(new ITTageEntry)

  val updateAllBitmask = VecInit.fill(ittageEntrySz)(1.U).asUInt // update all entry
  val updateNoBitmask  = VecInit.fill(ittageEntrySz)(0.U).asUInt // update no
  val updateNoUsBitmask =
    VecInit.tabulate(ittageEntrySz)(_.U >= ITTageUsBits.U).asUInt // update others besides useful bit
  val updateUsBitmask = VecInit.tabulate(ittageEntrySz)(_.U < ITTageUsBits.U).asUInt // update useful bit

  val needReset               = RegInit(false.B)
  val useful_can_reset        = !(io.req.fire || io.update.valid) && needReset
  val (resetSet, resetFinish) = Counter(useful_can_reset, nRows)
  when(io.update.reset_u) {
    needReset := true.B
  }.elsewhen(resetFinish) {
    needReset := false.B
  }
  val update_bitmask = Mux(
    io.update.uValid && io.update.valid,
    updateAllBitmask,
    Mux(io.update.valid, updateNoUsBitmask, Mux(useful_can_reset, updateUsBitmask, updateNoBitmask))
  )

  table.io.w.apply(
    valid = io.update.valid || useful_can_reset,
    data = update_wdata,
    setIdx = Mux(useful_can_reset, resetSet, update_idx),
    waymask = true.B,
    bitmask = update_bitmask
  )

  // Power-on reset
  val powerOnResetState = RegInit(true.B)
  when(table.io.r.req.ready) {
    // When all the SRAM first reach ready state, we consider power-on reset is done
    powerOnResetState := false.B
  }
  // Do not use table banks io.r.req.ready directly
  // All table_banks are single port SRAM, ready := !wen
  // We do not want write request block the whole BPU pipeline
  // Once read priority is higher than write, table_banks(*).io.r.req.ready can be used
  io.req.ready := !powerOnResetState

  val wrbypass = Module(new WrBypass(UInt(ITTageCtrBits.W), wrBypassEntries, log2Ceil(nRows)))

  wrbypass.io.wen       := io.update.valid
  wrbypass.io.write_idx := update_idx
  wrbypass.io.write_data.map(_ := update_wdata.ctr)

  val old_ctr = Mux(wrbypass.io.hit, wrbypass.io.hit_data(0).bits, io.update.oldCtr)
  update_wdata.valid  := true.B
  update_wdata.ctr    := Mux(io.update.alloc, 2.U, incCtr(old_ctr, io.update.correct))
  update_wdata.tag    := update_tag
  update_wdata.useful := Mux(useful_can_reset, false.B, io.update.u)
  // only when ctr is null
  update_wdata.target_offset := Mux(
    io.update.alloc || ctrNull(old_ctr),
    io.update.target_offset,
    io.update.old_target_offset
  )

  XSPerfAccumulate("ittage_table_updates", io.update.valid)
  XSPerfAccumulate("ittage_table_hits", io.resp.valid)
  XSPerfAccumulate("ittage_us_tick_reset", io.update.reset_u)
  XSPerfAccumulate("ittage_table_read_write_conflict", read_write_conflict)

  if (BPUDebug && debug) {
    val u   = io.update
    val idx = s0_idx
    val tag = s0_tag
    XSDebug(
      io.req.fire,
      p"ITTageTableReq: pc=0x${Hexadecimal(io.req.bits.pc)}, " +
        p"idx=$idx, tag=$tag\n"
    )
    XSDebug(
      RegNext(io.req.fire) && s1_req_rhit,
      p"ITTageTableResp: idx=$s1_idx, hit:${s1_req_rhit}, " +
        p"ctr:${io.resp.bits.ctr}, u:${io.resp.bits.u}, tar:${Hexadecimal(io.resp.bits.target_offset.offset)}\n"
    )
    XSDebug(
      io.update.valid,
      p"update ITTAGE Table: pc:${Hexadecimal(u.pc)}}, " +
        p"correct:${u.correct}, alloc:${u.alloc}, oldCtr:${u.oldCtr}, " +
        p"target:${Hexadecimal(u.target_offset.offset)}, old_target:${Hexadecimal(u.old_target_offset.offset)}\n"
    )
    XSDebug(
      io.update.valid,
      p"update ITTAGE Table: writing tag:${update_tag}, " +
        p"ctr: ${update_wdata.ctr}, target:${Hexadecimal(update_wdata.target_offset.offset)}" +
        p" in idx $update_idx\n"
    )
    XSDebug(RegNext(io.req.fire) && !s1_req_rhit, "TageTableResp: no hits!\n")

    // ------------------------------Debug-------------------------------------
    val valids = RegInit(0.U.asTypeOf(Vec(nRows, Bool())))
    when(io.update.valid)(valids(update_idx) := true.B)
    XSDebug("ITTAGE Table usage:------------------------\n")
    XSDebug("%d out of %d rows are valid\n", PopCount(valids), nRows.U)
  }

}

abstract class BaseITTage(implicit p: Parameters) extends BasePredictor with ITTageParams with BPUUtils {}

class FakeITTage(implicit p: Parameters) extends BaseITTage {
  io.out <> 0.U.asTypeOf(DecoupledIO(new BasePredictorOutput))

  io.s1_ready := true.B
  io.s2_ready := true.B
}

class ITTage(implicit p: Parameters) extends BaseITTage {
  override val meta_size = 0.U.asTypeOf(new ITTageMeta).getWidth

  val tables = ITTageTableInfos.zipWithIndex.map {
    case ((nRows, histLen, tagLen), i) =>
      val t = Module(new ITTageTable(nRows, histLen, tagLen, i))
      t
  }
  override def getFoldedHistoryInfo: Option[Set[(Int, Int)]] = Some(tables.map(_.getFoldedHistoryInfo).reduce(_ ++ _))

  val useAltOnNa = RegInit((1 << (UAONA_bits - 1)).U(UAONA_bits.W))
  val tickCtr    = RegInit(0.U(TickWidth.W))

  val rTable = Module(new RegionWays)

  // uftb miss or hasIndirect
  val s1_uftbHit         = io.in.bits.resp_in(0).s1_uftbHit
  val s1_uftbHasIndirect = io.in.bits.resp_in(0).s1_uftbHasIndirect
  val s1_isIndirect      = (!s1_uftbHit && !io.in.bits.resp_in(0).s1_ftbCloseReq) || s1_uftbHasIndirect

  // Keep the table responses to process in s2

  val s2_resps = VecInit(tables.map(t => t.io.resp))

  val debug_pc_s1 = RegEnable(s0_pc_dup(3), io.s0_fire(3))
  val debug_pc_s2 = RegEnable(debug_pc_s1, io.s1_fire(3))
  val debug_pc_s3 = RegEnable(debug_pc_s2, io.s2_fire(3))

  val s2_tageTarget        = Wire(UInt(VAddrBits.W))
  val s2_providerTarget    = Wire(UInt(VAddrBits.W))
  val s2_altProviderTarget = Wire(UInt(VAddrBits.W))
  val s2_provided          = Wire(Bool())
  val s2_provider          = Wire(UInt(log2Ceil(ITTageNTables).W))
  val s2_altProvided       = Wire(Bool())
  val s2_altProvider       = Wire(UInt(log2Ceil(ITTageNTables).W))
  val s2_providerU         = Wire(Bool())
  val s2_providerCtr       = Wire(UInt(ITTageCtrBits.W))
  val s2_altProviderCtr    = Wire(UInt(ITTageCtrBits.W))

  val s3_tageTarget_dup    = io.s2_fire.map(f => RegEnable(s2_tageTarget, f))
  val s3_providerTarget    = RegEnable(s2_providerTarget, io.s2_fire(3))
  val s3_altProviderTarget = RegEnable(s2_altProviderTarget, io.s2_fire(3))
  val s3_provided          = RegEnable(s2_provided, io.s2_fire(3))
  val s3_provider          = RegEnable(s2_provider, io.s2_fire(3))
  val s3_altProvided       = RegEnable(s2_altProvided, io.s2_fire(3))
  val s3_altProvider       = RegEnable(s2_altProvider, io.s2_fire(3))
  val s3_providerU         = RegEnable(s2_providerU, io.s2_fire(3))
  val s3_providerCtr       = RegEnable(s2_providerCtr, io.s2_fire(3))
  val s3_altProviderCtr    = RegEnable(s2_altProviderCtr, io.s2_fire(3))

  val resp_meta = WireInit(0.U.asTypeOf(new ITTageMeta))

  io.out.last_stage_meta := resp_meta.asUInt

  // Update logic
  val u_valid = RegNext(io.update.valid, init = false.B)

  val update = Wire(new BranchPredictionUpdate)
  update := RegEnable(io.update.bits, io.update.valid)

  // meta is splited by composer
  val updateMeta = Wire(new ITTageMeta)
  update.meta := updateMeta.asUInt

  // The pc register has been moved outside of predictor
  // pc field of update bundle and other update data are not in the same stage
  // so io.update.bits.pc is used directly here
  val update_pc = io.update.bits.pc

  // To improve Clock Gating Efficiency
  val u_meta = io.update.bits.meta.asTypeOf(new ITTageMeta)
  updateMeta := RegEnable(u_meta, io.update.valid)
  updateMeta.provider.bits := RegEnable(
    u_meta.provider.bits,
    io.update.valid && u_meta.provider.valid
  )
  updateMeta.providerTarget := RegEnable(
    u_meta.providerTarget,
    io.update.valid && u_meta.provider.valid
  )
  updateMeta.allocate.bits := RegEnable(
    u_meta.allocate.bits,
    io.update.valid && u_meta.allocate.valid
  )
  updateMeta.altProvider.bits := RegEnable(
    u_meta.altProvider.bits,
    io.update.valid && u_meta.altProvider.valid
  )
  updateMeta.altProviderTarget := RegEnable(
    u_meta.altProviderTarget,
    io.update.valid && u_meta.provider.valid && u_meta.altProvider.valid && u_meta.providerCtr === 0.U
  )
  update.full_target := RegEnable(
    io.update.bits.full_target,
    io.update.valid // not using mispred_mask, because mispred_mask timing is bad
  )
  update.cfi_idx.bits := RegEnable(io.update.bits.cfi_idx.bits, io.update.valid && io.update.bits.cfi_idx.valid)
  update.ghist        := RegEnable(io.update.bits.ghist, io.update.valid) // TODO: CGE

  val updateValid = update.is_jalr && !update.is_ret && u_valid && update.ftb_entry.jmpValid &&
    update.jmp_taken && update.cfi_idx.valid &&
    update.cfi_idx.bits === update.ftb_entry.tailSlot.offset && !update.ftb_entry.strong_bias(numBr - 1)

  val updateMask            = WireInit(0.U.asTypeOf(Vec(ITTageNTables, Bool())))
  val updateUMask           = WireInit(0.U.asTypeOf(Vec(ITTageNTables, Bool())))
  val updateResetU          = WireInit(false.B)
  val updateCorrect         = Wire(Vec(ITTageNTables, Bool()))
  val updateAlloc           = Wire(Vec(ITTageNTables, Bool()))
  val updateOldCtr          = Wire(Vec(ITTageNTables, UInt(ITTageCtrBits.W)))
  val updateU               = Wire(Vec(ITTageNTables, Bool()))
  val updateTargetOffset    = Wire(Vec(ITTageNTables, new ITTageOffset))
  val updateOldTargetOffset = Wire(Vec(ITTageNTables, new ITTageOffset))
  updateCorrect         := DontCare
  updateAlloc           := DontCare
  updateOldCtr          := DontCare
  updateU               := DontCare
  updateTargetOffset    := DontCare
  updateOldTargetOffset := DontCare

  val updateMisPred = update.mispred_mask(numBr) // the last one indicates jmp results

  // Predict
  tables.map { t =>
    t.io.req.valid            := io.s1_fire(3) && s1_isIndirect
    t.io.req.bits.pc          := s1_pc_dup(3)
    t.io.req.bits.folded_hist := io.in.bits.s1_folded_hist(3)
  }

  // access tag tables and output meta info
  class ITTageTableInfo(implicit p: Parameters) extends ITTageResp {
    val tableIdx   = UInt(log2Ceil(ITTageNTables).W)
    val maskTarget = Vec(ITTageNTables, UInt(VAddrBits.W))
  }

  val inputRes = VecInit(s2_resps.zipWithIndex.map {
    case (r, i) =>
      val tableInfo = Wire(new ITTageTableInfo)
      tableInfo.u             := r.bits.u
      tableInfo.ctr           := r.bits.ctr
      tableInfo.target_offset := r.bits.target_offset
      tableInfo.tableIdx      := i.U(log2Ceil(ITTageNTables).W)
      tableInfo.maskTarget    := VecInit(Seq.fill(ITTageNTables)(0.U(VAddrBits.W)))
      tableInfo.maskTarget(i) := "hffff_ffff_ffff_ffff".U
      SelectTwoInterRes(r.valid, tableInfo)
  })

  val selectedInfo = ParallelSelectTwo(inputRes.reverse)
  val provided     = selectedInfo.hasOne
  val altProvided  = selectedInfo.hasTwo

  val providerInfo    = selectedInfo.first
  val altProviderInfo = selectedInfo.second
  val providerNull    = providerInfo.ctr === 0.U

  val baseTarget             = io.in.bits.resp_in(0).s2.full_pred(3).jalr_target // use ftb pred as base target
  val region_r_target_offset = VecInit(s2_resps.map(r => r.bits.target_offset))

  rTable.io.req_pointer.zipWithIndex.map { case (req_pointer, i) =>
    req_pointer := region_r_target_offset(i).pointer
  }
  // When the entry corresponding to the pointer is valid and does not use PCRegion, use rTable region.
  val region_targets = Wire(Vec(ITTageNTables, UInt(VAddrBits.W)))
  for (i <- 0 until ITTageNTables) {
    region_targets(i) := Mux(
      rTable.io.resp_hit(i) && !region_r_target_offset(i).usePCRegion,
      Cat(rTable.io.resp_region(i), region_r_target_offset(i).offset),
      Cat(targetGetRegion(s2_pc_dup(0).getAddr()), region_r_target_offset(i).offset)
    )
  }

  val providerCatTarget = providerInfo.maskTarget.zipWithIndex.map {
    case (mask, i) => mask & region_targets(i)
  }.reduce(_ | _)

  val altproviderCatTarget = altProviderInfo.maskTarget.zipWithIndex.map {
    case (mask, i) => mask & region_targets(i)
  }.reduce(_ | _)

  s2_tageTarget := Mux1H(Seq(
    (provided && !(providerNull && altProvided), providerCatTarget),
    (altProvided && providerNull, altproviderCatTarget),
    (!provided, baseTarget)
  ))
  s2_provided          := provided
  s2_provider          := providerInfo.tableIdx
  s2_altProvided       := altProvided
  s2_altProvider       := altProviderInfo.tableIdx
  s2_providerU         := providerInfo.u
  s2_providerCtr       := providerInfo.ctr
  s2_altProviderCtr    := altProviderInfo.ctr
  s2_providerTarget    := providerCatTarget
  s2_altProviderTarget := altproviderCatTarget

  XSDebug(io.s2_fire(3), p"hit_taken_jalr:")

  for (
    fp & s3_tageTarget <-
      io.out.s3.full_pred zip s3_tageTarget_dup
  )
    yield fp.jalr_target := s3_tageTarget

  resp_meta.provider.valid    := s3_provided
  resp_meta.provider.bits     := s3_provider
  resp_meta.altProvider.valid := s3_altProvided
  resp_meta.altProvider.bits  := s3_altProvider
  resp_meta.altDiffers        := s3_providerTarget =/= s3_altProviderTarget
  resp_meta.providerU         := s3_providerU
  resp_meta.providerCtr       := s3_providerCtr
  resp_meta.altProviderCtr    := s3_altProviderCtr
  resp_meta.providerTarget    := s3_providerTarget
  resp_meta.altProviderTarget := s3_altProviderTarget
  resp_meta.pred_cycle.foreach(_ := GTimer())
  // TODO: adjust for ITTAGE
  // Create a mask fo tables which did not hit our query, and also contain useless entries
  // and also uses a longer history than the provider
  val s2_allocatableSlots = VecInit(s2_resps.map(r => !r.valid && !r.bits.u)).asUInt &
    (~(LowerMask(UIntToOH(s2_provider), ITTageNTables) & Fill(ITTageNTables, s2_provided.asUInt))).asUInt
  val s2_allocLFSR   = random.LFSR(width = 15)(ITTageNTables - 1, 0)
  val s2_firstEntry  = PriorityEncoder(s2_allocatableSlots)
  val s2_maskedEntry = PriorityEncoder(s2_allocatableSlots & s2_allocLFSR)
  val s2_allocEntry  = Mux(s2_allocatableSlots(s2_maskedEntry), s2_maskedEntry, s2_firstEntry)
  resp_meta.allocate.valid := RegEnable(s2_allocatableSlots =/= 0.U, io.s2_fire(3))
  resp_meta.allocate.bits  := RegEnable(s2_allocEntry, io.s2_fire(3))

  // Update in loop
  val updateRealTarget       = update.full_target
  val updatePCRegion         = targetGetRegion(update.pc)
  val updateRealTargetRegion = targetGetRegion(updateRealTarget)
  val metaProviderTargetOffset, metaAltProviderTargetOffset, updateRealTargetOffset =
    WireInit(0.U.asTypeOf(new ITTageOffset))
  updateRealTargetOffset.offset := targetGetOffset(updateRealTarget)
  val updateRealUsePCRegion = updateRealTargetRegion === updatePCRegion
  // If rTable is not written in Region, the pointer value will be invalid.
  // At this time, it is necessary to raise usePCRegion.
  // The update mechanism of the usePCRegion bit requires further consideration.
  updateRealTargetOffset.usePCRegion := updateRealUsePCRegion || !updateAlloc.reduce(_ || _)
  rTable.io.write_valid              := !updateRealUsePCRegion && updateAlloc.reduce(_ || _)
  rTable.io.write_region             := updateRealTargetRegion
  updateRealTargetOffset.pointer     := rTable.io.write_pointer

  val metaProviderTargetRegion    = targetGetRegion(updateMeta.providerTarget)
  val metaAltProviderTargetRegion = targetGetRegion(updateMeta.altProviderTarget)

  rTable.io.update_region              := VecInit(metaProviderTargetRegion, metaAltProviderTargetRegion)
  metaProviderTargetOffset.offset      := targetGetOffset(updateMeta.providerTarget)
  metaProviderTargetOffset.pointer     := rTable.io.update_pointer(0)
  metaProviderTargetOffset.usePCRegion := !rTable.io.update_hit(0)

  metaAltProviderTargetOffset.offset      := targetGetOffset(updateMeta.altProviderTarget)
  metaAltProviderTargetOffset.pointer     := rTable.io.update_pointer(1)
  metaAltProviderTargetOffset.usePCRegion := !rTable.io.update_hit(1)

  val provider    = updateMeta.provider.bits
  val altProvider = updateMeta.altProvider.bits
  val usedAltpred = updateMeta.altProvider.valid && updateMeta.providerCtr === 0.U
  when(updateValid) {
    when(updateMeta.provider.valid) {
      when(usedAltpred && updateMisPred) { // update altpred if used as pred
        updateMask(altProvider)            := true.B
        updateUMask(altProvider)           := false.B
        updateCorrect(altProvider)         := false.B
        updateOldCtr(altProvider)          := updateMeta.altProviderCtr
        updateAlloc(altProvider)           := false.B
        updateTargetOffset(altProvider)    := updateRealTargetOffset
        updateOldTargetOffset(altProvider) := metaAltProviderTargetOffset
      }

      updateMask(provider)  := true.B
      updateUMask(provider) := true.B

      updateU(provider) := Mux(
        !updateMeta.altDiffers,
        updateMeta.providerU,
        updateMeta.providerTarget === updateRealTarget
      )
      updateCorrect(provider)         := updateMeta.providerTarget === updateRealTarget
      updateOldCtr(provider)          := updateMeta.providerCtr
      updateAlloc(provider)           := false.B
      updateTargetOffset(provider)    := updateRealTargetOffset
      updateOldTargetOffset(provider) := metaProviderTargetOffset
    }
  }
  XSDebug(
    updateValid && updateMeta.provider.valid,
    p"update provider $provider, pred cycle ${updateMeta.pred_cycle.getOrElse(0.U)}\n"
  )
  XSDebug(
    updateValid && updateMeta.provider.valid && usedAltpred && updateMisPred,
    p"update altprovider $altProvider, pred cycle ${updateMeta.pred_cycle.getOrElse(0.U)}\n"
  )

  // if mispredicted and not the case that
  // provider offered correct target but used altpred due to unconfident
  val providerCorrect = updateMeta.provider.valid && updateMeta.providerTarget === updateRealTarget
  val providerUnconf  = updateMeta.providerCtr === 0.U
  val allocate        = updateMeta.allocate
  when(updateValid && updateMisPred && !(providerCorrect && providerUnconf)) {
    tickCtr := satUpdate(tickCtr, TickWidth, !allocate.valid)
    when(allocate.valid) {
      updateMask(allocate.bits)         := true.B
      updateCorrect(allocate.bits)      := DontCare // useless for alloc
      updateAlloc(allocate.bits)        := true.B
      updateUMask(allocate.bits)        := true.B
      updateU(allocate.bits)            := false.B
      updateTargetOffset(allocate.bits) := updateRealTargetOffset
    }
  }
  XSDebug(
    updateValid && updateMisPred && !(providerCorrect && providerUnconf) && allocate.valid,
    p"allocate new table entry, pred cycle ${updateMeta.pred_cycle.getOrElse(0.U)}\n"
  )

  when(tickCtr === ((1 << TickWidth) - 1).U) {
    tickCtr      := 0.U
    updateResetU := true.B
  }

  for (i <- 0 until ITTageNTables) {
    tables(i).io.update.valid             := RegNext(updateMask(i), init = false.B)
    tables(i).io.update.reset_u           := RegNext(updateResetU, init = false.B)
    tables(i).io.update.correct           := RegEnable(updateCorrect(i), updateMask(i))
    tables(i).io.update.alloc             := RegEnable(updateAlloc(i), updateMask(i))
    tables(i).io.update.oldCtr            := RegEnable(updateOldCtr(i), updateMask(i))
    tables(i).io.update.target_offset     := RegEnable(updateTargetOffset(i), updateMask(i))
    tables(i).io.update.old_target_offset := RegEnable(updateOldTargetOffset(i), updateMask(i))

    tables(i).io.update.uValid := RegEnable(updateUMask(i), false.B, updateMask(i))
    tables(i).io.update.u      := RegEnable(updateU(i), updateMask(i))
    tables(i).io.update.pc     := RegEnable(update_pc, updateMask(i))
    tables(i).io.update.ghist  := RegEnable(update.ghist, updateMask(i))
  }

  // all should be ready for req
  io.s1_ready := tables.map(_.io.req.ready).reduce(_ && _)

  // Debug and perf info
  XSPerfAccumulate("ittage_reset_u", updateResetU)
  XSPerfAccumulate("ittage_used", io.s1_fire(0) && s1_isIndirect)
  XSPerfAccumulate("ittage_closed_due_to_uftb_info", io.s1_fire(0) && !s1_isIndirect)
  XSPerfAccumulate("ittage_allocate", updateAlloc.reduce(_ || _))

  private def pred_perf(name:   String, cond: Bool) = XSPerfAccumulate(s"${name}_at_pred", cond && io.s2_fire(3))
  private def commit_perf(name: String, cond: Bool) = XSPerfAccumulate(s"${name}_at_commit", cond && updateValid)
  private def ittage_perf(name: String, pred_cond: Bool, commit_cond: Bool) = {
    pred_perf(s"ittage_${name}", pred_cond)
    commit_perf(s"ittage_${name}", commit_cond)
  }
  val pred_use_provider       = s2_provided && !ctrNull(s2_providerCtr)
  val pred_use_altpred        = s2_provided && ctrNull(s2_providerCtr)
  val pred_use_ht_as_altpred  = pred_use_altpred && s2_altProvided
  val pred_use_bim_as_altpred = pred_use_altpred && !s2_altProvided
  val pred_use_bim_as_pred    = !s2_provided

  val commit_use_provider       = updateMeta.provider.valid && !ctrNull(updateMeta.providerCtr)
  val commit_use_altpred        = updateMeta.provider.valid && ctrNull(updateMeta.providerCtr)
  val commit_use_ht_as_altpred  = commit_use_altpred && updateMeta.altProvider.valid
  val commit_use_ftb_as_altpred = commit_use_altpred && !updateMeta.altProvider.valid
  val commit_use_ftb_as_pred    = !updateMeta.provider.valid

  for (i <- 0 until ITTageNTables) {
    val pred_this_is_provider   = s2_provider === i.U
    val pred_this_is_altpred    = s2_altProvider === i.U
    val commit_this_is_provider = updateMeta.provider.bits === i.U
    val commit_this_is_altpred  = updateMeta.altProvider.bits === i.U
    ittage_perf(
      s"table_${i}_final_provided",
      pred_use_provider && pred_this_is_provider,
      commit_use_provider && commit_this_is_provider
    )
    ittage_perf(
      s"table_${i}_provided_not_used",
      pred_use_altpred && pred_this_is_provider,
      commit_use_altpred && commit_this_is_provider
    )
    ittage_perf(
      s"table_${i}_alt_provider_as_final_pred",
      pred_use_ht_as_altpred && pred_this_is_altpred,
      commit_use_ht_as_altpred && commit_this_is_altpred
    )
    ittage_perf(
      s"table_${i}_alt_provider_not_used",
      pred_use_provider && pred_this_is_altpred,
      commit_use_provider && commit_this_is_altpred
    )
  }

  ittage_perf("provided", s2_provided, updateMeta.provider.valid)
  ittage_perf("use_provider", pred_use_provider, commit_use_provider)
  ittage_perf("use_altpred", pred_use_altpred, commit_use_altpred)
  ittage_perf("use_ht_as_altpred", pred_use_ht_as_altpred, commit_use_ht_as_altpred)
  ittage_perf("use_ftb_when_no_provider", pred_use_bim_as_pred, commit_use_ftb_as_pred)
  ittage_perf("use_ftb_as_alt_provider", pred_use_bim_as_altpred, commit_use_ftb_as_altpred)
  XSPerfAccumulate("updated", updateValid)

  if (debug) {
    val s2_resps_regs = RegEnable(s2_resps, io.s2_fire(3))
    XSDebug("req: v=%d, pc=0x%x\n", io.s0_fire(3), s0_pc_dup(3))
    XSDebug("s1_fire:%d, resp: pc=%x\n", io.s1_fire(3), debug_pc_s1)
    XSDebug("s2_fireOnLastCycle: resp: pc=%x, target=%x, hit=%b\n", debug_pc_s2, io.out.s2.getTarget(3), s2_provided)
    for (i <- 0 until ITTageNTables) {
      XSDebug(
        "TageTable(%d): valids:%b, resp_ctrs:%b, resp_us:%b, target:%x\n",
        i.U,
        VecInit(s2_resps_regs(i).valid).asUInt,
        s2_resps_regs(i).bits.ctr,
        s2_resps_regs(i).bits.u,
        s2_resps_regs(i).bits.target_offset.offset
      )
    }
  }
  XSDebug(updateValid, p"pc: ${Hexadecimal(update_pc)}, target: ${Hexadecimal(update.full_target)}\n")
  XSDebug(updateValid, updateMeta.toPrintable + p"\n")
  XSDebug(updateValid, p"correct(${!updateMisPred})\n")

  generatePerfEvent()
}
