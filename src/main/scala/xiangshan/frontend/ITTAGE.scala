/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  ***************************************************************************************/

package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import chisel3.experimental.chiselName
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.transforms.RenameModules
import freechips.rocketchip.transforms.naming.RenameDesiredNames

import scala.math.min
import scala.util.matching.Regex
import firrtl.passes.wiring.Wiring

trait ITTageParams extends HasXSParameter with HasBPUParameter {

  val ITTageNTables = ITTageTableInfos.size // Number of tage tables
  val UBitPeriod = 2048
  val ITTageCtrBits = 2
  val uFoldedWidth = 16
  val TickWidth = 8
  def ctr_null(ctr: UInt, ctrBits: Int = ITTageCtrBits) = {
    ctr === 0.U
  }
  def ctr_unconf(ctr: UInt, ctrBits: Int = ITTageCtrBits) = {
    ctr < (1 << (ctrBits-1)).U
  }
  val UAONA_bits = 4

  val TotalBits = ITTageTableInfos.map {
    case (s, h, t) => {
      s * (1+t+ITTageCtrBits+VAddrBits)
    }
  }.reduce(_+_)
}
// reuse TAGE implementation

trait ITTageHasFoldedHistory {
  val histLen: Int
  def compute_folded_hist(hist: UInt, l: Int) = {
    if (histLen > 0) {
      val nChunks = (histLen + l - 1) / l
      val hist_chunks = (0 until nChunks) map {i =>
        hist(min((i+1)*l, histLen)-1, i*l)
      }
      ParallelXOR(hist_chunks)
    }
    else 0.U
  }
}



abstract class ITTageBundle(implicit p: Parameters)
  extends XSBundle with ITTageParams with BPUUtils

abstract class ITTageModule(implicit p: Parameters)
  extends XSModule with ITTageParams with BPUUtils
{}


class ITTageReq(implicit p: Parameters) extends ITTageBundle {
  val pc = UInt(VAddrBits.W)
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
}

class ITTageResp(implicit p: Parameters) extends ITTageBundle {
  val ctr = UInt(ITTageCtrBits.W)
  val u = UInt(2.W)
  val target = UInt(VAddrBits.W)
}

class ITTageUpdate(implicit p: Parameters) extends ITTageBundle {
  val pc = UInt(VAddrBits.W)
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
  // update tag and ctr
  val valid = Bool()
  val correct = Bool()
  val alloc = Bool()
  val oldCtr = UInt(ITTageCtrBits.W)
  // update u
  val uValid = Bool()
  val u = Bool()
  val reset_u = Bool()
  // target
  val target = UInt(VAddrBits.W)
  val old_target = UInt(VAddrBits.W)
}

// reuse TAGE Implementation

class ITTageMeta(implicit p: Parameters) extends XSBundle with ITTageParams{
  val provider = ValidUndirectioned(UInt(log2Ceil(ITTageNTables).W))
  val altProvider = ValidUndirectioned(UInt(log2Ceil(ITTageNTables).W))
  val altDiffers = Bool()
  val providerU = Bool()
  val providerCtr = UInt(ITTageCtrBits.W)
  val altProviderCtr = UInt(ITTageCtrBits.W)
  val allocate = ValidUndirectioned(UInt(log2Ceil(ITTageNTables).W))
  val taken = Bool()
  val providerTarget = UInt(VAddrBits.W)
  val altProviderTarget = UInt(VAddrBits.W)
  // val scMeta = new SCMeta(EnableSC)
  // TODO: check if we need target info here
  val pred_cycle = if (!env.FPGAPlatform) Some(UInt(64.W)) else None

  override def toPrintable = {
    p"pvdr(v:${provider.valid} num:${provider.bits} ctr:$providerCtr u:$providerU tar:${Hexadecimal(providerTarget)}), " +
    p"altpvdr(v:${altProvider.valid} num:${altProvider.bits}, ctr:$altProviderCtr, tar:${Hexadecimal(altProviderTarget)}), " +
    p"altdiff:$altDiffers, alloc(v:${allocate.valid} num:${allocate.bits}), taken:$taken, cycle:${pred_cycle.getOrElse(0.U)}"
  }
}


class FakeITTageTable()(implicit p: Parameters) extends ITTageModule {
  val io = IO(new Bundle() {
    val req = Input(Valid(new ITTageReq))
    val resp = Output(Valid(new ITTageResp))
    val update = Input(new ITTageUpdate)
  })
  io.resp := DontCare

}
@chiselName
class ITTageTable
(
  val nRows: Int, val histLen: Int, val tagLen: Int, val uBitPeriod: Int, val tableIdx: Int
)(implicit p: Parameters)
  extends ITTageModule with HasFoldedHistory {
  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new ITTageReq))
    val resp = Output(Valid(new ITTageResp))
    val update = Input(new ITTageUpdate)
  })

  val SRAM_SIZE=128
  val nBanks = 2
  val bankSize = nRows / nBanks
  val bankFoldWidth = if (bankSize >= SRAM_SIZE) bankSize / SRAM_SIZE else 1

  if (bankSize < SRAM_SIZE) {
    println(f"warning: ittage table $tableIdx has small sram depth of $bankSize")
  }
  val bankIdxWidth = log2Ceil(nBanks)
  def get_bank_mask(idx: UInt) = VecInit((0 until nBanks).map(idx(bankIdxWidth-1, 0) === _.U))
  def get_bank_idx(idx: UInt) = idx >> bankIdxWidth

  // override val debug = true
  // bypass entries for tage update
  val wrBypassEntries = 4

  require(histLen == 0 && tagLen == 0 || histLen != 0 && tagLen != 0)
  val idxFhInfo = (histLen, min(log2Ceil(nRows), histLen))
  val tagFhInfo = (histLen, min(histLen, tagLen))
  val altTagFhInfo = (histLen, min(histLen, tagLen-1))
  val allFhInfos = Seq(idxFhInfo, tagFhInfo, altTagFhInfo)

  def getFoldedHistoryInfo = allFhInfos.filter(_._1 >0).toSet

  def compute_tag_and_hash(unhashed_idx: UInt, allFh: AllFoldedHistories) = {
    if (histLen > 0) {
      val idx_fh = allFh.getHistWithInfo(idxFhInfo).folded_hist
      val tag_fh = allFh.getHistWithInfo(tagFhInfo).folded_hist
      val alt_tag_fh = allFh.getHistWithInfo(altTagFhInfo).folded_hist
      // require(idx_fh.getWidth == log2Ceil(nRows))
      val idx = (unhashed_idx ^ idx_fh)(log2Ceil(nRows)-1, 0)
      val tag = ((unhashed_idx >> log2Ceil(nRows)) ^ tag_fh ^ (alt_tag_fh << 1)) (tagLen - 1, 0)
      (idx, tag)
    }
    else {
      require(tagLen == 0)
      (unhashed_idx(log2Ceil(nRows)-1, 0), 0.U)
    }
  }

  def inc_ctr(ctr: UInt, taken: Bool): UInt = satUpdate(ctr, ITTageCtrBits, taken)

  class ITTageEntry() extends ITTageBundle {
    // val valid = Bool()
    val tag = UInt(tagLen.W)
    val ctr = UInt(ITTageCtrBits.W)
    val target = UInt(VAddrBits.W)
  }

  val validArray = RegInit(0.U(nRows.W))

  // Why need add instOffsetBits?
  val ittageEntrySz = 1 + tagLen + ITTageCtrBits + VAddrBits

  // pc is start address of basic block, most 2 branch inst in block
  // def getUnhashedIdx(pc: UInt) = pc >> (instOffsetBits+log2Ceil(TageBanks))
  def getUnhashedIdx(pc: UInt): UInt = pc >> instOffsetBits

  val s0_pc = io.req.bits.pc
  val s0_unhashed_idx = getUnhashedIdx(io.req.bits.pc)

  val (s0_idx, s0_tag) = compute_tag_and_hash(s0_unhashed_idx, io.req.bits.folded_hist)
  val (s1_idx, s1_tag) = (RegEnable(s0_idx, io.req.fire), RegEnable(s0_tag, io.req.fire))
  val s0_bank_req_1h = get_bank_mask(s0_idx)
  val s1_bank_req_1h = RegEnable(s0_bank_req_1h, io.req.fire)
  
  val us = Module(new Folded1WDataModuleTemplate(Bool(), nRows, 1, isSync=true, width=uFoldedWidth))
  // val table  = Module(new SRAMTemplate(new ITTageEntry, set=nRows, way=1, shouldReset=true, holdRead=true, singlePort=false))
  val table_banks = Seq.fill(nBanks)(
    Module(new FoldedSRAMTemplate(new ITTageEntry, set=nRows/nBanks, width=bankFoldWidth, shouldReset=true, holdRead=true, singlePort=true)))

  for (b <- 0 until nBanks) {
    table_banks(b).io.r.req.valid := io.req.fire && s0_bank_req_1h(b)
    table_banks(b).io.r.req.bits.setIdx := get_bank_idx(s0_idx)
  }

  us.io.raddr(0) := s0_idx

  val table_banks_r = table_banks.map(_.io.r.resp.data(0))

  val resp_selected = Mux1H(s1_bank_req_1h, table_banks_r)
  val s1_req_rhit = validArray(s1_idx) && resp_selected.tag === s1_tag
  val resp_invalid_by_write = Wire(Bool())

  io.resp.valid := (if (tagLen != 0) s1_req_rhit && !resp_invalid_by_write else true.B) // && s1_mask(b)
  io.resp.bits.ctr := resp_selected.ctr
  io.resp.bits.u := us.io.rdata(0)
  io.resp.bits.target := resp_selected.target

  val s1_bank_has_write_on_this_req = RegEnable(VecInit(table_banks.map(_.io.w.req.valid)), io.req.valid)

  // Use fetchpc to compute hash
  val (update_idx, update_tag) = compute_tag_and_hash(getUnhashedIdx(io.update.pc), io.update.folded_hist)
  val update_req_bank_1h = get_bank_mask(update_idx)
  val update_idx_in_bank = get_bank_idx(update_idx)
  val update_target = io.update.target
  val update_wdata = Wire(new ITTageEntry)



  resp_invalid_by_write := Mux1H(s1_bank_req_1h, s1_bank_has_write_on_this_req)

  for (b <- 0 until nBanks) {
    table_banks(b).io.w.apply(
      valid   = io.update.valid && update_req_bank_1h(b),
      data    = update_wdata,
      setIdx  = update_idx_in_bank,
      waymask = true.B
    )
  }

  val bank_conflict = (0 until nBanks).map(b => table_banks(b).io.w.req.valid && s0_bank_req_1h(b)).reduce(_||_)
  io.req.ready := true.B // !io.update.valid
  // io.req.ready := !bank_conflict
  XSPerfAccumulate(f"ittage_table_bank_conflict", bank_conflict)

  us.io.wen := io.update.uValid
  us.io.waddr := update_idx
  us.io.wdata := io.update.u

  val wrbypass = Module(new WrBypass(UInt(ITTageCtrBits.W), wrBypassEntries, log2Ceil(nRows)))

  wrbypass.io.wen := io.update.valid
  wrbypass.io.write_idx := update_idx
  wrbypass.io.write_data.map(_ := update_wdata.ctr)

  val old_ctr = Mux(wrbypass.io.hit, wrbypass.io.hit_data(0).bits, io.update.oldCtr)
  update_wdata.ctr   := Mux(io.update.alloc, 2.U, inc_ctr(old_ctr, io.update.correct))
  update_wdata.tag   := update_tag
  // only when ctr is null
  update_wdata.target := Mux(io.update.alloc || ctr_null(old_ctr), update_target, io.update.old_target)
  
  val newValidArray = VecInit(validArray.asBools)
  when (io.update.valid) {
    newValidArray(update_idx) := true.B
    validArray := newValidArray.asUInt
  }

  // reset all us in 32 cycles
  us.io.resetEn.map(_ := io.update.reset_u)

  XSPerfAccumulate("ittage_table_updates", io.update.valid)
  XSPerfAccumulate("ittage_table_hits", io.resp.valid)

  if (BPUDebug && debug) {
    val u = io.update
    val idx = s0_idx
    val tag = s0_tag
    XSDebug(io.req.fire,
      p"ITTageTableReq: pc=0x${Hexadecimal(io.req.bits.pc)}, " +
      p"idx=$idx, tag=$tag\n")
    XSDebug(RegNext(io.req.fire) && s1_req_rhit,
      p"ITTageTableResp: idx=$s1_idx, hit:${s1_req_rhit}, " +
      p"ctr:${io.resp.bits.ctr}, u:${io.resp.bits.u}, tar:${Hexadecimal(io.resp.bits.target)}\n")
    XSDebug(io.update.valid,
      p"update ITTAGE Table: pc:${Hexadecimal(u.pc)}}, " +
      p"correct:${u.correct}, alloc:${u.alloc}, oldCtr:${u.oldCtr}, " +
      p"target:${Hexadecimal(u.target)}, old_target:${Hexadecimal(u.old_target)}\n")
    XSDebug(io.update.valid,
      p"update ITTAGE Table: writing tag:${update_tag}, " +
      p"ctr: ${update_wdata.ctr}, target:${Hexadecimal(update_wdata.target)}" +
      p" in idx $update_idx\n")
    XSDebug(RegNext(io.req.fire) && !s1_req_rhit, "TageTableResp: no hits!\n")


    // ------------------------------Debug-------------------------------------
    val valids = RegInit(0.U.asTypeOf(Vec(nRows, Bool())))
    when (io.update.valid) { valids(update_idx) := true.B }
    XSDebug("ITTAGE Table usage:------------------------\n")
    XSDebug("%d out of %d rows are valid\n", PopCount(valids), nRows.U)
  }

}

abstract class BaseITTage(implicit p: Parameters) extends BasePredictor with ITTageParams with BPUUtils {
  // class TAGEResp {
  //   val takens = Vec(PredictWidth, Bool())
  //   val hits = Vec(PredictWidth, Bool())
  // }
  // class TAGEMeta {
  // }
  // class FromBIM {
  //   val ctrs = Vec(PredictWidth, UInt(2.W))
  // }
  // class TageIO extends DefaultBasePredictorIO {
  //   val resp = Output(new TAGEResp)
  //   val meta = Output(Vec(PredictWidth, new TageMeta))
  //   val bim = Input(new FromBIM)
  //   val s2Fire = Input(Bool())
  // }

  // override val io = IO(new TageIO)
}

class FakeITTage(implicit p: Parameters) extends BaseITTage {
  io.out <> 0.U.asTypeOf(DecoupledIO(new BasePredictorOutput))

  // io.s0_ready := true.B
  io.s1_ready := true.B
  io.s2_ready := true.B
}
// TODO: check target related logics
@chiselName
class ITTage(implicit p: Parameters) extends BaseITTage {
  override val meta_size = 0.U.asTypeOf(new ITTageMeta).getWidth

  val tables = ITTageTableInfos.zipWithIndex.map {
    case ((nRows, histLen, tagLen), i) =>
      // val t = if(EnableBPD) Module(new TageTable(nRows, histLen, tagLen, UBitPeriod)) else Module(new FakeTageTable)
      val t = Module(new ITTageTable(nRows, histLen, tagLen, UBitPeriod, i))
      t.io.req.valid := io.s0_fire
      t.io.req.bits.pc := s0_pc
      t.io.req.bits.folded_hist := io.in.bits.folded_hist
      t
  }
  override def getFoldedHistoryInfo = Some(tables.map(_.getFoldedHistoryInfo).reduce(_++_))


  val useAltOnNa = RegInit((1 << (UAONA_bits-1)).U(UAONA_bits.W))
  val tickCtr = RegInit(0.U(TickWidth.W))

  // Keep the table responses to process in s2

  val s1_resps = VecInit(tables.map(t => t.io.resp))
  val s2_resps = RegEnable(s1_resps, io.s1_fire)

  val debug_pc_s1 = RegEnable(s0_pc, io.s0_fire)
  val debug_pc_s2 = RegEnable(debug_pc_s1, io.s1_fire)
  val debug_pc_s3 = RegEnable(debug_pc_s2, io.s2_fire)

  val s2_tageTaken         = Wire(Bool())
  val s2_tageTarget        = Wire(UInt(VAddrBits.W))
  val s2_providerTarget    = Wire(UInt(VAddrBits.W))
  val s2_altProviderTarget = Wire(UInt(VAddrBits.W))
  val s2_provided          = Wire(Bool())
  val s2_provider          = Wire(UInt(log2Ceil(ITTageNTables).W))
  val s2_altProvided       = Wire(Bool())
  val s2_altProvider       = Wire(UInt(log2Ceil(ITTageNTables).W))
  val s2_finalAltPred      = Wire(Bool())
  val s2_providerU         = Wire(Bool())
  val s2_providerCtr       = Wire(UInt(ITTageCtrBits.W))
  val s2_altProviderCtr    = Wire(UInt(ITTageCtrBits.W))

  val s3_tageTaken         = RegEnable(s2_tageTaken, io.s2_fire)
  val s3_tageTarget        = RegEnable(s2_tageTarget, io.s2_fire)
  val s3_providerTarget    = RegEnable(s2_providerTarget, io.s2_fire)
  val s3_altProviderTarget = RegEnable(s2_altProviderTarget, io.s2_fire)
  val s3_provided          = RegEnable(s2_provided, io.s2_fire)
  val s3_provider          = RegEnable(s2_provider, io.s2_fire)
  val s3_altProvided       = RegEnable(s2_altProvided, io.s2_fire)
  val s3_altProvider       = RegEnable(s2_altProvider, io.s2_fire)
  val s3_finalAltPred      = RegEnable(s2_finalAltPred, io.s2_fire)
  val s3_providerU         = RegEnable(s2_providerU, io.s2_fire)
  val s3_providerCtr       = RegEnable(s2_providerCtr, io.s2_fire)
  val s3_altProviderCtr    = RegEnable(s2_altProviderCtr, io.s2_fire)

  // val updateBank = u.pc(log2Ceil(TageBanks)+instOffsetBits-1, instOffsetBits)
  val resp_meta = WireInit(0.U.asTypeOf(new ITTageMeta))

  io.out.last_stage_meta := resp_meta.asUInt

  // Update logic
  val u_valid = io.update.valid
  val update = io.update.bits
  val updateValid =
    update.is_jalr && !update.is_ret && u_valid && update.ftb_entry.jmpValid &&
    update.jmp_taken && update.cfi_idx.valid && update.cfi_idx.bits === update.ftb_entry.tailSlot.offset
  val updateFhist = update.spec_info.folded_hist

  // meta is splited by composer
  val updateMeta = update.meta.asTypeOf(new ITTageMeta)

  val updateMask      = WireInit(0.U.asTypeOf(Vec(ITTageNTables, Bool())))
  val updateUMask     = WireInit(0.U.asTypeOf(Vec(ITTageNTables, Bool())))
  val updateResetU    = WireInit(false.B)
  val updateCorrect   = Wire(Vec(ITTageNTables, Bool()))
  val updateTarget    = Wire(Vec(ITTageNTables, UInt(VAddrBits.W)))
  val updateOldTarget = Wire(Vec(ITTageNTables, UInt(VAddrBits.W)))
  val updateAlloc     = Wire(Vec(ITTageNTables, Bool()))
  val updateOldCtr    = Wire(Vec(ITTageNTables, UInt(ITTageCtrBits.W)))
  val updateU         = Wire(Vec(ITTageNTables, Bool()))
  updateCorrect   := DontCare
  updateTarget  := DontCare
  updateOldTarget  := DontCare
  updateAlloc   := DontCare
  updateOldCtr  := DontCare
  updateU       := DontCare

  // val updateTageMisPreds = VecInit((0 until numBr).map(i => updateMetas(i).taken =/= u.takens(i)))
  val updateMisPred = update.mispred_mask(numBr) // the last one indicates jmp results
  // access tag tables and output meta info
  class ITTageTableInfo(implicit p: Parameters) extends ITTageResp {
    val tableIdx = UInt(log2Ceil(ITTageNTables).W)
  }
  val inputRes = VecInit(s2_resps.zipWithIndex.map{case (r, i) => {
    val tableInfo = Wire(new ITTageTableInfo)
    tableInfo.u := r.bits.u
    tableInfo.ctr := r.bits.ctr
    tableInfo.target := r.bits.target
    tableInfo.tableIdx := i.U(log2Ceil(ITTageNTables).W)
    SelectTwoInterRes(r.valid, tableInfo)
  }})

  val selectedInfo = ParallelSelectTwo(inputRes.reverse)
  val provided = selectedInfo.hasOne
  val altProvided = selectedInfo.hasTwo
  val providerInfo = selectedInfo.first
  val altProviderInfo = selectedInfo.second
  val providerNull = providerInfo.ctr === 0.U
  
  val basePred   = true.B
  val baseTarget = io.in.bits.resp_in(0).s2.full_pred.jalr_target // use ftb pred as base target
  
  s2_tageTaken := Mux1H(Seq(
    (provided && !providerNull, providerInfo.ctr(ITTageCtrBits-1)),
    (altProvided && providerNull, altProviderInfo.ctr(ITTageCtrBits-1)),
    (!provided || providerNull && !altProvided, basePred)
  )) // TODO: reintroduce BIM
  s2_tageTarget := Mux1H(Seq(
    (provided && !providerNull, providerInfo.target),
    (altProvided && providerNull, altProviderInfo.target),
    (!provided || providerNull && !altProvided, baseTarget)
  ))
  s2_finalAltPred := Mux(altProvided, altProviderInfo.ctr(ITTageCtrBits-1), basePred)
  s2_provided       := provided
  s2_provider       := providerInfo.tableIdx
  s2_altProvided    := altProvided
  s2_altProvider    := altProviderInfo.tableIdx
  s2_providerU      := providerInfo.u
  s2_providerCtr    := providerInfo.ctr
  s2_altProviderCtr := altProviderInfo.ctr
  s2_providerTarget := providerInfo.target
  s2_altProviderTarget := altProviderInfo.target

  XSDebug(io.s2_fire, p"hit_taken_jalr:")

  when(s3_tageTaken) {
    io.out.s3.full_pred.jalr_target := s3_tageTarget
  }

  resp_meta.provider.valid    := s3_provided
  resp_meta.provider.bits     := s3_provider
  resp_meta.altProvider.valid := s3_altProvided
  resp_meta.altProvider.bits  := s3_altProvider
  resp_meta.altDiffers        := s3_finalAltPred =/= s3_tageTaken
  resp_meta.providerU         := s3_providerU
  resp_meta.providerCtr       := s3_providerCtr
  resp_meta.altProviderCtr    := s3_altProviderCtr
  resp_meta.taken             := s3_tageTaken
  resp_meta.providerTarget    := s3_providerTarget
  resp_meta.altProviderTarget := s3_altProviderTarget
  resp_meta.pred_cycle.map(_:= GTimer())
  // TODO: adjust for ITTAGE
  // Create a mask fo tables which did not hit our query, and also contain useless entries
  // and also uses a longer history than the provider
  val s2_allocatableSlots = VecInit(s2_resps.map(r => !r.valid && !r.bits.u)).asUInt &
    ~(LowerMask(UIntToOH(s2_provider), ITTageNTables) & Fill(ITTageNTables, s2_provided.asUInt))
  val s2_allocLFSR   = LFSR64()(ITTageNTables - 1, 0)
  val s2_firstEntry  = PriorityEncoder(s2_allocatableSlots)
  val s2_maskedEntry = PriorityEncoder(s2_allocatableSlots & s2_allocLFSR)
  val s2_allocEntry  = Mux(s2_allocatableSlots(s2_maskedEntry), s2_maskedEntry, s2_firstEntry)
  resp_meta.allocate.valid := RegEnable(s2_allocatableSlots =/= 0.U, io.s2_fire)
  resp_meta.allocate.bits  := RegEnable(s2_allocEntry, io.s2_fire)

  // Update in loop
  val updateRealTarget = update.full_target
  when (updateValid) {
    when (updateMeta.provider.valid) {
      val provider = updateMeta.provider.bits
      XSDebug(true.B, p"update provider $provider, pred cycle ${updateMeta.pred_cycle.getOrElse(0.U)}\n")
      val altProvider = updateMeta.altProvider.bits
      val usedAltpred = updateMeta.altProvider.valid && updateMeta.providerCtr === 0.U
      when (usedAltpred && updateMisPred) { // update altpred if used as pred
        XSDebug(true.B, p"update altprovider $altProvider, pred cycle ${updateMeta.pred_cycle.getOrElse(0.U)}\n")

        updateMask(altProvider)    := true.B
        updateUMask(altProvider)   := false.B
        updateCorrect(altProvider) := false.B
        updateOldCtr(altProvider)  := updateMeta.altProviderCtr
        updateAlloc(altProvider)   := false.B
        updateTarget(altProvider)  := updateRealTarget
        updateOldTarget(altProvider) := updateMeta.altProviderTarget
      }


      updateMask(provider)   := true.B
      updateUMask(provider)  := true.B

      updateU(provider) := Mux(!updateMeta.altDiffers, updateMeta.providerU, !updateMisPred)
      updateCorrect(provider)  := updateMeta.providerTarget === updateRealTarget
      updateTarget(provider) := updateRealTarget
      updateOldTarget(provider) := updateMeta.providerTarget
      updateOldCtr(provider) := updateMeta.providerCtr
      updateAlloc(provider)  := false.B
    }
  }

  // if mispredicted and not the case that
  // provider offered correct target but used altpred due to unconfident
  val providerCorrect = updateMeta.provider.valid && updateMeta.providerTarget === updateRealTarget
  val providerUnconf = updateMeta.providerCtr === 0.U
  when (updateValid && updateMisPred && !(providerCorrect && providerUnconf)) {
    val allocate = updateMeta.allocate
    tickCtr := satUpdate(tickCtr, TickWidth, !allocate.valid)
    when (allocate.valid) {
      XSDebug(true.B, p"allocate new table entry, pred cycle ${updateMeta.pred_cycle.getOrElse(0.U)}\n")
      updateMask(allocate.bits)  := true.B
      updateCorrect(allocate.bits) := true.B // useless for alloc
      updateTarget(allocate.bits) := updateRealTarget
      updateAlloc(allocate.bits) := true.B
      updateUMask(allocate.bits) := true.B
      updateU(allocate.bits) := false.B
    }
  }

  when (tickCtr === ((1 << TickWidth) - 1).U) {
    tickCtr := 0.U
    updateResetU := true.B
  }

  XSPerfAccumulate(s"ittage_reset_u", updateResetU)



  for (i <- 0 until ITTageNTables) {
    tables(i).io.update.valid := RegNext(updateMask(i))
    tables(i).io.update.correct := RegNext(updateCorrect(i))
    tables(i).io.update.target := RegNext(updateTarget(i))
    tables(i).io.update.old_target := RegNext(updateOldTarget(i))
    tables(i).io.update.alloc := RegNext(updateAlloc(i))
    tables(i).io.update.oldCtr := RegNext(updateOldCtr(i))

    tables(i).io.update.reset_u := RegNext(updateResetU)
    tables(i).io.update.uValid := RegNext(updateUMask(i))
    tables(i).io.update.u := RegNext(updateU(i))
    tables(i).io.update.pc := RegNext(update.pc)
    // use fetch pc instead of instruction pc
    tables(i).io.update.folded_hist := RegNext(updateFhist)
  }

  // all should be ready for req
  io.s1_ready := tables.map(_.io.req.ready).reduce(_&&_)
  XSPerfAccumulate(f"ittage_write_blocks_read", !io.s1_ready)
  // Debug and perf info

  def pred_perf(name: String, cond: Bool)   = XSPerfAccumulate(s"${name}_at_pred", cond && io.s2_fire)
  def commit_perf(name: String, cond: Bool) = XSPerfAccumulate(s"${name}_at_commit", cond && updateValid)
  def ittage_perf(name: String, pred_cond: Bool, commit_cond: Bool) = {
    pred_perf(s"ittage_${name}", pred_cond)
    commit_perf(s"ittage_${name}", commit_cond)
  }
  val pred_use_provider = s2_provided && !ctr_null(s2_providerCtr)
  val pred_use_altpred = s2_provided && ctr_null(s2_providerCtr)
  val pred_use_ht_as_altpred = pred_use_altpred && s2_altProvided
  val pred_use_bim_as_altpred = pred_use_altpred && !s2_altProvided
  val pred_use_bim_as_pred = !s2_provided

  val commit_use_provider = updateMeta.provider.valid && !ctr_null(updateMeta.providerCtr)
  val commit_use_altpred = updateMeta.provider.valid && ctr_null(updateMeta.providerCtr)
  val commit_use_ht_as_altpred = commit_use_altpred && updateMeta.altProvider.valid
  val commit_use_bim_as_altpred = commit_use_altpred && !updateMeta.altProvider.valid
  val commit_use_bim_as_pred = !updateMeta.provider.valid

  for (i <- 0 until ITTageNTables) {
    val pred_this_is_provider = s2_provider === i.U
    val pred_this_is_altpred  = s2_altProvider === i.U
    val commit_this_is_provider = updateMeta.provider.bits === i.U
    val commit_this_is_altpred  = updateMeta.altProvider.bits === i.U
    ittage_perf(s"table_${i}_final_provided",
      pred_use_provider && pred_this_is_provider,
      commit_use_provider && commit_this_is_provider
    )
    ittage_perf(s"table_${i}_provided_not_used",
      pred_use_altpred && pred_this_is_provider,
      commit_use_altpred && commit_this_is_provider
    )
    ittage_perf(s"table_${i}_alt_provider_as_final_pred",
      pred_use_ht_as_altpred && pred_this_is_altpred,
      commit_use_ht_as_altpred && commit_this_is_altpred
    )
    ittage_perf(s"table_${i}_alt_provider_not_used",
      pred_use_provider && pred_this_is_altpred,
      commit_use_provider && commit_this_is_altpred
    )
  }

  ittage_perf("provided", s2_provided, updateMeta.provider.valid)
  ittage_perf("use_provider", pred_use_provider, commit_use_provider)
  ittage_perf("use_altpred", pred_use_altpred, commit_use_altpred)
  ittage_perf("use_ht_as_altpred", pred_use_ht_as_altpred, commit_use_ht_as_altpred)
  ittage_perf("use_bim_when_no_provider", pred_use_bim_as_pred, commit_use_bim_as_pred)
  ittage_perf("use_bim_as_alt_provider", pred_use_bim_as_altpred, commit_use_bim_as_altpred)
  // XSPerfAccumulate("ittage_provider_right")
  XSPerfAccumulate("updated", updateValid)

  if (debug) {
    // for (b <- 0 until ITTageBanks) {
    //   val m = updateMetas(b)
    //   // val bri = u.metas(b)
    //   XSDebug(updateValids(b), "update(%d): pc=%x, cycle=%d, hist=%x, taken:%b, misPred:%d, bimctr:%d, pvdr(%d):%d, altDiff:%d, pvdrU:%d, pvdrCtr:%d, alloc(%d):%d\n",
    //     b.U, update.pc, 0.U, updateHist.predHist, update.full_pred.taken_mask(b), update.mispred_mask(b),
    //     0.U, m.provider.valid, m.provider.bits, m.altDiffers, m.providerU, m.providerCtr, m.allocate.valid, m.allocate.bits
    //   )
    // }
    val s2_resps = RegEnable(s1_resps, io.s1_fire)
    XSDebug("req: v=%d, pc=0x%x\n", io.s0_fire, s0_pc)
    XSDebug("s1_fire:%d, resp: pc=%x\n", io.s1_fire, debug_pc_s1)
    XSDebug("s2_fireOnLastCycle: resp: pc=%x, target=%x, hit=%b, taken=%b\n",
      debug_pc_s2, io.out.s2.getTarget, s2_provided, s2_tageTaken)
    for (i <- 0 until ITTageNTables) {
      XSDebug("TageTable(%d): valids:%b, resp_ctrs:%b, resp_us:%b, target:%x\n",
        i.U, VecInit(s2_resps(i).valid).asUInt, s2_resps(i).bits.ctr,
        s2_resps(i).bits.u, s2_resps(i).bits.target)
    }
  }
  XSDebug(updateValid, p"pc: ${Hexadecimal(update.pc)}, target: ${Hexadecimal(update.full_target)}\n")
  XSDebug(updateValid, updateMeta.toPrintable+p"\n")
  XSDebug(updateValid, p"correct(${!updateMisPred})\n")

  generatePerfEvent()
}


// class Tage_SC(implicit p: Parameters) extends Tage with HasSC {}
