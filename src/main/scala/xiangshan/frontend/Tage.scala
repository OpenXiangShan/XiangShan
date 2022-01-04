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
import chisel3.experimental.chiselName
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.transforms.RenameModules
import freechips.rocketchip.transforms.naming.RenameDesiredNames

import scala.math.min
import scala.util.matching.Regex
import os.followLink

trait TageParams extends HasBPUConst with HasXSParameter {
  // println(BankTageTableInfos)
  val TageNTables = TageTableInfos.size
  // val BankTageNTables = BankTageTableInfos.map(_.size) // Number of tage tables
  // val UBitPeriod = 256
  val TageCtrBits = 3
  val TickWidth = 8

  val TotalBits = BankTageTableInfos.map { info =>
    info.map{
      case (s, h, t) => {
        s * (1+t+TageCtrBits)
      }
    }.reduce(_+_)
  }.reduce(_+_)

  def posUnconf(ctr: UInt) = ctr === (1 << (ctr.getWidth - 1)).U
  def negUnconf(ctr: UInt) = ctr === ((1 << (ctr.getWidth - 1)) - 1).U

  def unconf(ctr: UInt) = posUnconf(ctr) || negUnconf(ctr)

}

trait HasFoldedHistory {
  val histLen: Int
  def compute_folded_hist(hist: UInt, l: Int)(histLen: Int) = {
    if (histLen > 0) {
      val nChunks = (histLen + l - 1) / l
      val hist_chunks = (0 until nChunks) map {i =>
        hist(min((i+1)*l, histLen)-1, i*l)
      }
      ParallelXOR(hist_chunks)
    }
    else 0.U
  }
  val compute_folded_ghist = compute_folded_hist(_: UInt, _: Int)(histLen)
}

abstract class TageBundle(implicit p: Parameters)
  extends XSBundle with TageParams with BPUUtils

abstract class TageModule(implicit p: Parameters)
  extends XSModule with TageParams with BPUUtils
  {}



class TageReq(implicit p: Parameters) extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val ghist = UInt(HistoryLength.W)
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
}

class TageResp(implicit p: Parameters) extends TageBundle {
  val ctrs = Vec(numBr, UInt(TageCtrBits.W))
  val u = Bool()
}

class TageUpdate(implicit p: Parameters) extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
  val ghist = UInt(HistoryLength.W)
  // update tag and ctr
  val mask = Vec(numBr, Bool())
  val takens = Vec(numBr, Bool())
  val alloc = Bool()
  val oldCtrs = Vec(numBr, UInt(TageCtrBits.W))
  // update u
  val uMask = Bool()
  val u = Bool()
  val reset_u = Bool()
}

class TageMeta(implicit p: Parameters)
  extends TageBundle with HasSCParameter
{
  val provider = ValidUndirectioned(UInt(log2Ceil(TageNTables).W))
  val providerResp = new TageResp
  val altProvider = ValidUndirectioned(UInt(log2Ceil(TageNTables).W))
  val altProviderResp = new TageResp
  val altDiffers = Vec(numBr, Bool())
  val basecnt = Vec(numBr, UInt(2.W))
  val allocate = ValidUndirectioned(UInt(log2Ceil(TageNTables).W))
  val takens = Vec(numBr, Bool())
  val scMeta = if (EnableSC) Some(new SCMeta(SCNTables)) else None
  val pred_cycle = if (!env.FPGAPlatform) Some(UInt(64.W)) else None
}

trait TBTParams extends HasXSParameter {
  val BtSize = 2048
  val bypassEntries = 8
}

@chiselName
class TageBTable(implicit p: Parameters) extends XSModule with TBTParams{
  val io = IO(new Bundle {
    val s0_fire = Input(Bool())
    val s0_pc   = Input(UInt(VAddrBits.W))
    val s1_cnt     = Output(Vec(numBr,UInt(2.W)))
    val update_mask = Input(Vec(TageBanks, Bool()))
    val update_pc = Input(UInt(VAddrBits.W))
    val update_cnt  = Input(Vec(numBr,UInt(2.W)))
    val update_takens = Input(Vec(TageBanks, Bool()))
   // val update  = Input(new TageUpdate)
  })

  val bimAddr = new TableAddr(log2Up(BtSize), 1)

  val bt = Module(new SRAMTemplate(UInt(2.W), set = BtSize, way=numBr, shouldReset = false, holdRead = true))

  val doing_reset = RegInit(true.B)
  val resetRow = RegInit(0.U(log2Ceil(BtSize).W))
  resetRow := resetRow + doing_reset
  when (resetRow === (BtSize-1).U) { doing_reset := false.B }

  val s0_idx = bimAddr.getIdx(io.s0_pc)
  bt.io.r.req.valid := io.s0_fire
  bt.io.r.req.bits.setIdx := s0_idx

  val s1_read = bt.io.r.resp.data

  io.s1_cnt := bt.io.r.resp.data

  // Update logic

  val u_idx = bimAddr.getIdx(io.update_pc)

  val newCtrs = Wire(Vec(numBr, UInt(2.W)))

  val wrbypass = Module(new WrBypass(UInt(2.W), bypassEntries, log2Up(BtSize), numWays = numBr))
  wrbypass.io.wen := io.update_mask.reduce(_||_)
  wrbypass.io.write_idx := u_idx
  wrbypass.io.write_data := newCtrs
  wrbypass.io.write_way_mask.map(_ := io.update_mask)


  val oldCtrs =
    VecInit((0 until numBr).map(i =>
      Mux(wrbypass.io.hit && wrbypass.io.hit_data(i).valid,
        wrbypass.io.hit_data(i).bits,
        io.update_cnt(i))
    ))

  def satUpdate(old: UInt, len: Int, taken: Bool): UInt = {
    val oldSatTaken = old === ((1 << len)-1).U
    val oldSatNotTaken = old === 0.U
    Mux(oldSatTaken && taken, ((1 << len)-1).U,
      Mux(oldSatNotTaken && !taken, 0.U,
        Mux(taken, old + 1.U, old - 1.U)))
  }

  val newTakens = io.update_takens
  newCtrs := VecInit((0 until numBr).map(i =>
    satUpdate(oldCtrs(i), 2, newTakens(i))
  ))

  bt.io.w.apply(
    valid = io.update_mask.reduce(_||_) || doing_reset,
    data = Mux(doing_reset, VecInit(Seq.fill(numBr)(2.U(2.W))), newCtrs),
    setIdx = Mux(doing_reset, resetRow, u_idx),
    waymask = Mux(doing_reset, Fill(numBr, 1.U(1.W)).asUInt(), io.update_mask.asUInt())
  )

}


@chiselName
class TageTable
(
  val nRows: Int, val histLen: Int, val tagLen: Int, val tableIdx: Int
)(implicit p: Parameters)
  extends TageModule with HasFoldedHistory {
  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new TageReq))
    val resp = Output(Valid(new TageResp))
    val update = Input(new TageUpdate)
  })

  class TageEntry() extends TageBundle {
    val valid = Bool()
    val tag = UInt(tagLen.W)
    val ctrs = Vec(numBr, UInt(TageCtrBits.W))
  }


  val SRAM_SIZE = 256 // physical size
  require(nRows % SRAM_SIZE == 0)
  val nBanks = 8
  val bankSize = nRows / nBanks
  val bankFoldWidth = if (bankSize >= SRAM_SIZE) bankSize / SRAM_SIZE else 1
  val uFoldedWidth = nRows / SRAM_SIZE
  if (bankSize < SRAM_SIZE) {
    println(f"warning: tage table $tableIdx has small sram depth of $bankSize")
  }
  val bankIdxWidth = log2Ceil(nBanks)
  def get_bank_mask(idx: UInt) = VecInit((0 until nBanks).map(idx(bankIdxWidth-1, 0) === _.U))
  def get_bank_idx(idx: UInt) = idx >> bankIdxWidth

  // bypass entries for tage update
  val perBankWrbypassEntries = 8

  val idxFhInfo = (histLen, min(log2Ceil(nRows), histLen))
  val tagFhInfo = (histLen, min(histLen, tagLen))
  val altTagFhInfo = (histLen, min(histLen, tagLen-1))
  val allFhInfos = Seq(idxFhInfo, tagFhInfo, altTagFhInfo)

  def getFoldedHistoryInfo = allFhInfos.filter(_._1 >0).toSet
  def compute_tag_and_hash(unhashed_idx: UInt, allFh: AllFoldedHistories) = {
    val idx_fh = allFh.getHistWithInfo(idxFhInfo).folded_hist
    val tag_fh = allFh.getHistWithInfo(tagFhInfo).folded_hist
    val alt_tag_fh = allFh.getHistWithInfo(altTagFhInfo).folded_hist
    // require(idx_fh.getWidth == log2Ceil(nRows))
    val idx = (unhashed_idx ^ idx_fh)(log2Ceil(nRows)-1, 0)
    val tag = ((unhashed_idx >> log2Ceil(nRows)) ^ tag_fh ^ (alt_tag_fh << 1)) (tagLen - 1, 0)
    (idx, tag)
  }

  def inc_ctr(ctr: UInt, taken: Bool): UInt = satUpdate(ctr, TageCtrBits, taken)
  
  if (EnableGHistDiff) {
    val idx_history = compute_folded_ghist(io.req.bits.ghist, log2Ceil(nRows))
    val idx_fh = io.req.bits.folded_hist.getHistWithInfo(idxFhInfo)
    XSError(idx_history =/= idx_fh.folded_hist, p"tage table $tableIdx has different fh," +
      p" ghist: ${Binary(idx_history)}, fh: ${Binary(idx_fh.folded_hist)}\n")
  }
  // pc is start address of basic block, most 2 branch inst in block
  // def getUnhashedIdx(pc: UInt) = pc >> (instOffsetBits+log2Ceil(TageBanks))
  def getUnhashedIdx(pc: UInt): UInt = pc >> instOffsetBits

  // val s1_pc = io.req.bits.pc
  val req_unhashed_idx = getUnhashedIdx(io.req.bits.pc)

  val us = withReset(reset.asBool || io.update.reset_u) {
    Module(new FoldedSRAMTemplate(Bool(), set=nRows, width=uFoldedWidth, shouldReset=true, holdRead=true, singlePort=true))
  }

  val table_banks = Seq.fill(nBanks)(
    Module(new FoldedSRAMTemplate(new TageEntry, set=nRows/nBanks, width=bankFoldWidth, shouldReset=true, holdRead=true, singlePort=true)))


  val (s0_idx, s0_tag) = compute_tag_and_hash(req_unhashed_idx, io.req.bits.folded_hist)
  val s0_bank_req_1h = get_bank_mask(s0_idx)

  for (b <- 0 until nBanks) {
    table_banks(b).io.r.req.valid := io.req.fire && s0_bank_req_1h(b)
    table_banks(b).io.r.req.bits.setIdx := get_bank_idx(s0_idx)
  }

  us.io.r.req.valid := io.req.fire
  us.io.r.req.bits.setIdx := s0_idx


  val s1_idx = RegEnable(s0_idx, io.req.fire)
  val s1_tag = RegEnable(s0_tag, io.req.fire)
  val s1_bank_req_1h = RegEnable(s0_bank_req_1h, io.req.fire)
  val s1_bank_has_write_last_cycle = RegNext(VecInit(table_banks.map(_.io.w.req.valid)))

  val tables_r = table_banks.map(_.io.r.resp.data(0)) // s1

  val resp_selected = Mux1H(s1_bank_req_1h, tables_r)
  val resp_invalid_by_write = Mux1H(s1_bank_req_1h, s1_bank_has_write_last_cycle)
  val req_rhit = resp_selected.valid && resp_selected.tag === s1_tag && !resp_invalid_by_write

  io.resp.valid := req_rhit
  io.resp.bits.ctrs := resp_selected.ctrs
  io.resp.bits.u := us.io.r.resp.data(0)

  if (EnableGHistDiff) {
    val update_idx_history = compute_folded_ghist(io.update.ghist, log2Ceil(nRows))
    val update_idx_info = (histLen, min(log2Ceil(nRows), histLen))
    val update_idx_fh = io.update.folded_hist.getHistWithInfo(update_idx_info)
    XSError(update_idx_history =/= update_idx_fh.folded_hist && io.update.mask.reduce(_||_),
      p"tage table $tableIdx has different fh when update," +
      p" ghist: ${Binary(update_idx_history)}, fh: ${Binary(update_idx_fh.folded_hist)}\n")
  }
  // Use fetchpc to compute hash
  val per_bank_update_wdata = Wire(Vec(nBanks, new TageEntry))

  val (update_idx, update_tag) =  compute_tag_and_hash(getUnhashedIdx(io.update.pc), io.update.folded_hist)
  val update_req_bank_1h = get_bank_mask(update_idx)
  val update_idx_in_bank = get_bank_idx(update_idx)

  val per_bank_not_silent_update = Wire(Vec(nBanks, Vec(numBr, Bool())))
  // val silent_update_from_wrbypass = Wire(Bool())

  for (b <- 0 until nBanks) {
    table_banks(b).io.w.apply(
      valid   = io.update.mask.reduce(_||_) && update_req_bank_1h(b) && per_bank_not_silent_update(b).reduce(_||_)/*  && !s0_bank_req_1h(b) */,
      data    = per_bank_update_wdata(b),
      setIdx  = update_idx_in_bank,
      waymask = true.B
    )
  }


  val bank_conflict = (0 until nBanks).map(b => table_banks(b).io.w.req.valid && s0_bank_req_1h(b)).reduce(_||_)
  io.req.ready := true.B
  // io.req.ready := !(io.update.mask && not_silent_update)
  // io.req.ready := !bank_conflict
  XSPerfAccumulate(f"tage_table_bank_conflict", bank_conflict)

  us.io.w.apply(io.update.uMask, io.update.u, update_idx, true.B)
  
  // remove silent updates
  def silentUpdate(ctr: UInt, taken: Bool) = {
    ctr.andR && taken || !ctr.orR && !taken
  }

  val bank_wrbypasses = Seq.fill(nBanks)(Seq.fill(numBr)(
    Module(new WrBypass(UInt(TageCtrBits.W), perBankWrbypassEntries, log2Ceil(nRows/nBanks), tagWidth=tagLen))
  ))

  for (b <- 0 until nBanks) {
    val update_wdata = per_bank_update_wdata(b)
    val not_silent_update = per_bank_not_silent_update(b)
    for (i <- 0 until numBr) {
      val wrbypass = bank_wrbypasses(b)(i)
      wrbypass.io.wen := io.update.mask(i) && update_req_bank_1h(b)
      wrbypass.io.write_data(0) := update_wdata.ctrs(i)
      val bypass_ctr = wrbypass.io.hit_data(0).bits
      update_wdata.ctrs(i) :=
        Mux(io.update.alloc,
          Mux(io.update.takens(i), 4.U, 3.U),
          Mux(wrbypass.io.hit,
                inc_ctr(bypass_ctr,           io.update.takens(i)),
                inc_ctr(io.update.oldCtrs(i), io.update.takens(i))
          )
        )
      not_silent_update(i) :=
        Mux(wrbypass.io.hit,
          !silentUpdate(bypass_ctr,           io.update.takens(i)),
          !silentUpdate(io.update.oldCtrs(i), io.update.takens(i))) ||
        io.update.alloc
      wrbypass.io.write_idx := get_bank_idx(update_idx)
      wrbypass.io.write_tag.map(_ := update_tag)
    }
    update_wdata.valid := true.B
    update_wdata.tag   := update_tag
  }

  for (i <- 0 until numBr) {
    for (b <- 0 until nBanks) {
      val wrbypass = bank_wrbypasses(b)(i)
      XSPerfAccumulate(f"tage_table_bank_${b}_wrbypass_enq_$i", io.update.mask(i) && update_req_bank_1h(b) && !wrbypass.io.hit)
      XSPerfAccumulate(f"tage_table_bank_${b}_wrbypass_hit_$i", io.update.mask(i) && update_req_bank_1h(b) &&  wrbypass.io.hit)
    }
  }

  for (b <- 0 until nBanks) {
    val not_silent_update = per_bank_not_silent_update(b)
    XSPerfAccumulate(f"tage_table_bank_${b}_real_updates",
      io.update.mask.reduce(_||_) && update_req_bank_1h(b) && not_silent_update.reduce(_||_))
    XSPerfAccumulate(f"tage_table_bank_${b}_silent_updates_eliminated",
      io.update.mask.reduce(_||_) && update_req_bank_1h(b) && !not_silent_update.reduce(_||_))
  }

  XSPerfAccumulate("tage_table_hits", PopCount(io.resp.valid))
  
  for (b <- 0 until nBanks) {
    XSPerfAccumulate(f"tage_table_bank_${b}_update_req", io.update.mask.reduce(_||_) && update_req_bank_1h(b))
  }

  val u = io.update
  val b = PriorityEncoder(u.mask)
  val ub = PriorityEncoder(u.uMask)
  XSDebug(io.req.fire,
    p"tableReq: pc=0x${Hexadecimal(io.req.bits.pc)}, " +
    p"idx=$s0_idx, tag=$s0_tag\n")
  XSDebug(RegNext(io.req.fire) && req_rhit,
    p"TageTableResp: idx=$s1_idx, hit:$req_rhit, " +
    p"ctrs:${io.resp.bits.ctrs}, u:${io.resp.bits.u}\n")
  XSDebug(io.update.mask.reduce(_||_),
    p"update Table: pc:${Hexadecimal(u.pc)}}, " +
    p"takens:${u.takens.asUInt}, alloc:${u.alloc}, oldCtrs:${u.oldCtrs.asUInt}\n")
  XSDebug(io.update.mask.reduce(_||_),
    p"update Table: writing tag:$update_tag, " +
    p"ctrs: ${Mux1H(update_req_bank_1h, per_bank_update_wdata).ctrs.asUInt} in idx ${update_idx}\n")
  XSDebug(RegNext(io.req.fire) && !req_rhit, "TageTableResp: not hit!\n")

  // ------------------------------Debug-------------------------------------
  val valids = Reg(Vec(nRows, Bool()))
  when (reset.asBool) { valids.foreach(r => r := false.B) }
  when (io.update.mask.reduce(_||_)) { valids(update_idx) := true.B }
  XSDebug("Table usage:------------------------\n")
  XSDebug("%d out of %d rows are valid\n", PopCount(valids), nRows.U)

}

abstract class BaseTage(implicit p: Parameters) extends BasePredictor with TageParams with BPUUtils {
}

class FakeTage(implicit p: Parameters) extends BaseTage {
  io.out <> 0.U.asTypeOf(DecoupledIO(new BasePredictorOutput))

  // io.s0_ready := true.B
  io.s1_ready := true.B
  io.s2_ready := true.B
}

@chiselName
class Tage(implicit p: Parameters) extends BaseTage {

  val resp_meta = Wire(new TageMeta)
  override val meta_size = resp_meta.getWidth
  val tables = TageTableInfos.zipWithIndex.map {
    case ((nRows, histLen, tagLen), i) => {
      val t = Module(new TageTable(nRows, histLen, tagLen, i))
      t.io.req.valid := io.s0_fire
      t.io.req.bits.pc := s0_pc
      t.io.req.bits.folded_hist := io.in.bits.folded_hist
      t.io.req.bits.ghist := io.in.bits.ghist
      t
    }
  }
  val bt = Module (new TageBTable)
  bt.io.s0_fire := io.s0_fire
  bt.io.s0_pc   := s0_pc

  val tickCtr = RegInit(0.U(TickWidth.W))

  val tage_fh_info = tables.map(_.getFoldedHistoryInfo).reduce(_++_).toSet
  override def getFoldedHistoryInfo = Some(tage_fh_info)

  val s1_resps = VecInit(tables.map(_.io.resp))

  //val s1_bim = io.in.bits.resp_in(0).s1.full_pred
  // val s2_bim = RegEnable(s1_bim, enable=io.s1_fire)

  val debug_pc_s0 = s0_pc
  val debug_pc_s1 = RegEnable(s0_pc, enable=io.s0_fire)
  val debug_pc_s2 = RegEnable(debug_pc_s1, enable=io.s1_fire)

  val s1_provided        = Wire(Bool())
  val s1_provider        = Wire(UInt(log2Ceil(TageNTables).W))
  val s1_providerResp    = Wire(new TageResp)
  val s1_altProvided     = Wire(Bool())
  val s1_altProvider     = Wire(UInt(log2Ceil(TageNTables).W))
  val s1_altProviderResp = Wire(new TageResp)
  val s1_tageTakens      = Wire(Vec(numBr, Bool()))
  val s1_finalAltPreds   = Wire(Vec(numBr, Bool()))
  val s1_basecnts        = Wire(Vec(TageBanks, UInt(2.W)))

  val s2_provided        = RegEnable(s1_provided, io.s1_fire)
  val s2_provider        = RegEnable(s1_provider, io.s1_fire)
  val s2_providerResp    = RegEnable(s1_providerResp, io.s1_fire)
  val s2_altProvided     = RegEnable(s1_altProvided, io.s1_fire)
  val s2_altProvider     = RegEnable(s1_altProvider, io.s1_fire)
  val s2_altProviderResp = RegEnable(s1_altProviderResp, io.s1_fire)  
  val s2_tageTakens      = RegEnable(s1_tageTakens, io.s1_fire)
  val s2_finalAltPreds   = RegEnable(s1_finalAltPreds, io.s1_fire)
  val s2_basecnts        = RegEnable(s1_basecnts, io.s1_fire)

  io.out.resp := io.in.bits.resp_in(0)
  io.out.last_stage_meta := resp_meta.asUInt

  // val ftb_hit = io.in.bits.resp_in(0).s2.full_pred.hit
  val ftb_entry = io.in.bits.resp_in(0).s2.ftb_entry
  val resp_s2 = io.out.resp.s2

  // Update logic
  val u_valid = io.update.valid
  val update = io.update.bits
  val updateValids = VecInit((0 until TageBanks).map(w =>
      update.ftb_entry.brValids(w) && u_valid && !update.ftb_entry.always_taken(w) &&
      !(PriorityEncoder(update.full_pred.br_taken_mask) < w.U)))
  val updateFHist = update.folded_hist

  val updateMeta = update.meta.asTypeOf(new TageMeta)

  val updateMask    = WireInit(0.U.asTypeOf(Vec(numBr, Vec(TageNTables, Bool()))))
  val updateUMask   = WireInit(0.U.asTypeOf(Vec(TageNTables, Bool())))
  val updateResetU  = WireInit(false.B) // per predictor
  val updateTakens   = Wire(Vec(numBr, Vec(TageNTables, Bool())))
  val updateAlloc   = WireInit(0.U.asTypeOf(Vec(TageNTables, Bool())))
  val updateOldCtrs  = Wire(Vec(numBr, Vec(TageNTables, UInt(TageCtrBits.W))))
  val updateU       = Wire(Vec(TageNTables, Bool()))
  val updatebcnt    = Wire(Vec(TageBanks, UInt(2.W)))
  val baseupdate    = WireInit(0.U.asTypeOf(Vec(TageBanks, Bool())))
  val bUpdateTakens = Wire(Vec(TageBanks, Bool()))
  updateTakens  := DontCare
  updateOldCtrs  := DontCare
  updateU       := DontCare

  val updateMisPreds = update.mispred_mask

  class TageTableInfo(implicit p: Parameters) extends XSBundle {
    val resp = new TageResp
    val tableIdx = UInt(log2Ceil(TageNTables).W)
  }
  // access tag tables and output meta info

  val inputRes = VecInit(s1_resps.zipWithIndex.map{case (r, i) => {
    val tableInfo = Wire(new TageTableInfo)
    tableInfo.resp := r.bits
    tableInfo.tableIdx := i.U(log2Ceil(TageNTables).W)
    SelectTwoInterRes(r.valid, tableInfo)
  }})

  val selectedInfo = ParallelSelectTwo(inputRes.reverse)
  val provided = selectedInfo.hasOne
  val altProvided = selectedInfo.hasTwo
  val providerInfo = selectedInfo.first
  val altProviderInfo = selectedInfo.second
  
  s1_provided      := provided
  s1_provider      := providerInfo.tableIdx
  s1_providerResp  := providerInfo.resp
  s1_altProvided   := altProvided
  s1_altProvider   := altProviderInfo.tableIdx
  s1_altProviderResp := altProviderInfo.resp

  resp_meta.provider.valid    := RegEnable(s2_provided, io.s2_fire)
  resp_meta.provider.bits     := RegEnable(s2_provider, io.s2_fire)
  resp_meta.providerResp      := RegEnable(s2_providerResp, io.s2_fire)
  resp_meta.altProvider.valid := RegEnable(s2_altProvided, io.s2_fire)
  resp_meta.altProvider.bits  := RegEnable(s2_altProvider, io.s2_fire)
  resp_meta.altProviderResp   := RegEnable(s2_altProviderResp, io.s2_fire)
  resp_meta.pred_cycle.map(_ := RegEnable(GTimer(), io.s2_fire))
  
  // Create a mask fo tables which did not hit our query, and also contain useless entries
  // and also uses a longer history than the provider
  val allocatableSlots =
    RegEnable(
      VecInit(s1_resps.map(r => !r.valid && !r.bits.u)).asUInt &
        ~(LowerMask(UIntToOH(s1_provider), TageNTables) &
          Fill(TageNTables, s1_provided.asUInt)),
      io.s1_fire
    )
  val allocLFSR   = LFSR64()(TageNTables - 1, 0)
  val firstEntry  = PriorityEncoder(allocatableSlots)
  val maskedEntry = PriorityEncoder(allocatableSlots & allocLFSR)
  val allocEntry  = Mux(allocatableSlots(maskedEntry), maskedEntry, firstEntry)
  resp_meta.allocate.valid := RegEnable(allocatableSlots =/= 0.U, io.s2_fire)
  resp_meta.allocate.bits  := RegEnable(allocEntry, io.s2_fire)

  val updateProvided    = updateMeta.provider.valid
  val updateProvider    = updateMeta.provider.bits
  val updateAltProvided = updateMeta.altProvider.valid
  val updateAltProvider = updateMeta.altProvider.bits

  val hasUpdate = updateValids.reduce(_||_)
  val needToAllocate = (updateValids zip updateMisPreds).zipWithIndex.map {
    case ((v, m), i) => v && m && !(updateProvided &&
      (negUnconf(updateMeta.providerResp.ctrs(i)) && !update.full_pred.br_taken_mask(i) ||
       posUnconf(updateMeta.providerResp.ctrs(i)) &&  update.full_pred.br_taken_mask(i))
    )
  }.reduce(_||_)
  val canAllocate = updateMeta.allocate.valid
  val decrMask = Mux(updateProvided,
    ~LowerMask(UIntToOH(updateProvider), TageNTables),
    0.U(TageNTables.W)
  )
  for (i <- 0 until TageNTables) {
    updateUMask(i) := hasUpdate && (
      updateProvided && updateProvider === i.U ||
      needToAllocate && (canAllocate && updateMeta.allocate.bits === i.U || !canAllocate && decrMask(i))
    )
    updateU(i) := updateProvided && updateProvider === i.U && (
      !updateMeta.altDiffers.reduce(_||_) && updateMeta.providerResp.u ||
       updateMeta.altDiffers.reduce(_||_) && !updateMisPreds.reduce(_||_)
    )
    updateAlloc(i) := needToAllocate && updateMeta.allocate.bits === i.U && canAllocate
  }

  when (needToAllocate) {
    tickCtr := satUpdate(tickCtr, TickWidth, !canAllocate)
  }

  XSPerfAccumulate(f"tage_update_allocate_failure", needToAllocate && !canAllocate)
  XSPerfAccumulate(f"tage_update_allocate_success", needToAllocate &&  canAllocate)

  when (tickCtr === ((1 << TickWidth) - 1).U) {
    tickCtr := 0.U
    updateResetU := true.B
  }

  for (w <- 0 until TageBanks) {
    val providerUnconf = unconf(providerInfo.resp.ctrs(w))

    s1_tageTakens(w)     := Mux1H(Seq(
      (provided && !providerUnconf, providerInfo.resp.ctrs(w)(TageCtrBits-1)),
      (altProvided && providerUnconf, altProviderInfo.resp.ctrs(w)(TageCtrBits-1)),
      (!provided, bt.io.s1_cnt(w)(1))
    ))
    s1_finalAltPreds(w)  := Mux(altProvided,
      altProviderInfo.resp.ctrs(w)(TageCtrBits-1),
      bt.io.s1_cnt(w)(1)
    )
    s1_basecnts(w)       := bt.io.s1_cnt(w)

    resp_meta.altDiffers(w) := RegEnable(s2_finalAltPreds(w) =/= s2_tageTakens(w), io.s2_fire)
    resp_meta.takens(w)     := RegEnable(s2_tageTakens(w), io.s2_fire)
    resp_meta.basecnt(w)    := RegEnable(s2_basecnts(w), io.s2_fire)


    // Update in loop
    val updateValid = updateValids(w)
    val isUpdateTaken = updateValid && update.full_pred.br_taken_mask(w)
    val updateMisPred = updateMisPreds(w)

    val updateProviderWeakTaken = posUnconf(updateMeta.providerResp.ctrs(w))
    val updateProviderWeaknotTaken = negUnconf(updateMeta.providerResp.ctrs(w))
    val updateProviderWeak = unconf(updateMeta.providerResp.ctrs(w))

    updateTakens(w).map(_ := isUpdateTaken)

    when (updateValid) {
      when (updateProvided) {
        when (updateMisPred && updateAltProvided && updateProviderWeak) {
          updateMask(w)(updateAltProvider) := true.B
          updateOldCtrs(w)(updateAltProvider) := updateMeta.altProviderResp.ctrs(w)
        }

        updateMask(w)(updateProvider)   := true.B
        updateOldCtrs(w)(updateProvider) := updateMeta.providerResp.ctrs(w)
      }
    }

    // update base table if used base table to predict
    baseupdate(w) := updateValid && (
      (updateProvided && !updateAltProvided && updateMisPred && updateProviderWeak) ||
      !updateProvided
    )
    updatebcnt(w) := updateMeta.basecnt(w)
    bUpdateTakens(w) := isUpdateTaken

    // if mispredicted and not the case that
    // provider offered correct target but used altpred due to unconfident
    when (updateValid && updateMisPred && 
      ~((updateProviderWeaknotTaken && ~isUpdateTaken ||
        updateProviderWeakTaken && isUpdateTaken) &&
        updateProvided)
    ) {
      when (canAllocate) {
        updateMask(w)(updateMeta.allocate.bits)  := true.B
      }
    }
    XSPerfAccumulate(s"tage_bank_${w}_mispred", updateValid && updateMisPred)
  }
  XSPerfAccumulate(s"tage_reset_u", updateResetU)



  for (i <- 0 until numBr) {
    resp_s2.full_pred.br_taken_mask(i) := s2_tageTakens(i)
  }

  for (w <- 0 until TageBanks) {
    for (i <- 0 until TageNTables) {
      tables(i).io.update.mask(w)    := RegNext(updateMask(w)(i))
      tables(i).io.update.takens(w)  := RegNext(updateTakens(w)(i))
      tables(i).io.update.alloc      := RegNext(updateAlloc(i))
      tables(i).io.update.oldCtrs(w) := RegNext(updateOldCtrs(w)(i))

      tables(i).io.update.uMask    := RegNext(updateUMask(i))
      tables(i).io.update.u        := RegNext(updateU(i))
      tables(i).io.update.reset_u  := RegNext(updateResetU)
      // use fetch pc instead of instruction pc
      tables(i).io.update.pc       := RegNext(update.pc)
      tables(i).io.update.folded_hist := RegNext(updateFHist)
      tables(i).io.update.ghist := RegNext(io.update.bits.ghist)
    }
  }
  bt.io.update_mask := RegNext(baseupdate)
  bt.io.update_cnt := RegNext(updatebcnt)
  bt.io.update_pc := RegNext(update.pc)
  bt.io.update_takens := RegNext(bUpdateTakens)

  // all should be ready for req
  io.s1_ready := tables.map(_.io.req.ready).reduce(_&&_)
  XSPerfAccumulate(f"tage_write_blocks_read", !io.s1_ready)

  def pred_perf(name: String, cnt: UInt)   = XSPerfAccumulate(s"${name}_at_pred", cnt)
  def commit_perf(name: String, cnt: UInt) = XSPerfAccumulate(s"${name}_at_commit", cnt)
  def tage_perf(name: String, pred_cnt: UInt, commit_cnt: UInt) = {
    pred_perf(name, pred_cnt)
    commit_perf(name, commit_cnt)
  }

  // Debug and perf info
  for (b <- 0 until TageBanks) {
    for (i <- 0 until TageNTables) {
      val pred_i_provided =
        s2_provided && s2_provider === i.U
      val commit_i_provided =
        updateProvided && updateProvider === i.U && updateValids(b)
      tage_perf(
        s"bank_${b}_tage_table_${i}_provided",
        PopCount(pred_i_provided),
        PopCount(commit_i_provided)
      )
    }
    tage_perf(
      s"bank_${b}_tage_use_bim",
      PopCount(!s2_provided),
      PopCount(!updateProvided && updateValids(b))
    )
    def unconf(providerCtr: UInt) = providerCtr === 3.U || providerCtr === 4.U
    tage_perf(
      s"bank_${b}_tage_use_altpred",
      PopCount(s2_provided && unconf(s2_providerResp.ctrs(b))),
      PopCount(updateProvided &&
        unconf(updateMeta.providerResp.ctrs(b)) && updateValids(b))
    )
    tage_perf(
      s"bank_${b}_tage_provided",
      PopCount(s2_provided),
      PopCount(updateProvided && updateValids(b))
    )
  }

  for (b <- 0 until TageBanks) {
    val m = updateMeta
    // val bri = u.metas(b)
    XSDebug(updateValids(b), "update(%d): pc=%x, cycle=%d, taken:%b, misPred:%d, bimctr:%d, pvdr(%d):%d, altDiff:%d, pvdrU:%d, pvdrCtr:%d, alloc(%d):%d\n",
      b.U, update.pc, 0.U, update.full_pred.br_taken_mask(b), update.mispred_mask(b),
      0.U, m.provider.valid, m.provider.bits, m.altDiffers(b), m.providerResp.u,
      m.providerResp.ctrs(b), m.allocate.valid, m.allocate.bits
    )
  }
  val s2_resps = RegEnable(s1_resps, io.s1_fire)
  XSDebug("req: v=%d, pc=0x%x\n", io.s0_fire, s0_pc)
  XSDebug("s1_fire:%d, resp: pc=%x\n", io.s1_fire, debug_pc_s1)
  XSDebug("s2_fireOnLastCycle: resp: pc=%x, target=%x, hit=%b, takens=%b\n",
    debug_pc_s2, io.out.resp.s2.getTarget, s2_provided, s2_tageTakens.asUInt)

  for (b <- 0 until TageBanks) {
    for (i <- 0 until TageNTables) {
      XSDebug("bank(%d)_tage_table(%d): valid:%b, resp_ctr:%d, resp_us:%d\n",
        b.U, i.U, s2_resps(i).valid, s2_resps(i).bits.ctrs(b), s2_resps(i).bits.u)
    }
  }
    // XSDebug(io.update.valid && updateIsBr, p"update: sc: ${updateSCMeta}\n")
    // XSDebug(true.B, p"scThres: use(${useThreshold}), update(${updateThreshold})\n")
}


class Tage_SC(implicit p: Parameters) extends Tage with HasSC {}
