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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._

import scala.math.min
import scala.util.matching.Regex
import scala.{Tuple2 => &}

trait ITTageParams extends HasXSParameter with HasBPUParameter {

  val ITTageNTables = ITTageTableInfos.size // Number of tage tables
  val UBitPeriod = 2048
  val ITTageCtrBits = 2
  val uFoldedWidth = 16
  val TickWidth = 8
  val ITTageUsBits = 1
  def ctr_null(ctr: UInt, ctrBits: Int = ITTageCtrBits) = {
    ctr === 0.U
  }
  def ctr_unconf(ctr: UInt, ctrBits: Int = ITTageCtrBits) = {
    ctr < (1 << (ctrBits-1)).U
  }
  val UAONA_bits = 4

  val TotalBits = ITTageTableInfos.map {
    case (s, h, t) => {
      s * (1+t+ITTageCtrBits+ITTageUsBits+VAddrBits)
    }
  }.reduce(_+_)
}
// reuse TAGE implementation

trait ITTageHasFoldedHistory {
  def compute_folded_hist(hist: UInt, histLen: Int, l: Int) = {
    if (histLen > 0) {
      val nChunks = (histLen + l - 1) / l
      val hist_chunks = (0 until nChunks) map {i =>
        hist(min((i+1)*l, histLen)-1, i*l)
      }
      ParallelXOR(hist_chunks)
    }
    else 0.U
  }
  val compute_folded_ghist = compute_folded_hist(_: UInt, _: Int, _: Int)
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
  // TODO: check if we need target info here
  val pred_cycle = if (!env.FPGAPlatform) Some(UInt(64.W)) else None
}


class FakeITTageTable()(implicit p: Parameters) extends ITTageModule {
  val io = IO(new Bundle() {
    val req = Input(Valid(new ITTageReq))
    val resp = Output(Valid(new ITTageResp))
    val update = Input(new ITTageUpdate)
  })
  io.resp := DontCare

}

class ITTageTable
(
  val nRows: Int, val histLen: Int, val tagLen: Int, val uBitPeriod: Int, val tableIdx: Int
)(implicit p: Parameters)
  extends ITTageModule with HasFoldedHistory {
  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new ITTageReq))
    val resp = Output(Valid(new ITTageResp))
    val update = Flipped(DecoupledIO(new ITTageUpdate))
  })

  val SRAM_SIZE=128

  val foldedWidth = if (nRows >= SRAM_SIZE) nRows / SRAM_SIZE else 1

  if (nRows < SRAM_SIZE) {
    println(f"warning: ittage table $tableIdx has small sram depth of $nRows")
  }

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
    val valid = Bool()
    val tag = UInt(tagLen.W)
    val ctr = UInt(ITTageCtrBits.W)
    val target = UInt(VAddrBits.W)
    val useful = Bool()
  }

  // Why need add instOffsetBits?
  val ittageEntrySz = 1 + tagLen + ITTageCtrBits + ITTageUsBits + VAddrBits

  // pc is start address of basic block, most 2 branch inst in block
  // def getUnhashedIdx(pc: UInt) = pc >> (instOffsetBits+log2Ceil(TageBanks))
  def getUnhashedIdx(pc: UInt): UInt = pc >> instOffsetBits

  val s0_valid = io.req.valid
  val s0_pc = io.req.bits.pc
  val s0_unhashed_idx = getUnhashedIdx(io.req.bits.pc)

  val (s0_idx, s0_tag) = compute_tag_and_hash(s0_unhashed_idx, io.req.bits.folded_hist)
  val (s1_idx, s1_tag) = (RegEnable(s0_idx, io.req.fire), RegEnable(s0_tag, io.req.fire))
  val s1_valid = RegNext(s0_valid)

  val table = Module(new FoldedSRAMTemplate(
    new ITTageEntry, set=nRows, width=foldedWidth, shouldReset=true, holdRead=true, singlePort=true, useBitmask=true))

  table.io.r.req.valid := io.req.fire
  table.io.r.req.bits.setIdx := s0_idx

  val table_read_data = table.io.r.resp.data(0)

  val s1_req_rhit = table_read_data.valid && table_read_data.tag === s1_tag

  val read_write_conflict = io.update.valid && io.req.valid
  val s1_read_write_conflict = RegEnable(read_write_conflict, io.req.valid)

  io.resp.valid := (if (tagLen != 0) s1_req_rhit else true.B) && s1_valid // && s1_mask(b)
  io.resp.bits.ctr := table_read_data.ctr
  io.resp.bits.u := table_read_data.useful
  io.resp.bits.target := table_read_data.target

  // Use updating buffered folded_hist
  val update_folded_hist = io.update.bits.folded_hist

  val (update_idx, update_tag) = compute_tag_and_hash(getUnhashedIdx(io.update.bits.pc), update_folded_hist)
  val update_target = io.update.bits.target
  val update_wdata = Wire(new ITTageEntry)


  val updateAllBitmask = VecInit.fill(ittageEntrySz)(1.U).asUInt                        //update all entry
  val updateNoBitmask = VecInit.fill(ittageEntrySz)(0.U).asUInt                         //update no
  val updateNoUsBitmask = VecInit.tabulate(ittageEntrySz)(_.U >= ITTageUsBits.U).asUInt //update others besides useful bit
  val updateUsBitmask = VecInit.tabulate(ittageEntrySz)(_.U < ITTageUsBits.U).asUInt    //update useful bit
  val update_buff_valid = RegInit(false.B)
  val needReset = RegInit(false.B)
  val useful_can_reset = !(io.req.fire || io.update.valid || update_buff_valid) && needReset
  val (resetSet, resetFinish) = Counter(useful_can_reset, nRows)
  when (io.update.bits.reset_u) {
    needReset := true.B
  }.elsewhen (resetFinish) {
    needReset := false.B
  }
  val update_bitmask =  Mux(io.update.bits.uValid && io.update.valid,
                          updateAllBitmask,
                          Mux(io.update.valid, updateNoUsBitmask,
                            Mux(useful_can_reset, updateUsBitmask, updateNoBitmask)
                          ))

  val update_buff_data    = RegEnable(update_wdata, read_write_conflict)
  val update_buff_idx     = RegEnable(update_idx, read_write_conflict)
  val update_buff_bitmask = RegEnable(update_bitmask(ITTageUsBits - 1, 0), read_write_conflict)  //for update bitmask only need store bitmask(0)
  when(read_write_conflict){
    update_buff_valid := true.B
  }.elsewhen(!io.req.fire){
    update_buff_valid := false.B
  }

  table.io.w.apply(
    valid   = (io.update.valid || update_buff_valid) && !io.req.fire || useful_can_reset, //read first
    data    = Mux(update_buff_valid, update_buff_data, update_wdata),
    setIdx  = Mux(useful_can_reset, resetSet, Mux(update_buff_valid, update_buff_idx, update_idx)),
    waymask = true.B,
    bitmask = Mux(update_buff_valid, Cat(Fill(ittageEntrySz - ITTageUsBits, 1.U), update_buff_bitmask), update_bitmask)
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
  io.update.ready := !update_buff_valid
  XSPerfAccumulate("ittage_table_read_write_conflict", read_write_conflict)
  XSPerfAccumulate("ittage_table_update_buff_valid", update_buff_valid)

  val old_ctr = io.update.bits.oldCtr
  update_wdata.valid := true.B
  update_wdata.ctr   := Mux(io.update.bits.alloc, 2.U, inc_ctr(old_ctr, io.update.bits.correct))
  update_wdata.tag   := update_tag
  update_wdata.useful:= Mux(useful_can_reset, false.B, io.update.bits.u)
  // only when ctr is null
  update_wdata.target := Mux(io.update.bits.alloc || ctr_null(old_ctr), update_target, io.update.bits.old_target)

  XSPerfAccumulate("ittage_table_updates", io.update.valid)
  XSPerfAccumulate("ittage_table_hits", io.resp.valid)
  XSPerfAccumulate("ittage_us_tick_reset", io.update.bits.reset_u)

  if (BPUDebug && debug) {
    val u = io.update.bits
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

abstract class BaseITTage(implicit p: Parameters) extends BasePredictor with ITTageParams with ITTageHasFoldedHistory with BPUUtils {
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
class ITTage(implicit p: Parameters) extends BaseITTage {
  override val meta_size = 0.U.asTypeOf(new ITTageMeta).getWidth

  val tables = ITTageTableInfos.zipWithIndex.map {
    case ((nRows, histLen, tagLen), i) =>
      // val t = if(EnableBPD) Module(new TageTable(nRows, histLen, tagLen, UBitPeriod)) else Module(new FakeTageTable)
      val t = Module(new ITTageTable(nRows, histLen, tagLen, UBitPeriod, i))
      t
  }
  override def getFoldedHistoryInfo = Some(tables.map(_.getFoldedHistoryInfo).reduce(_++_))


  val useAltOnNa = RegInit((1 << (UAONA_bits-1)).U(UAONA_bits.W))
  val tickCtr = RegInit(0.U(TickWidth.W))

  // uftb miss or hasIndirect
  val s1_uftbHit = io.in.bits.resp_in(0).s1_uftbHit
  val s1_uftbHasIndirect = io.in.bits.resp_in(0).s1_uftbHasIndirect
  val s1_isIndirect = (!s1_uftbHit && !io.in.bits.resp_in(0).s1_ftbCloseReq) || s1_uftbHasIndirect

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

  // val updateBank = u.pc(log2Ceil(TageBanks)+instOffsetBits-1, instOffsetBits)
  val resp_meta = WireInit(0.U.asTypeOf(new ITTageMeta))

  io.out.last_stage_meta := resp_meta.asUInt

  // Update logic
  val u_valid = io.update.valid
  val update = io.update.bits
  val updateValid =
    update.is_jalr && !update.is_ret && u_valid && update.ftb_entry.jmpValid &&
    update.jmp_taken && update.cfi_idx.valid && update.cfi_idx.bits === update.ftb_entry.tailSlot.offset

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
  val delay_updateMisPred = WireInit(0.U.asTypeOf(update.mispred_mask(numBr))) // the last one indicates jmp results

  //Using ghist create folded_hist
  val ufolded_hist = WireInit(0.U.asTypeOf(new AllFoldedHistories(foldedGHistInfos)))
  val ittageFoldedGHistInfos = (ITTageTableInfos.map{ case (nRows, h, t) =>
    if (h > 0)
      Set((h, min(log2Ceil(nRows), h)), (h, min(h, t)), (h, min(h, t-1)))
    else
      Set[FoldedHistoryInfo]()
    }.reduce(_++_).toSet).toList
  ittageFoldedGHistInfos.map{
    case (h, l) =>
      ufolded_hist.getHistWithInfo((h, l)).folded_hist := compute_folded_hist(update.ghist, h, l)
  }

  //updating read
  val u_req_buff_valid = RegInit(false.B)
  val u_req_stall_ftq  = RegInit(false.B)
  val delay_full_target = RegInit(0.U.asTypeOf(update.full_target)) //if update target need buffer
  val delay_u_pc     = RegInit(0.U.asTypeOf(update.pc))
  val delay_ufolded_hist = RegInit(0.U.asTypeOf(new AllFoldedHistories(foldedGHistInfos)))

  val u_req_valid  = (updateValid || u_req_buff_valid) && !(io.s1_fire(3) && s1_isIndirect)
  val u_req_valid_reg = RegNext(u_req_valid)
  val u_req_conflict = updateValid && io.s1_fire(3) && s1_isIndirect

  //when a conflict occurs, need storage the update data.
  //If a read request is sent during the update process，it need clean
  when(u_req_conflict){
    u_req_buff_valid := true.B
  }.elsewhen(u_req_valid){
    u_req_buff_valid := false.B
  }
  //For reading during updates, even if there are no conflicts, it is necessary to block FTQ and store the update information.
  //Release when it can be written
  when(updateValid){
    u_req_stall_ftq := true.B
    delay_full_target := update.full_target
    delay_u_pc := update.pc
    delay_updateMisPred := update.mispred_mask(numBr)
    delay_ufolded_hist := ufolded_hist
  }.elsewhen(u_req_valid_reg){
    u_req_stall_ftq := false.B
    delay_full_target := 0.U.asTypeOf(update.full_target)
    delay_ufolded_hist := 0.U.asTypeOf(new AllFoldedHistories(foldedGHistInfos))
    delay_updateMisPred := update.mispred_mask(numBr)
  }

  //Read during updates
  val u_providerTarget    = s2_providerTarget
  val u_altProviderTarget = s2_altProviderTarget
  val u_provided          = s2_provided
  val u_provider          = s2_provider
  val u_altProvided       = s2_altProvided
  val u_altProvider       = s2_altProvider
  val u_providerU         = s2_providerU
  val u_providerCtr       = s2_providerCtr
  val u_altProviderCtr    = s2_altProviderCtr
  val u_allocate          = Wire(ValidUndirectioned(UInt(log2Ceil(ITTageNTables).W)))
  val u_altDiffers        = Mux(u_altProvided, u_altProviderCtr(ITTageCtrBits-1), true.B) =/= 1.B

  // Predict
  tables.map { t => {
      t.io.req.valid := io.s1_fire(3) && s1_isIndirect || u_req_valid
      t.io.req.bits.pc := Mux(u_req_valid, Mux(u_req_buff_valid, delay_u_pc, update.pc), s1_pc_dup(3))
      t.io.req.bits.folded_hist := Mux(u_req_valid, Mux(u_req_buff_valid, delay_ufolded_hist, ufolded_hist), io.in.bits.s1_folded_hist(3))
    }
  }

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

  val baseTarget = io.in.bits.resp_in(0).s2.full_pred(3).jalr_target // use ftb pred as base target

  s2_tageTarget := Mux1H(Seq(
    (provided && !(providerNull && altProvided), providerInfo.target),
    (altProvided && providerNull, altProviderInfo.target),
    (!provided, baseTarget)
  ))
  s2_provided       := provided
  s2_provider       := providerInfo.tableIdx
  s2_altProvided    := altProvided
  s2_altProvider    := altProviderInfo.tableIdx
  s2_providerU      := providerInfo.u
  s2_providerCtr    := providerInfo.ctr
  s2_altProviderCtr := altProviderInfo.ctr
  s2_providerTarget := providerInfo.target
  s2_altProviderTarget := altProviderInfo.target

  XSDebug(io.s2_fire(3), p"hit_taken_jalr:")

  for (fp & s3_tageTarget <-
    io.out.s3.full_pred zip s3_tageTarget_dup)
    yield
    fp.jalr_target := s3_tageTarget

  resp_meta.pred_cycle.map(_:= GTimer())
  // TODO: adjust for ITTAGE
  // Create a mask fo tables which did not hit our query, and also contain useless entries
  // and also uses a longer history than the provider
  val s2_allocatableSlots = VecInit(s2_resps.map(r => !r.valid && !r.bits.u)).asUInt &
    ~(LowerMask(UIntToOH(s2_provider), ITTageNTables) & Fill(ITTageNTables, s2_provided.asUInt))
  val s2_allocLFSR   = random.LFSR(width = 15)(ITTageNTables - 1, 0)
  val s2_firstEntry  = PriorityEncoder(s2_allocatableSlots)
  val s2_maskedEntry = PriorityEncoder(s2_allocatableSlots & s2_allocLFSR)
  val s2_allocEntry  = Mux(s2_allocatableSlots(s2_maskedEntry), s2_maskedEntry, s2_firstEntry)

  // Update in loop
  u_allocate.valid       := s2_allocatableSlots =/= 0.U
  u_allocate.bits        := s2_allocEntry
  val updateRealTarget = delay_full_target
  when (u_req_valid_reg) {
    when (u_provided) {
      val provider = u_provider
      XSDebug(true.B, p"update provider $provider, pred cycle ${updateMeta.pred_cycle.getOrElse(0.U)}\n")
      val altProvider = u_altProvider
      val usedAltpred = u_altProvided && u_providerCtr === 0.U
      when (usedAltpred && delay_updateMisPred) { // update altpred if used as pred
        XSDebug(true.B, p"update altprovider $altProvider, pred cycle ${updateMeta.pred_cycle.getOrElse(0.U)}\n")

        updateMask(altProvider)    := true.B
        updateUMask(altProvider)   := false.B
        updateCorrect(altProvider) := false.B
        updateOldCtr(altProvider)  := u_altProviderCtr
        updateAlloc(altProvider)   := false.B
        updateTarget(altProvider)  := updateRealTarget
        updateOldTarget(altProvider) := u_altProviderTarget
      }


      updateMask(provider)   := true.B
      updateUMask(provider)  := true.B

      updateU(provider) := Mux(!u_altDiffers, u_providerU, !delay_updateMisPred)
      updateCorrect(provider)  := u_providerTarget === updateRealTarget
      updateTarget(provider) := updateRealTarget
      updateOldTarget(provider) := u_providerTarget
      updateOldCtr(provider) := u_providerCtr
      updateAlloc(provider)  := false.B
    }
  }

  // if mispredicted and not the case that
  // provider offered correct target but used altpred due to unconfident
  val providerCorrect = u_provided && u_providerTarget === updateRealTarget
  val providerUnconf = u_providerCtr === 0.U
  when (u_req_valid_reg && delay_updateMisPred && !(providerCorrect && providerUnconf)) {
    val allocate = u_allocate
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

  for (i <- 0 until ITTageNTables) {
    tables(i).io.update.valid := RegNext(updateMask(i), init = false.B)
    tables(i).io.update.bits.reset_u := RegNext(updateResetU, init = false.B)
    tables(i).io.update.bits.correct := RegEnable(updateCorrect(i), updateMask(i))
    tables(i).io.update.bits.target := RegEnable(updateTarget(i), updateMask(i))
    tables(i).io.update.bits.old_target := RegEnable(updateOldTarget(i), updateMask(i))
    tables(i).io.update.bits.alloc := RegEnable(updateAlloc(i), updateMask(i))
    tables(i).io.update.bits.oldCtr := RegEnable(updateOldCtr(i), updateMask(i))

    tables(i).io.update.bits.uValid := RegEnable(updateUMask(i), false.B, updateMask(i))
    tables(i).io.update.bits.u := RegEnable(updateU(i), updateMask(i))
    tables(i).io.update.bits.pc := RegEnable(delay_u_pc, updateMask(i))
    // use fetch pc instead of instruction pc
    tables(i).io.update.bits.folded_hist := RegEnable(delay_ufolded_hist, updateMask(i))
  }


  // all should be ready for req
  io.s1_ready := tables.map(_.io.req.ready).reduce(_&&_)
  //if ittage table has conflict need block ftq update req
  io.update.ready := tables.map(_.io.update.ready).reduce(_&&_) && !u_req_stall_ftq
  XSPerfAccumulate("ittage_update_not_ready", io.update.ready)
  val ittage_updateMask_and_read_conflict = RegNext(updateMask.reduce(_||_)) && io.s1_fire(3) && s1_isIndirect
  XSPerfAccumulate("ittage_updateMask_and_read_conflict", ittage_updateMask_and_read_conflict)

  // Debug and perf info
  XSPerfAccumulate("ittage_reset_u", updateResetU)
  XSPerfAccumulate("ittage_used", io.s1_fire(0) && s1_isIndirect)
  XSPerfAccumulate("ittage_closed_due_to_uftb_info", io.s1_fire(0) && !s1_isIndirect)

  def pred_perf(name: String, cond: Bool)   = XSPerfAccumulate(s"${name}_at_pred", cond && io.s2_fire(3))
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

  // XSPerfAccumulate("ittage_provider_right")
  XSPerfAccumulate("updated", updateValid)

  if (debug) {
    val s2_resps_regs = RegEnable(s2_resps, io.s2_fire(3))
    XSDebug("req: v=%d, pc=0x%x\n", io.s0_fire(3), s0_pc_dup(3))
    XSDebug("s1_fire:%d, resp: pc=%x\n", io.s1_fire(3), debug_pc_s1)
    XSDebug("s2_fireOnLastCycle: resp: pc=%x, target=%x, hit=%b\n",
      debug_pc_s2, io.out.s2.getTarget(3), s2_provided)
    for (i <- 0 until ITTageNTables) {
      XSDebug("TageTable(%d): valids:%b, resp_ctrs:%b, resp_us:%b, target:%x\n",
        i.U, VecInit(s2_resps_regs(i).valid).asUInt, s2_resps_regs(i).bits.ctr,
        s2_resps_regs(i).bits.u, s2_resps_regs(i).bits.target)
    }
  }
  XSDebug(updateValid, p"pc: ${Hexadecimal(update.pc)}, target: ${Hexadecimal(update.full_target)}\n")
  XSDebug(updateValid, p"correct(${!delay_updateMisPred})\n")

  generatePerfEvent()
}
