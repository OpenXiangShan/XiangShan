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
import firrtl.passes.wiring.Wiring

trait ITTageParams extends HasXSParameter with HasBPUParameter {

  val ITTageNTables = ITTageTableInfos.size // Number of tage tables
  val UBitPeriod = 2048
  val ITTageCtrBits = 2
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
  val phist = UInt(PathHistoryLength.W)
}

class ITTageResp(implicit p: Parameters) extends ITTageBundle {
  val ctr = UInt(ITTageCtrBits.W)
  val u = UInt(2.W)
  val target = UInt(VAddrBits.W)
}

class ITTageUpdate(implicit p: Parameters) extends ITTageBundle {
  val pc = UInt(VAddrBits.W)
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
  val phist = UInt(PathHistoryLength.W)
  // update tag and ctr
  val valid = Bool()
  val correct = Bool()
  val alloc = Bool()
  val oldCtr = UInt(ITTageCtrBits.W)
  // update u
  val uValid = Bool()
  val u = UInt(2.W)
  // target
  val target = UInt(VAddrBits.W)
  val old_target = UInt(VAddrBits.W)
}

// reuse TAGE Implementation

class ITTageMeta(implicit p: Parameters) extends XSBundle with ITTageParams{
  val provider = ValidUndirectioned(UInt(log2Ceil(ITTageNTables).W))
  val altProvider = ValidUndirectioned(UInt(log2Ceil(ITTageNTables).W))
  val altDiffers = Bool()
  val providerU = UInt(2.W)
  val providerCtr = UInt(ITTageCtrBits.W)
  val altProviderCtr = UInt(ITTageCtrBits.W)
  val allocate = ValidUndirectioned(UInt(log2Ceil(ITTageNTables).W))
  val taken = Bool()
  val providerTarget = UInt(VAddrBits.W)
  val altProviderTarget = UInt(VAddrBits.W)
  // val scMeta = new SCMeta(EnableSC)
  // TODO: check if we need target info here
  val pred_cycle = UInt(64.W) // TODO: Use Option
  
  override def toPrintable = {
    p"pvdr(v:${provider.valid} num:${provider.bits} ctr:$providerCtr u:$providerU tar:${Hexadecimal(providerTarget)}), " +
    p"altpvdr(v:${altProvider.valid} num:${altProvider.bits}, ctr:$altProviderCtr, tar:${Hexadecimal(altProviderTarget)}), " +
    p"altdiff:$altDiffers, alloc(v:${allocate.valid} num:${allocate.bits}), taken:$taken, cycle:$pred_cycle"
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
    val req = Input(Valid(new ITTageReq))
    val resp = Output(Valid(new ITTageResp))
    val update = Input(new ITTageUpdate)
  })

  // override val debug = true
  // bypass entries for tage update
  val wrBypassEntries = 4
  val phistLen = if (PathHistoryLength > histLen) histLen else PathHistoryLength

  // def compute_tag_and_hash(unhashed_idx: UInt, hist: UInt, phist: UInt) = {
  //   val idx_history = compute_folded_ghist(hist, log2Ceil(nRows))
  //   // val idx = (unhashed_idx ^ (unhashed_idx >> (log2Ceil(nRows)-tableIdx+1)) ^ idx_history ^ idx_phist)(log2Ceil(nRows) - 1, 0)
  //   val idx = (unhashed_idx ^ idx_history)(log2Ceil(nRows) - 1, 0)
  //   val tag_history = compute_folded_ghist(hist, tagLen)
  //   val alt_tag_history = compute_folded_ghist(hist, tagLen-1)
  //   // Use another part of pc to make tags
  //   val tag = (
  //     if (tagLen > 1)
  //       ((unhashed_idx >> log2Ceil(nRows)) ^ tag_history ^ (alt_tag_history << 1)) (tagLen - 1, 0)
  //     else 0.U
  //   )
  //   (idx, tag)
  // }
  
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
  }

  // Why need add instOffsetBits?
  val ittageEntrySz = 1 + tagLen + ITTageCtrBits + VAddrBits

  // pc is start address of basic block, most 2 branch inst in block
  // def getUnhashedIdx(pc: UInt) = pc >> (instOffsetBits+log2Ceil(TageBanks))
  def getUnhashedIdx(pc: UInt): UInt = pc >> instOffsetBits

  val s0_pc = io.req.bits.pc
  val s0_unhashed_idx = getUnhashedIdx(io.req.bits.pc)

  // val (s0_idx, s0_tag) = compute_tag_and_hash(s0_unhashed_idx, io.req.bits.hist, io.req.bits.phist)
  val (s0_idx, s0_tag) = compute_tag_and_hash(s0_unhashed_idx, io.req.bits.folded_hist)
  val (s1_idx, s1_tag) = (RegEnable(s0_idx, io.req.valid), RegEnable(s0_tag, io.req.valid))

  val hi_us   = Module(new Folded1WDataModuleTemplate(Bool(), nRows, numRead=1, isSync=true, width=8))
  val lo_us   = Module(new Folded1WDataModuleTemplate(Bool(), nRows, numRead=1, isSync=true, width=8))
  val table  = Module(new SRAMTemplate(new ITTageEntry, set=nRows, way=1, shouldReset=true, holdRead=true, singlePort=false))
  //val hi_us = Module(new SRAMTemplate(UInt(2.W), set=nRows, way=ITTageBanks, shouldReset=true, holdRead=true, singlePort=false))
  //val lo_us = Module(new SRAMTemplate(UInt(2.W), set=nRows, way=ITTageBanks, shouldReset=true, holdRead=true, singlePort=false))
  //val table = Module(new SRAMTemplate(new ITTageEntry, set=nRows, way=ITTageBanks, shouldReset=true, holdRead=true, singlePort=false))

  table.io.r.req.valid := io.req.valid
  //hi_us.io.r.req.valid := io.req.valid
  //lo_us.io.r.req.valid := io.req.valid
  table.io.r.req.bits.setIdx := s0_idx
  hi_us.io.raddr(0) := s0_idx
  lo_us.io.raddr(0) := s0_idx

  val s1_hi_us_r = hi_us.io.rdata(0) //.resp.data
  val s1_lo_us_r = lo_us.io.rdata(0) //.resp.data
  val s1_table_r = table.io.r.resp.data(0)


  val s1_req_rhit = s1_table_r.valid && s1_table_r.tag === s1_tag

  io.resp.valid := (if (tagLen != 0) s1_req_rhit else true.B) // && s1_mask(b)
  io.resp.bits.ctr := s1_table_r.ctr
  io.resp.bits.u := Cat(s1_hi_us_r,s1_lo_us_r)
  io.resp.bits.target := s1_table_r.target


  // uBitPeriod = 2048, nRows = 128
  val clear_u_ctr = RegInit(0.U((log2Ceil(uBitPeriod) + log2Ceil(nRows) + 1).W))
  clear_u_ctr := clear_u_ctr + 1.U

  val doing_clear_u = clear_u_ctr(log2Ceil(uBitPeriod)-1,0) === 0.U
  val doing_clear_u_hi = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 1.U
  val doing_clear_u_lo = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 0.U
  val clear_u_idx = clear_u_ctr >> log2Ceil(uBitPeriod)

  // Use fetchpc to compute hash
  // val (update_idx, update_tag) = compute_tag_and_hash(getUnhashedIdx(io.update.pc), io.update.hist, io.update.phist)
  val (update_idx, update_tag) = compute_tag_and_hash(getUnhashedIdx(io.update.pc), io.update.folded_hist)
  val update_target = io.update.target

  val update_wdata = Wire(new ITTageEntry)

  table.io.w.apply(
    valid = io.update.valid,
    data = update_wdata,
    setIdx = update_idx,
    waymask = io.update.valid
  )

  val update_hi_wdata = Wire(Bool())
  /*
  hi_us(0).io.w.apply(
    valid = io.update.uMask.asUInt.orR || doing_clear_u_hi,
    data = Mux(doing_clear_u_hi, 0.U.asTypeOf(Vec(ITTageBanks, UInt(2.W))), update_hi_wdata),
    setIdx = Mux(doing_clear_u_hi, clear_u_idx, update_idx),
    waymask = Mux(doing_clear_u_hi, Fill(ITTageBanks, "b1".U), io.update.uMask.asUInt)
  )
   */
  hi_us.io.wen := io.update.uValid || doing_clear_u_hi
  hi_us.io.wdata := Mux(doing_clear_u_hi, false.B, update_hi_wdata)
  hi_us.io.waddr := Mux(doing_clear_u_hi, clear_u_idx, update_idx)

  val update_lo_wdata = Wire(Bool())
  /*
  lo_us(0).io.w.apply(
    valid = io.update.uMask.asUInt.orR || doing_clear_u_lo,
    data = Mux(doing_clear_u_lo, 0.U.asTypeOf(Vec(ITTageBanks, UInt(2.W))), update_lo_wdata),
    setIdx = Mux(doing_clear_u_lo, clear_u_idx, update_idx),
    waymask = Mux(doing_clear_u_lo, Fill(ITTageBanks, "b1".U), io.update.uMask.asUInt)
  )
   */
  lo_us.io.wen := io.update.uValid || doing_clear_u_lo
  lo_us.io.wdata := Mux(doing_clear_u_lo, false.B, update_lo_wdata)
  lo_us.io.waddr := Mux(doing_clear_u_lo, clear_u_idx, update_idx)

  val wrbypass_tags    = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(tagLen.W))))
  val wrbypass_idxs    = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(log2Ceil(nRows).W))))
  val wrbypass_ctrs    = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(ITTageCtrBits.W))))
  val wrbypass_enq_idx = RegInit(0.U(log2Ceil(wrBypassEntries).W))


  val wrbypass_hits    = VecInit((0 until wrBypassEntries) map { i =>
    wrbypass_tags(i) === update_tag &&
      wrbypass_idxs(i) === update_idx
  })


  val wrbypass_hit      = wrbypass_hits.reduce(_||_)
  // val wrbypass_rhit     = wrbypass_rhits.reduce(_||_)
  val wrbypass_hit_idx  = ParallelPriorityEncoder(wrbypass_hits)
  // val wrbypass_rhit_idx = PriorityEncoder(wrbypass_rhits)

  // val wrbypass_rctr_hits = VecInit((0 until TageBanks).map( b => wrbypass_ctr_valids(wrbypass_rhit_idx)(b)))

  // val rhit_ctrs = RegEnable(wrbypass_ctrs(wrbypass_rhit_idx), wrbypass_rhit)

  // when (RegNext(wrbypass_rhit)) {
  //   for (b <- 0 until TageBanks) {
  //     when (RegNext(wrbypass_rctr_hits(b.U + baseBank))) {
  //       io.resp(b).bits.ctr := rhit_ctrs(s2_bankIdxInOrder(b))
  //     }
  //   }
  // }


  val old_ctr = Mux(wrbypass_hit, wrbypass_ctrs(wrbypass_hit_idx), io.update.oldCtr)
  update_wdata.ctr   := Mux(io.update.alloc, 2.U, inc_ctr(old_ctr, io.update.correct))
  update_wdata.valid := true.B
  update_wdata.tag   := update_tag
  // only when ctr is null
  update_wdata.target := Mux(ctr_null(old_ctr), update_target, io.update.old_target)

  update_hi_wdata    := io.update.u(1)
  update_lo_wdata    := io.update.u(0)

  when (io.update.valid) {
    when (wrbypass_hit) {
      wrbypass_ctrs(wrbypass_hit_idx) := update_wdata.ctr
    } .otherwise {
      wrbypass_ctrs(wrbypass_enq_idx) := update_wdata.ctr
    }
  }

  when (io.update.valid && !wrbypass_hit) {
    wrbypass_tags(wrbypass_enq_idx) := update_tag
    wrbypass_idxs(wrbypass_enq_idx) := update_idx
    wrbypass_enq_idx := (wrbypass_enq_idx + 1.U)(log2Ceil(wrBypassEntries)-1,0)
  }

  XSPerfAccumulate("ittage_table_wrbypass_hit", io.update.valid && wrbypass_hit)
  XSPerfAccumulate("ittage_table_wrbypass_enq", io.update.valid && !wrbypass_hit)
  XSPerfAccumulate("ittage_table_hits", io.resp.valid)

  if (BPUDebug && debug) {
    val u = io.update
    val idx = s0_idx
    val tag = s0_tag
    XSDebug(io.req.valid,
      p"ITTageTableReq: pc=0x${Hexadecimal(io.req.bits.pc)}, " +
      p"idx=$idx, tag=$tag\n")
    XSDebug(RegNext(io.req.valid) && s1_req_rhit,
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
    val hitCtr = wrbypass_ctrs(wrbypass_hit_idx)
    XSDebug(wrbypass_hit && io.update.valid,
      p"wrbypass hit wridx:$wrbypass_hit_idx, idx:$update_idx, tag: $update_tag, " +
        p"ctr:$hitCtr, newCtr:${update_wdata.ctr}\n")

    XSDebug(RegNext(io.req.valid) && !s1_req_rhit, "TageTableResp: no hits!\n")

    // when (wrbypass_rhit && wrbypass_ctr_valids(wrbypass_rhit_idx).reduce(_||_)) {
    //   for (b <- 0 until TageBanks) {
    //     XSDebug(wrbypass_ctr_valids(wrbypass_rhit_idx)(b),
    //       "wrbypass rhits, wridx:%d, tag:%x, idx:%d, hitctr:%d, bank:%d\n",
    //       wrbypass_rhit_idx, tag, idx, wrbypass_ctrs(wrbypass_rhit_idx)(b), b.U)
    //   }
    // }

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
  io.s3_ready := true.B
}
// TODO: check target related logics
@chiselName
class ITTage(implicit p: Parameters) extends BaseITTage {
  override val meta_size = 0.U.asTypeOf(new ITTageMeta).getWidth

  val tables = ITTageTableInfos.zipWithIndex.map {
    case ((nRows, histLen, tagLen), i) =>
      // val t = if(EnableBPD) Module(new TageTable(nRows, histLen, tagLen, UBitPeriod)) else Module(new FakeTageTable)
      val t = Module(new ITTageTable(nRows, histLen, tagLen, UBitPeriod, i))
      // t.io.req.valid := io.pc.valid
      // t.io.req.bits.pc := io.pc.bits
      // t.io.req.bits.hist := io.hist
      // t.io.req.bits.mask := io.inMask

      t.io.req.valid := io.s0_fire
      t.io.req.bits.pc := s0_pc
      t.io.req.bits.folded_hist := io.in.bits.folded_hist
      t.io.req.bits.phist := io.in.bits.phist
      t
  }
  override def getFoldedHistoryInfo = Some(tables.map(_.getFoldedHistoryInfo).reduce(_++_))


  val useAltOnNa = RegInit((1 << (UAONA_bits-1)).U(UAONA_bits.W))

  // Keep the table responses to process in s2
  // val if4_resps = RegEnable(VecInit(tables.map(t => t.io.resp)), enable=s2_fire)
  // val if4_scResps = RegEnable(VecInit(scTables.map(t => t.io.resp)), enable=s2_fire)

  val s1_resps = VecInit(tables.map(t => t.io.resp))
  val base_table_resp = s1_resps(0)

  val s1_bim = io.in.bits.resp_in(0).s1.preds
  val s2_bim = RegEnable(s1_bim, enable=io.s1_fire)

  val debug_pc_s1 = RegEnable(s0_pc, enable=io.s0_fire)
  val debug_pc_s2 = RegEnable(debug_pc_s1, enable=io.s1_fire)
  val debug_pc_s3 = RegEnable(debug_pc_s2, enable=io.s2_fire)

  val s1_tageTaken         = Wire(Bool())
  val s1_tageTarget        = Wire(UInt(VAddrBits.W))
  val s1_providerTarget    = Wire(UInt(VAddrBits.W))
  val s1_altProviderTarget = Wire(UInt(VAddrBits.W))
  val s1_provided          = Wire(Bool())
  val s1_provider          = Wire(UInt(log2Ceil(ITTageNTables).W))
  val s1_altProvided       = Wire(Bool())
  val s1_altProvider       = Wire(UInt(log2Ceil(ITTageNTables).W))
  val s1_finalAltPred      = Wire(Bool())
  val s1_providerU         = Wire(UInt(2.W))
  val s1_providerCtr       = Wire(UInt(ITTageCtrBits.W))
  val s1_altProviderCtr    = Wire(UInt(ITTageCtrBits.W))

  val s2_tageTaken         = RegEnable(s1_tageTaken, io.s1_fire)
  val s2_tageTarget        = RegEnable(s1_tageTarget, io.s1_fire)
  val s2_providerTarget    = RegEnable(s1_providerTarget, io.s1_fire)
  val s2_altProviderTarget = RegEnable(s1_altProviderTarget, io.s1_fire)
  val s2_provided          = RegEnable(s1_provided, io.s1_fire)
  val s2_provider          = RegEnable(s1_provider, io.s1_fire)
  val s2_altProvided       = RegEnable(s1_altProvided, io.s1_fire)
  val s2_altProvider       = RegEnable(s1_altProvider, io.s1_fire)
  val s2_finalAltPred      = RegEnable(s1_finalAltPred, io.s1_fire)
  val s2_providerU         = RegEnable(s1_providerU, io.s1_fire)
  val s2_providerCtr       = RegEnable(s1_providerCtr, io.s1_fire)
  val s2_altProviderCtr    = RegEnable(s1_altProviderCtr, io.s1_fire)

  // val updateBank = u.pc(log2Ceil(TageBanks)+instOffsetBits-1, instOffsetBits)
  val resp_meta = WireInit(0.U.asTypeOf(new ITTageMeta))

  io.out.resp := io.in.bits.resp_in(0)
  io.out.s3_meta := RegEnable(resp_meta.asUInt, io.s2_fire)

  val ftb_hit = io.in.bits.resp_in(0).s2.preds.hit
  val ftb_entry = io.in.bits.resp_in(0).s2.ftb_entry
  val resp_s2 = io.out.resp.s2

  // Update logic
  val u_valid = io.update.valid
  val update = io.update.bits
  val updateValid =
    update.ftb_entry.isJalr && u_valid && update.ftb_entry.jmpValid &&
    !(update.real_br_taken_mask().reduce(_||_))
  val updatePhist = update.phist
  val updateFhist = update.folded_hist

  // meta is splited by composer
  val updateMeta = update.meta.asTypeOf(new ITTageMeta)

  val updateMask      = WireInit(0.U.asTypeOf(Vec(ITTageNTables, Bool())))
  val updateUMask     = WireInit(0.U.asTypeOf(Vec(ITTageNTables, Bool())))
  val updateCorrect   = Wire(Vec(ITTageNTables, Bool()))
  val updateTarget    = Wire(Vec(ITTageNTables, UInt(VAddrBits.W)))
  val updateOldTarget = Wire(Vec(ITTageNTables, UInt(VAddrBits.W)))
  val updateAlloc     = Wire(Vec(ITTageNTables, Bool()))
  val updateOldCtr    = Wire(Vec(ITTageNTables, UInt(ITTageCtrBits.W)))
  val updateU         = Wire(Vec(ITTageNTables, UInt(2.W)))
  updateCorrect   := DontCare
  updateTarget  := DontCare
  updateOldTarget  := DontCare
  updateAlloc   := DontCare
  updateOldCtr  := DontCare
  updateU       := DontCare

  // val updateTageMisPreds = VecInit((0 until numBr).map(i => updateMetas(i).taken =/= u.takens(i)))
  val updateMisPred = update.mispred_mask(numBr) // the last one indicates jmp results
  // access tag tables and output meta info
  val basePred             = base_table_resp.bits.ctr =/= 0.U
  val baseTarget           = base_table_resp.bits.target
  s1_tageTaken         := basePred // TODO: reintroduce BIM
  s1_tageTarget        := baseTarget
  s1_finalAltPred      := basePred
  val s1_finalAltTarget    = WireInit(baseTarget)
  var s1_temp_altPred      = basePred
  var s1_temp_altTarget    = baseTarget
  var s1_temp_provided     = false.B
  var s1_temp_provider     = 0.U
  var s1_temp_alt_provided = false.B
  var s1_temp_alt_provider = 0.U

  for (i <- 1 until ITTageNTables) { // skip base table
    val hit = s1_resps(i).valid
    val ctr = s1_resps(i).bits.ctr
    val target = s1_resps(i).bits.target
    when (hit) {
      s1_tageTaken := Mux(ctr === 0.U, s1_temp_altPred, true.B) // Use altpred on weak taken
      s1_tageTarget := Mux(ctr === 0.U, s1_temp_altTarget, target)
      s1_finalAltPred := s1_temp_altPred
      s1_finalAltTarget := s1_temp_altTarget
    }
    s1_temp_alt_provided = (s1_temp_provided && hit || s1_temp_alt_provided) // assign before s1_provided
    s1_temp_provided = s1_temp_provided || hit          // Once hit then provide
    s1_temp_alt_provider = Mux(hit, s1_temp_provider, s1_temp_alt_provider) // assign before s1 provider
    s1_temp_provider = Mux(hit, i.U, s1_temp_provider)  // Use the last hit as provider
    s1_temp_altPred = Mux(hit, true.B, s1_temp_altPred) // Save current pred as potential altpred
    s1_temp_altTarget = Mux(hit, target, s1_temp_altTarget)
  }
  s1_provided       := s1_temp_provided
  s1_provider       := s1_temp_provider
  s1_altProvided    := s1_temp_alt_provided
  s1_altProvider    := s1_temp_alt_provider
  s1_providerU      := s1_resps(s1_temp_provider).bits.u
  s1_providerCtr    := s1_resps(s1_temp_provider).bits.ctr
  s1_altProviderCtr := s1_resps(s1_temp_alt_provider).bits.ctr
  s1_providerTarget := s1_resps(s1_temp_provider).bits.target
  s1_altProviderTarget := s1_finalAltTarget

  XSDebug(io.s2_fire, p"hit_taken_jalr:")
  when(io.s2_fire && io.in.bits.resp_in(0).s2.hit_taken_on_jalr && s2_tageTaken) {
    // FIXME: should use s1 globally
    io.out.resp.s2.preds.targets.last := s2_tageTarget
  }

  resp_meta.provider.valid    := s2_provided
  resp_meta.provider.bits     := s2_provider
  resp_meta.altProvider.valid := s2_altProvided
  resp_meta.altProvider.bits  := s2_altProvider
  resp_meta.altDiffers        := s2_finalAltPred =/= s2_tageTaken
  resp_meta.providerU         := s2_providerU
  resp_meta.providerCtr       := s2_providerCtr
  resp_meta.altProviderCtr    := s2_altProviderCtr
  resp_meta.taken             := s2_tageTaken
  resp_meta.providerTarget    := s2_providerTarget
  resp_meta.altProviderTarget := s2_altProviderTarget
  resp_meta.pred_cycle        := GTimer()
  // TODO: adjust for ITTAGE
  // Create a mask fo tables which did not hit our query, and also contain useless entries
  // and also uses a longer history than the provider
  val allocatableSlots = RegEnable(VecInit(s1_resps.map(r => !r.valid && r.bits.u === 0.U)).asUInt &
    ~(LowerMask(UIntToOH(s1_provider), ITTageNTables) & Fill(ITTageNTables, s1_provided.asUInt)), io.s1_fire
  )
  val allocLFSR   = LFSR64()(ITTageNTables - 1, 0)
  val firstEntry  = PriorityEncoder(allocatableSlots)
  val maskedEntry = PriorityEncoder(allocatableSlots & allocLFSR)
  val allocEntry  = Mux(allocatableSlots(maskedEntry), maskedEntry, firstEntry)
  resp_meta.allocate.valid := allocatableSlots =/= 0.U
  resp_meta.allocate.bits  := allocEntry

  // Update in loop
  val updateTaken = updateValid && update.ftb_entry.jmpValid
  val updateRealTarget = update.full_target
  when (updateValid) {
    when (updateMeta.provider.valid) {
      val provider = updateMeta.provider.bits
      XSDebug(true.B, p"update provider $provider, pred cycle ${updateMeta.pred_cycle}\n")
      val altProvider = updateMeta.altProvider.bits
      val usedAltpred = updateMeta.altProvider.valid && updateMeta.providerCtr === 0.U
      when (usedAltpred && updateMisPred) { // update altpred if used as pred
        XSDebug(true.B, p"update altprovider $altProvider, pred cycle ${updateMeta.pred_cycle}\n")

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

      updateU(provider) := Mux(!updateMeta.altDiffers, updateMeta.providerU,
        Mux(updateMisPred, Mux(updateMeta.providerU === 0.U, 0.U, updateMeta.providerU - 1.U),
          Mux(updateMeta.providerU === 3.U, 3.U, updateMeta.providerU + 1.U))
      )
      updateCorrect(provider)  := updateMeta.providerTarget === updateRealTarget
      updateTarget(provider) := updateRealTarget
      updateOldTarget(provider) := updateMeta.providerTarget
      updateOldCtr(provider) := updateMeta.providerCtr
      updateAlloc(provider)  := false.B
    }
  }

  // update base table if used base table to predict
  when (updateValid) {
    val useBaseTableAsAltPred = updateMeta.provider.valid && !updateMeta.altProvider.valid && updateMeta.providerCtr === 0.U
    val usedBaseTable = !updateMeta.provider.valid || useBaseTableAsAltPred
    XSDebug(usedBaseTable, p"update base table, pred cycle ${updateMeta.pred_cycle}\n")
    updateMask(0) := usedBaseTable
    updateCorrect(0) := !updateMisPred
    updateTarget(0) := updateRealTarget
    updateOldTarget(0) := Mux(useBaseTableAsAltPred,
      updateMeta.altProviderTarget, updateMeta.providerTarget)
    updateOldCtr(0) := Mux(useBaseTableAsAltPred,
      updateMeta.altProviderCtr, updateMeta.providerCtr)
    updateAlloc(0) := false.B
    updateUMask(0) := false.B
  }

  // if mispredicted and not the case that
  // provider offered correct target but used altpred due to unconfident
  val providerCorrect = updateMeta.provider.valid && updateMeta.providerTarget === updateRealTarget
  val providerUnconf = updateMeta.providerCtr === 0.U
  when (updateValid && updateMisPred && !(providerCorrect && providerUnconf)) {
    val allocate = updateMeta.allocate
    when (allocate.valid) {
      XSDebug(true.B, p"allocate new table entry, pred cycle ${updateMeta.pred_cycle}\n")
      updateMask(allocate.bits)  := true.B
      updateCorrect(allocate.bits) := true.B // useless for alloc
      updateTarget(allocate.bits) := updateRealTarget
      updateAlloc(allocate.bits) := true.B
      updateUMask(allocate.bits) := true.B
      updateU(allocate.bits) := 0.U
    }.otherwise {

      val provider = updateMeta.provider
      val decrMask = Mux(provider.valid, ~LowerMask(UIntToOH(provider.bits), ITTageNTables), 0.U(ITTageNTables.W))
      for (i <- 0 until ITTageNTables) {
        when (decrMask(i)) {
          updateUMask(i) := true.B
          updateU(i) := 0.U
        }
      }
    }
  }

  /*
  val fallThruAddr = getFallThroughAddr(s2_pc, ftb_entry.carry, ftb_entry.pftAddr)
  when(ftb_hit) {
    io.out.resp.s2.preds.target := Mux(resp_s2.preds.is_jalr & ftb_entry.isJalr,
      resp_s2.preds.target,
      Mux(ftb_entry.jmpValid, ftb_entry.jmpTarget, fallThruAddr))
  }*/


  for (i <- 0 until ITTageNTables) {
    tables(i).io.update.valid := RegNext(updateMask(i))
    tables(i).io.update.correct := RegNext(updateCorrect(i))
    tables(i).io.update.target := RegNext(updateTarget(i))
    tables(i).io.update.old_target := RegNext(updateOldTarget(i))
    tables(i).io.update.alloc := RegNext(updateAlloc(i))
    tables(i).io.update.oldCtr := RegNext(updateOldCtr(i))

    tables(i).io.update.uValid := RegNext(updateUMask(i))
    tables(i).io.update.u := RegNext(updateU(i))
    tables(i).io.update.pc := RegNext(update.pc)
    // use fetch pc instead of instruction pc
    tables(i).io.update.phist := RegNext(updatePhist)
    tables(i).io.update.folded_hist := RegNext(updateFhist)
  }

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
    //     b.U, update.pc, 0.U, updateHist.predHist, update.preds.taken_mask(b), update.mispred_mask(b),
    //     0.U, m.provider.valid, m.provider.bits, m.altDiffers, m.providerU, m.providerCtr, m.allocate.valid, m.allocate.bits
    //   )
    // }
    val s2_resps = RegEnable(s1_resps, io.s1_fire)
    XSDebug("req: v=%d, pc=0x%x\n", io.s0_fire, s0_pc)
    XSDebug("s1_fire:%d, resp: pc=%x\n", io.s1_fire, debug_pc_s1)
    XSDebug("s2_fireOnLastCycle: resp: pc=%x, target=%x, hit=%b, taken=%b\n",
      debug_pc_s2, io.out.resp.s2.target, s2_provided, s2_tageTaken)
    for (i <- 0 until ITTageNTables) {
      XSDebug("TageTable(%d): valids:%b, resp_ctrs:%b, resp_us:%b, target:%x\n",
        i.U, VecInit(s2_resps(i).valid).asUInt, s2_resps(i).bits.ctr,
        s2_resps(i).bits.u, s2_resps(i).bits.target)
    }
  }
  XSDebug(updateValid, p"pc: ${Hexadecimal(update.pc)}, target: ${Hexadecimal(update.full_target)}\n")
  XSDebug(updateValid, updateMeta.toPrintable+p"\n")
  XSDebug(updateValid, p"correct(${!updateMisPred})\n")
}


// class Tage_SC(implicit p: Parameters) extends Tage with HasSC {}
