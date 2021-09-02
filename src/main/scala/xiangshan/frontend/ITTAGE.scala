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

trait ITTageParams extends HasXSParameter with HasBPUParameter {
  //                   Sets  Hist   Tag
  val TableInfo = Seq(( 128,    2,    7),
    ( 128,    4,    7),
    ( 128,    8,    8),
    ( 256,   16,    8))
  // ( 128*8,   32,    9),
  // ( 128*8,   64,    9))
  // (  64,   64,   11),
  // (  64,  101,   12),
  // (  64,  160,   12),
  // (  64,  254,   13),
  // (  32,  403,   14),
  // (  32,  640,   15))
  val ITTageNTables = TableInfo.size // Number of tage tables
  val UBitPeriod = 2048
  val ITTageBanks = 1 // numBr
  val ITTageCtrBits = 3

  val TotalBits = TableInfo.map {
    case (s, h, t) => {
      s * (1+t+ITTageCtrBits+VAddrBits) * ITTageBanks
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
  val hist = UInt(HistoryLength.W)
  val phist = UInt(PathHistoryLength.W)
  val mask = UInt(numBr.W)
}

class ITTageResp(implicit p: Parameters) extends ITTageBundle {
  val ctr = UInt(ITTageCtrBits.W)
  val u = UInt(2.W)
  val target = UInt(VAddrBits.W)
}

class ITTageUpdate(implicit p: Parameters) extends ITTageBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
  val phist = UInt(PathHistoryLength.W)
  // update tag and ctr
  val mask = Vec(ITTageBanks, Bool())
  val taken = Vec(ITTageBanks, Bool())
  val alloc = Vec(ITTageBanks, Bool())
  val oldCtr = Vec(ITTageBanks, UInt(ITTageCtrBits.W))
  // update u
  val uMask = Vec(ITTageBanks, Bool())
  val u = Vec(ITTageBanks, UInt(2.W))
  val target = Vec(ITTageBanks, UInt(VAddrBits.W))
}

// reuse TAGE Implementation

class ITTageMeta(implicit p: Parameters) extends XSBundle with ITTageParams{
  val provider = ValidUndirectioned(UInt(log2Ceil(ITTageNTables).W))
  val altDiffers = Bool()
  val providerU = UInt(2.W)
  val providerCtr = UInt(3.W)
  val allocate = ValidUndirectioned(UInt(log2Ceil(ITTageNTables).W))
  val taken = Bool()
  // val scMeta = new SCMeta(EnableSC)
  // TODO: check if we need target info here
  val pred_cycle = UInt(64.W) // TODO: Use Option
}


class FakeITTageTable()(implicit p: Parameters) extends ITTageModule {
  val io = IO(new Bundle() {
    val req = Input(Valid(new ITTageReq))
    val resp = Output(Vec(ITTageBanks, Valid(new ITTageResp)))
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
    val resp = Output(Vec(ITTageBanks, Valid(new ITTageResp)))
    val update = Input(new ITTageUpdate)
  })

  // override val debug = true
  // bypass entries for tage update
  val wrBypassEntries = 4
  val phistLen = if (PathHistoryLength > histLen) histLen else PathHistoryLength

  def compute_tag_and_hash(unhashed_idx: UInt, hist: UInt, phist: UInt) = {
    def F(phist: UInt, len: Int) = {
      val lenMask = Fill(len, 1.U(1.W))
      val rowMask = Fill(log2Ceil(nRows), 1.U(1.W))
      val masked = phist & lenMask
      val a1 = masked & rowMask
      val a2 = masked >> log2Ceil(nRows)
      val a3 = ((a2 << tableIdx) & rowMask) + (a2 >> (log2Ceil(nRows) - tableIdx))
      val a4 = a1 ^ a3
      val res = ((a3 << tableIdx) & rowMask) + (a3 >> (log2Ceil(nRows) - tableIdx))
      res
    }
    val idx_history = compute_folded_ghist(hist, log2Ceil(nRows))
    val idx_phist = F(phist, (if (PathHistoryLength > histLen) histLen else PathHistoryLength))
    // val idx = (unhashed_idx ^ (unhashed_idx >> (log2Ceil(nRows)-tableIdx+1)) ^ idx_history ^ idx_phist)(log2Ceil(nRows) - 1, 0)
    val idx = (unhashed_idx ^ idx_history)(log2Ceil(nRows) - 1, 0)
    val tag_history = compute_folded_ghist(hist, tagLen)
    val alt_tag_history = compute_folded_ghist(hist, tagLen-1)
    // Use another part of pc to make tags
    val tag = ((unhashed_idx >> log2Ceil(nRows)) ^ tag_history ^ (alt_tag_history << 1)) (tagLen - 1, 0)
    (idx, tag)
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

  val (s0_idx, s0_tag) = compute_tag_and_hash(s0_unhashed_idx, io.req.bits.hist, io.req.bits.phist)
  val (s1_idx, s1_tag) = (RegEnable(s0_idx, io.req.valid), RegEnable(s0_tag, io.req.valid))

  val hi_us   = Seq.fill(ITTageBanks)(Module(new Folded1WDataModuleTemplate(Bool(), nRows, numRead=1, isSync=true, width=8)))
  val lo_us   = Seq.fill(ITTageBanks)(Module(new Folded1WDataModuleTemplate(Bool(), nRows, numRead=1, isSync=true, width=8)))
  val tables  = Seq.fill(ITTageBanks)(Module(new SRAMTemplate(new ITTageEntry, set=nRows, way=1, shouldReset=true, holdRead=true, singlePort=false)))
  //val hi_us = Module(new SRAMTemplate(UInt(2.W), set=nRows, way=ITTageBanks, shouldReset=true, holdRead=true, singlePort=false))
  //val lo_us = Module(new SRAMTemplate(UInt(2.W), set=nRows, way=ITTageBanks, shouldReset=true, holdRead=true, singlePort=false))
  //val table = Module(new SRAMTemplate(new ITTageEntry, set=nRows, way=ITTageBanks, shouldReset=true, holdRead=true, singlePort=false))

  tables(0).io.r.req.valid := io.req.valid
  //hi_us.io.r.req.valid := io.req.valid
  //lo_us.io.r.req.valid := io.req.valid
  tables(0).io.r.req.bits.setIdx := s0_idx
  hi_us(0).io.raddr(0) := s0_idx
  lo_us(0).io.raddr(0) := s0_idx

  val s1_hi_us_r = hi_us(0).io.rdata(0) //.resp.data
  val s1_lo_us_r = lo_us(0).io.rdata(0) //.resp.data
  val s1_table_r = tables(0).io.r.resp.data

  val s0_mask = io.req.bits.mask
  val s1_mask = RegEnable(s0_mask, enable=io.req.valid)

  val s1_req_rhits = VecInit((0 until ITTageBanks).map(b => {
    s1_table_r(b).valid && s1_table_r(b).tag === s1_tag
  }))

  (0 until ITTageBanks).map(b => {
    io.resp(b).valid := s1_req_rhits(b) // && s1_mask(b)
    io.resp(b).bits.ctr := s1_table_r(b).ctr
    io.resp(b).bits.u := Cat(s1_hi_us_r(b),s1_lo_us_r(b))
    io.resp(b).bits.target := s1_table_r(b).target
  })


  // uBitPeriod = 2048, nRows = 128
  val clear_u_ctr = RegInit(0.U((log2Ceil(uBitPeriod) + log2Ceil(nRows) + 1).W))
  clear_u_ctr := clear_u_ctr + 1.U

  val doing_clear_u = clear_u_ctr(log2Ceil(uBitPeriod)-1,0) === 0.U
  val doing_clear_u_hi = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 1.U
  val doing_clear_u_lo = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 0.U
  val clear_u_idx = clear_u_ctr >> log2Ceil(uBitPeriod)

  // Use fetchpc to compute hash
  val (update_idx, update_tag) = compute_tag_and_hash(getUnhashedIdx(io.update.pc), io.update.hist, io.update.phist)
  val update_target = io.update.target

  val update_wdata = Wire(Vec(ITTageBanks, new ITTageEntry))

  tables(0).io.w.apply(
    valid = io.update.mask.asUInt.orR,
    data = update_wdata,
    setIdx = update_idx,
    waymask = io.update.mask.asUInt
  )

  val update_hi_wdata = Wire(Vec(ITTageBanks, UInt(2.W)))
  /*
  hi_us(0).io.w.apply(
    valid = io.update.uMask.asUInt.orR || doing_clear_u_hi,
    data = Mux(doing_clear_u_hi, 0.U.asTypeOf(Vec(ITTageBanks, UInt(2.W))), update_hi_wdata),
    setIdx = Mux(doing_clear_u_hi, clear_u_idx, update_idx),
    waymask = Mux(doing_clear_u_hi, Fill(ITTageBanks, "b1".U), io.update.uMask.asUInt)
  )
   */
  hi_us(0).io.wen := (io.update.uMask.asUInt.orR || doing_clear_u_hi) && Mux(doing_clear_u_hi, true.B, io.update.uMask.asUInt.orR())
  hi_us(0).io.wdata := Mux(doing_clear_u_hi, false.B, update_hi_wdata(0))
  hi_us(0).io.waddr := Mux(doing_clear_u_hi, clear_u_idx, update_idx)

  val update_lo_wdata = Wire(Vec(ITTageBanks, UInt(2.W)))
  /*
  lo_us(0).io.w.apply(
    valid = io.update.uMask.asUInt.orR || doing_clear_u_lo,
    data = Mux(doing_clear_u_lo, 0.U.asTypeOf(Vec(ITTageBanks, UInt(2.W))), update_lo_wdata),
    setIdx = Mux(doing_clear_u_lo, clear_u_idx, update_idx),
    waymask = Mux(doing_clear_u_lo, Fill(ITTageBanks, "b1".U), io.update.uMask.asUInt)
  )
   */
  lo_us(0).io.wen := (io.update.uMask.asUInt.orR || doing_clear_u_lo) && Mux(doing_clear_u_lo, true.B, io.update.uMask(0))
  lo_us(0).io.wdata := Mux(doing_clear_u_lo, false.B, update_lo_wdata(0))
  lo_us(0).io.waddr := Mux(doing_clear_u_lo, clear_u_idx, update_idx)

  val wrbypass_tags    = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(tagLen.W))))
  val wrbypass_idxs    = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(log2Ceil(nRows).W))))
  val wrbypass_targets = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, Vec(ITTageBanks, UInt(VAddrBits.W)))))
  val wrbypass_ctrs    = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, Vec(ITTageBanks, UInt(ITTageCtrBits.W)))))
  val wrbypass_ctr_valids = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, Vec(ITTageBanks, Bool()))))
  val wrbypass_enq_idx = RegInit(0.U(log2Ceil(wrBypassEntries).W))

  when (reset.asBool) { wrbypass_ctr_valids.foreach(_.foreach(_ := false.B))}

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


  val updateBank = PriorityEncoder(io.update.mask)

  for (w <- 0 until ITTageBanks) {
    update_wdata(w).ctr   := Mux(io.update.alloc(w),
      Mux(io.update.taken(w), 4.U,
                              3.U
      ),
      Mux(wrbypass_hit && wrbypass_ctr_valids(wrbypass_hit_idx)(w),
        inc_ctr(wrbypass_ctrs(wrbypass_hit_idx)(w), io.update.taken(w)),
        inc_ctr(io.update.oldCtr(w), io.update.taken(w))
      )
    )
    update_wdata(w).valid := true.B
    update_wdata(w).tag   := update_tag
    update_wdata(w).target := update_target(w)

    update_hi_wdata(w)    := io.update.u(w)(1)
    update_lo_wdata(w)    := io.update.u(w)(0)

    when (io.update.mask.reduce(_||_)) {
      when (wrbypass_hit) {
        when (io.update.mask(w)) {
          wrbypass_ctrs(wrbypass_hit_idx)(w) := update_wdata(w).ctr
          wrbypass_ctr_valids(wrbypass_hit_idx)(w) := true.B
          // wrbypass_targets(wrbypass_hit_idx)(w) := update_wdata(w).target
        }
      } .otherwise {
        // reset valid bit first
        wrbypass_ctr_valids(wrbypass_enq_idx)(w) := false.B
        when (io.update.mask(w)) {
          wrbypass_ctr_valids(wrbypass_enq_idx)(w) := true.B
          wrbypass_ctrs(wrbypass_enq_idx)(w) := update_wdata(w).ctr
          // wrbypass_targets(wrbypass_enq_idx)(w) := update_wdata(w).target
        }
      }
    }
  }

  when (io.update.mask.reduce(_||_) && !wrbypass_hit) {
    wrbypass_tags(wrbypass_enq_idx) := update_tag
    wrbypass_idxs(wrbypass_enq_idx) := update_idx
    wrbypass_targets(wrbypass_enq_idx) := update_target
    wrbypass_enq_idx := (wrbypass_enq_idx + 1.U)(log2Ceil(wrBypassEntries)-1,0)
  }

  XSPerfAccumulate("ittage_table_wrbypass_hit", io.update.mask.reduce(_||_) && wrbypass_hit)
  XSPerfAccumulate("ittage_table_wrbypass_enq", io.update.mask.reduce(_||_) && !wrbypass_hit)
  XSPerfAccumulate("ittage_table_hits", PopCount(VecInit(io.resp.map(_.valid))))

  if (BPUDebug && debug) {
    val u = io.update
    val b = PriorityEncoder(u.mask)
    val ub = PriorityEncoder(u.uMask)
    val idx = s0_idx
    val tag = s0_tag
    XSDebug(io.req.valid,
      p"tableReq: pc=0x${Hexadecimal(io.req.bits.pc)}, " +
        p"hist=${Hexadecimal(io.req.bits.hist)}, idx=$idx, " +
        p"tag=$tag, mask=${Binary(s0_mask)}\n")
    for (i <- 0 until ITTageBanks) {
      XSDebug(RegNext(io.req.valid && io.req.bits.mask(i)) && s1_req_rhits(i),
        p"TageTableResp[$i]: idx=$s1_idx, hit:${s1_req_rhits(i)}, " +
          p"ctr:${io.resp(i).bits.ctr}, u:${io.resp(i).bits.u}\n")
      XSDebug(io.update.mask(i),
        p"update Table bank $i: pc:${Hexadecimal(u.pc)}, hist:${Hexadecimal(u.hist)}, " +
          p"taken:${u.taken(i)}, alloc:${u.alloc(i)}, oldCtr:${u.oldCtr(i)}\n")
      XSDebug(io.update.mask(i),
        p"update Table bank $i: writing tag:${update_tag}, " +
          p"ctr: ${update_wdata(b).ctr} in idx $update_idx\n")
      val hitCtr = wrbypass_ctrs(wrbypass_hit_idx)(i)
      XSDebug(wrbypass_hit && wrbypass_ctr_valids(wrbypass_hit_idx)(i) && io.update.mask(i),
        p"bank $i wrbypass hit wridx:$wrbypass_hit_idx, idx:$update_idx, tag: $update_tag, " +
          p"ctr:$hitCtr, newCtr:${update_wdata(i).ctr}")
    }

    XSDebug(RegNext(io.req.valid) && !s1_req_rhits.reduce(_||_), "TageTableResp: no hits!\n")

    // when (wrbypass_rhit && wrbypass_ctr_valids(wrbypass_rhit_idx).reduce(_||_)) {
    //   for (b <- 0 until TageBanks) {
    //     XSDebug(wrbypass_ctr_valids(wrbypass_rhit_idx)(b),
    //       "wrbypass rhits, wridx:%d, tag:%x, idx:%d, hitctr:%d, bank:%d\n",
    //       wrbypass_rhit_idx, tag, idx, wrbypass_ctrs(wrbypass_rhit_idx)(b), b.U)
    //   }
    // }

    // ------------------------------Debug-------------------------------------
    val valids = Reg(Vec(ITTageBanks, Vec(nRows, Bool())))
    when (reset.asBool) { valids.foreach(b => b.foreach(r => r := false.B)) }
    (0 until ITTageBanks).map(b => { when (io.update.mask(b)) { valids(b)(update_idx) := true.B }})
    XSDebug("Table usage:------------------------\n")
    (0 until ITTageBanks).map(b => { XSDebug("Bank(%d): %d out of %d rows are valid\n", b.U, PopCount(valids(b)), nRows.U)})
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
  override val meta_size = 0.U.asTypeOf(Vec(ITTageBanks, new ITTageMeta)).getWidth

  val tables = TableInfo.zipWithIndex.map {
    case ((nRows, histLen, tagLen), i) =>
      // val t = if(EnableBPD) Module(new TageTable(nRows, histLen, tagLen, UBitPeriod)) else Module(new FakeTageTable)
      val t = Module(new ITTageTable(nRows, histLen, tagLen, UBitPeriod, i))
      // t.io.req.valid := io.pc.valid
      // t.io.req.bits.pc := io.pc.bits
      // t.io.req.bits.hist := io.hist
      // t.io.req.bits.mask := io.inMask

      t.io.req.valid := io.s0_fire
      t.io.req.bits.pc := s0_pc
      t.io.req.bits.hist := io.in.bits.ghist
      t.io.req.bits.phist := io.in.bits.phist
      t.io.req.bits.mask := VecInit(Seq.fill(numBr)(1.U(1.W))).asUInt()
      t
  }

  // Keep the table responses to process in s2
  // val if4_resps = RegEnable(VecInit(tables.map(t => t.io.resp)), enable=s2_fire)
  // val if4_scResps = RegEnable(VecInit(scTables.map(t => t.io.resp)), enable=s2_fire)

  val s1_resps = VecInit(tables.map(t => t.io.resp))

  val s1_bim = io.in.bits.resp_in(0).s1.preds
  val s2_bim = RegEnable(s1_bim, enable=io.s1_fire)

  val debug_pc_s0 = RegEnable(s0_pc, enable=io.s0_fire)
  val debug_pc_s1 = RegEnable(debug_pc_s0, enable=io.s0_fire)
  val debug_pc_s2 = RegEnable(debug_pc_s1, enable=io.s1_fire)

  val debug_hist_s0 = io.in.bits.ghist
  val debug_hist_s1 = RegEnable(debug_hist_s0, enable=io.s0_fire)
  val debug_hist_s2 = RegEnable(debug_hist_s1, enable=io.s1_fire)

  val s1_tageTakens    = Wire(Vec(ITTageBanks, Bool()))
  val s1_tageTargets   = Wire(Vec(ITTageBanks, UInt(VAddrBits.W)))
  val s1_provideds     = Wire(Vec(ITTageBanks, Bool()))
  val s1_providers     = Wire(Vec(ITTageBanks, UInt(log2Ceil(ITTageNTables).W)))
  val s1_finalAltPreds = Wire(Vec(ITTageBanks, Bool()))
  val s1_providerUs    = Wire(Vec(ITTageBanks, UInt(2.W)))
  val s1_providerCtrs  = Wire(Vec(ITTageBanks, UInt(ITTageCtrBits.W)))

  val s2_tageTakens    = RegEnable(s1_tageTakens, io.s1_fire)
  val s2_tageTargets   = RegEnable(s1_tageTargets, io.s1_fire)
  val s2_provideds     = RegEnable(s1_provideds, io.s1_fire)
  val s2_providers     = RegEnable(s1_providers, io.s1_fire)
  val s2_finalAltPreds = RegEnable(s1_finalAltPreds, io.s1_fire)
  val s2_providerUs    = RegEnable(s1_providerUs, io.s1_fire)
  val s2_providerCtrs  = RegEnable(s1_providerCtrs, io.s1_fire)

  // val updateBank = u.pc(log2Ceil(TageBanks)+instOffsetBits-1, instOffsetBits)
  val resp_meta = WireInit(0.U.asTypeOf(Vec(ITTageBanks, new ITTageMeta)))

  io.out.resp := io.in.bits.resp_in(0)
  io.out.s3_meta := resp_meta.asUInt

  val ftb_hit = io.in.bits.resp_in(0).s2.preds.hit
  val ftb_entry = io.in.bits.resp_in(0).s2.ftb_entry
  val resp_s2 = io.out.resp.s2

  // Update logic
  val u_valid = io.update.valid
  val update = io.update.bits
  val updateValids = VecInit(Seq.fill(ITTageBanks)(update.ftb_entry.isJalr && u_valid))
  val updateHist = update.ghist
  val updatePhist = update.phist

  // meta is splited by composer
  val updateMetas = update.meta.asTypeOf(Vec(ITTageBanks, new ITTageMeta))

  val updateMask    = WireInit(0.U.asTypeOf(Vec(ITTageNTables, Vec(ITTageBanks, Bool()))))
  val updateUMask   = WireInit(0.U.asTypeOf(Vec(ITTageNTables, Vec(ITTageBanks, Bool()))))
  val updateTaken   = Wire(Vec(ITTageNTables, Vec(ITTageBanks, Bool())))
  val updateTarget  = Wire(Vec(ITTageNTables, Vec(ITTageBanks, UInt(VAddrBits.W))))
  val updateAlloc   = Wire(Vec(ITTageNTables, Vec(ITTageBanks, Bool())))
  val updateOldCtr  = Wire(Vec(ITTageNTables, Vec(ITTageBanks, UInt(ITTageCtrBits.W))))
  val updateU       = Wire(Vec(ITTageNTables, Vec(ITTageBanks, UInt(2.W))))
  updateTaken   := DontCare
  updateTarget  := DontCare
  updateAlloc   := DontCare
  updateOldCtr  := DontCare
  updateU       := DontCare

  // val updateTageMisPreds = VecInit((0 until numBr).map(i => updateMetas(i).taken =/= u.takens(i)))
  val updateMisPreds = update.mispred_mask(numBr) // the last one indicates jmp results
  // access tag tables and output meta info
  for (w <- 0 until ITTageBanks) {
    val s1_tageTaken     = WireInit(0.U) // TODO: reintroduce BIM
    val s1_tageTarget    = WireInit(0.U.asTypeOf(UInt(VAddrBits.W)))
    var s1_altPred       = 0.U // TODO: reintroduce BIM
    var s1_altTarget     = 0.U.asTypeOf(UInt(VAddrBits.W))
    val s1_finalAltPred  = WireInit(0.U)
    val s1_finalAltTarget = WireInit(0.U.asTypeOf(UInt(VAddrBits.W)))
    var s1_provided      = false.B
    var s1_provider      = 0.U

    for (i <- 0 until ITTageNTables) {
      val hit = s1_resps(i)(w).valid
      val ctr = s1_resps(i)(w).bits.ctr
      val target = s1_resps(i)(w).bits.target
      when (hit) {
        s1_tageTaken := Mux(ctr < 4.U, s1_altPred, true.B) // Use altpred on weak taken
        s1_tageTarget := Mux(ctr < 4.U, s1_altTarget, target)
        s1_finalAltPred := s1_altPred
        s1_finalAltTarget := s1_altTarget
      }
      s1_provided = s1_provided || hit          // Once hit then provide
      s1_provider = Mux(hit, i.U, s1_provider)  // Use the last hit as provider
      s1_altPred = Mux(hit, true.B, s1_altPred) // Save current pred as potential altpred
      s1_altTarget = Mux(hit, target, s1_altTarget)
    }
    s1_provideds(w)      := s1_provided
    s1_providers(w)      := s1_provider
    s1_finalAltPreds(w)  := s1_finalAltPred
    s1_tageTakens(w)     := s1_tageTaken
    s1_tageTargets(w)    := s1_tageTarget
    s1_providerUs(w)     := s1_resps(s1_provider)(w).bits.u
    s1_providerCtrs(w)   := s1_resps(s1_provider)(w).bits.ctr

    when(io.s2_fire && io.in.bits.resp_in(0).s2.hit_taken_on_jalr && s2_tageTakens(w)) {
      // FIXME: should use s1 globally
      io.out.resp.s2.preds.jmp_target := s2_tageTargets(w)
    }

    resp_meta(w).provider.valid := s2_provideds(w)
    resp_meta(w).provider.bits  := s2_providers(w)
    resp_meta(w).altDiffers     := s2_finalAltPreds(w) =/= s2_tageTakens(w)
    resp_meta(w).providerU      := s2_providerUs(w)
    resp_meta(w).providerCtr    := s2_providerCtrs(w)
    resp_meta(w).taken          := s2_tageTakens(w)
    resp_meta(w).pred_cycle     := GTimer()
    // TODO: adjust for ITTAGE
    // Create a mask fo tables which did not hit our query, and also contain useless entries
    // and also uses a longer history than the provider
    val allocatableSlots = RegEnable(VecInit(s1_resps.map(r => !r(w).valid && r(w).bits.u === 0.U)).asUInt &
      ~(LowerMask(UIntToOH(s1_provider), ITTageNTables) & Fill(ITTageNTables, s1_provided.asUInt)), io.s1_fire
    )
    val allocLFSR   = LFSR64()(ITTageNTables - 1, 0)
    val firstEntry  = PriorityEncoder(allocatableSlots)
    val maskedEntry = PriorityEncoder(allocatableSlots & allocLFSR)
    val allocEntry  = Mux(allocatableSlots(maskedEntry), maskedEntry, firstEntry)
    resp_meta(w).allocate.valid := allocatableSlots =/= 0.U
    resp_meta(w).allocate.bits  := allocEntry

    // Update in loop
    val updateValid = updateValids(w)
    val updateMeta = updateMetas(w)
    val isUpdateTaken = updateValid && update.ftb_entry.jmpValid
    val isUpdateTarget = update.ftb_entry.getJmpTarget(update.pc)
    val updateMisPred = updateMisPreds(w)
    when (updateValid) {
      when (updateMeta.provider.valid) {
        val provider = updateMeta.provider.bits

        updateMask(provider)(w)   := true.B
        updateUMask(provider)(w)  := true.B

        updateU(provider)(w) := Mux(!updateMeta.altDiffers, updateMeta.providerU,
          Mux(updateMisPred, Mux(updateMeta.providerU === 0.U, 0.U, updateMeta.providerU - 1.U),
            Mux(updateMeta.providerU === 3.U, 3.U, updateMeta.providerU + 1.U))
        )
        updateTaken(provider)(w)  := isUpdateTaken
        updateTarget(provider)(w) := isUpdateTarget
        updateOldCtr(provider)(w) := updateMeta.providerCtr
        updateAlloc(provider)(w)  := false.B
      }
    }
    when (updateValid && updateMisPred) {
      val allocate = updateMeta.allocate
      when (allocate.valid) {
        updateMask(allocate.bits)(w)  := true.B
        updateTaken(allocate.bits)(w) := isUpdateTaken
        updateTarget(allocate.bits)(w) := isUpdateTarget
        updateAlloc(allocate.bits)(w) := true.B
        updateUMask(allocate.bits)(w) := true.B
        updateU(allocate.bits)(w) := 0.U
      }.otherwise {

        val provider = updateMeta.provider
        val decrMask = Mux(provider.valid, ~LowerMask(UIntToOH(provider.bits), ITTageNTables), 0.U(ITTageNTables.W))
        for (i <- 0 until ITTageNTables) {
          when (decrMask(i)) {
            updateUMask(i)(w) := true.B
            updateU(i)(w) := 0.U
          }
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
    for (w <- 0 until ITTageBanks) {
      tables(i).io.update.mask(w) := RegNext(updateMask(i)(w))
      tables(i).io.update.taken(w) := RegNext(updateTaken(i)(w))
      tables(i).io.update.target(w) := RegNext(updateTarget(i)(w))
      tables(i).io.update.alloc(w) := RegNext(updateAlloc(i)(w))
      tables(i).io.update.oldCtr(w) := RegNext(updateOldCtr(i)(w))

      tables(i).io.update.uMask(w) := RegNext(updateUMask(i)(w))
      tables(i).io.update.u(w) := RegNext(updateU(i)(w))
      tables(i).io.update.pc := RegNext(update.pc)
    }
    // use fetch pc instead of instruction pc
    tables(i).io.update.hist := RegNext(updateHist.predHist)
    tables(i).io.update.phist := RegNext(updatePhist)
  }

  // Debug and perf info

  def pred_perf(name: String, cnt: UInt)   = XSPerfAccumulate(s"${name}_at_pred", cnt)
  def commit_perf(name: String, cnt: UInt) = XSPerfAccumulate(s"${name}_at_commit", cnt)
  def tage_perf(name: String, pred_cnt: UInt, commit_cnt: UInt) = {
    pred_perf(name, pred_cnt)
    commit_perf(name, commit_cnt)
  }
  for (i <- 0 until ITTageNTables) {
    val pred_i_provided =
      VecInit(updateMetas map (m => m.provider.valid && m.provider.bits === i.U))
    val commit_i_provided =
      VecInit(updateMetas zip updateValids map {
        case (m, v) => m.provider.valid && m.provider.bits === i.U && v
      })
    tage_perf(s"ittage_table_${i}_provided",
      PopCount(pred_i_provided),
      PopCount(commit_i_provided))
  }
  tage_perf("ittage_use_bim",
    PopCount(VecInit(updateMetas map (!_.provider.valid))),
    PopCount(VecInit(updateMetas zip updateValids map {
      case (m, v) => !m.provider.valid && v}))
  )
  def unconf(providerCtr: UInt) = providerCtr === 3.U || providerCtr === 4.U
  tage_perf("ittage_use_altpred",
    PopCount(VecInit(updateMetas map (
      m => m.provider.valid && unconf(m.providerCtr)))),
    PopCount(VecInit(updateMetas zip updateValids map {
      case (m, v) => m.provider.valid && unconf(m.providerCtr) && v
    })))
  tage_perf("ittage_provided",
    PopCount(updateMetas.map(_.provider.valid)),
    PopCount(VecInit(updateMetas zip updateValids map {
      case (m, v) => m.provider.valid && v
    })))

  if (debug) {
    for (b <- 0 until ITTageBanks) {
      val m = updateMetas(b)
      // val bri = u.metas(b)
      XSDebug(updateValids(b), "update(%d): pc=%x, cycle=%d, hist=%x, taken:%b, misPred:%d, bimctr:%d, pvdr(%d):%d, altDiff:%d, pvdrU:%d, pvdrCtr:%d, alloc(%d):%d\n",
        b.U, update.pc, 0.U, updateHist.predHist, update.preds.taken_mask(b), update.mispred_mask(b),
        0.U, m.provider.valid, m.provider.bits, m.altDiffers, m.providerU, m.providerCtr, m.allocate.valid, m.allocate.bits
      )
    }
    val s2_resps = RegEnable(s1_resps, io.s1_fire)
    XSDebug("req: v=%d, pc=0x%x, hist=%b\n", io.s0_fire, s0_pc, io.in.bits.ghist)
    XSDebug("s1_fire:%d, resp: pc=%x, hist=%b\n", io.s1_fire, debug_pc_s1, debug_hist_s1)
    XSDebug("s2_fireOnLastCycle: resp: pc=%x, target=%x, hist=%b, hits=%b, takens=%b\n",
      debug_pc_s2, io.out.resp.s2.target, debug_hist_s2, s2_provideds.asUInt, s2_tageTakens.asUInt)
    for (i <- 0 until ITTageNTables) {
      XSDebug("TageTable(%d): valids:%b, resp_ctrs:%b, resp_us:%b\n",
        i.U, VecInit(s2_resps(i).map(_.valid)).asUInt, Cat(s2_resps(i).map(_.bits.ctr)),
        Cat(s2_resps(i).map(_.bits.u)))
    }
    // XSDebug(io.update.valid && updateIsBr, p"update: sc: ${updateSCMeta}\n")
    // XSDebug(true.B, p"scThres: use(${useThreshold}), update(${updateThreshold})\n")
  }
}


// class Tage_SC(implicit p: Parameters) extends Tage with HasSC {}
