/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

trait TageParams extends HasXSParameter with HasBPUParameter {
  //                   Sets  Hist   Tag
  val TableInfo = Seq(( 128*8,    2,    7),
                      ( 128*8,    4,    7),
                      ( 256*8,    8,    8),
                      ( 256*8,   16,    8),
                      ( 128*8,   32,    9),
                      ( 128*8,   64,    9))
                      // (  64,   64,   11),
                      // (  64,  101,   12),
                      // (  64,  160,   12),
                      // (  64,  254,   13),
                      // (  32,  403,   14),
                      // (  32,  640,   15))
  val TageNTables = TableInfo.size // Number of tage tables
  val UBitPeriod = 2048
  val TageBanks = numBr
  val TageCtrBits = 3

  val TotalBits = TableInfo.map {
    case (s, h, t) => {
      s * (1+t+TageCtrBits) * TageBanks
    }
  }.reduce(_+_)
}

trait HasFoldedHistory {
  val histLen: Int
  val phistLen: Int
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
  val compute_folded_phist = compute_folded_hist(_: UInt, _: Int)(phistLen)
}

abstract class TageBundle(implicit p: Parameters)
  extends XSBundle with TageParams with BPUUtils

abstract class TageModule(implicit p: Parameters)
  extends XSModule with TageParams with BPUUtils
  {}


class TageReq(implicit p: Parameters) extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
  val phist = UInt(PathHistoryLength.W)
  val mask = UInt(numBr.W)
}

class TageResp(implicit p: Parameters) extends TageBundle {
  val ctr = UInt(TageCtrBits.W)
  val u = UInt(2.W)
}

class TageUpdate(implicit p: Parameters) extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
  val phist = UInt(PathHistoryLength.W)
  // update tag and ctr
  val mask = Vec(TageBanks, Bool())
  val taken = Vec(TageBanks, Bool())
  val alloc = Vec(TageBanks, Bool())
  val oldCtr = Vec(TageBanks, UInt(TageCtrBits.W))
  // update u
  val uMask = Vec(TageBanks, Bool())
  val u = Vec(TageBanks, UInt(2.W))
}

class SCMeta(val useSC: Boolean)(implicit p: Parameters) extends XSBundle with HasSCParameter {
  val tageTaken = if (useSC) Bool() else UInt(0.W)
  val scUsed = if (useSC) Bool() else UInt(0.W)
  val scPred = if (useSC) Bool() else UInt(0.W)
  // Suppose ctrbits of all tables are identical
  val ctrs = if (useSC) Vec(SCNTables, SInt(SCCtrBits.W)) else Vec(SCNTables, SInt(0.W))
}

class TageMeta(implicit p: Parameters) extends XSBundle with TageParams{
  val provider = ValidUndirectioned(UInt(log2Ceil(TageNTables).W))
  val altDiffers = Bool()
  val providerU = UInt(2.W)
  val providerCtr = UInt(3.W)
  val allocate = ValidUndirectioned(UInt(log2Ceil(TageNTables).W))
  val taken = Bool()
  val scMeta = new SCMeta(EnableSC)
  val pred_cycle = UInt(64.W) // TODO: Use Option
}

class FakeTageTable()(implicit p: Parameters) extends TageModule {
  val io = IO(new Bundle() {
    val req = Input(Valid(new TageReq))
    val resp = Output(Vec(TageBanks, Valid(new TageResp)))
    val update = Input(new TageUpdate)
  })
  io.resp := DontCare

}
@chiselName
class TageTable
(
  val nRows: Int, val histLen: Int, val tagLen: Int, val uBitPeriod: Int
)(implicit p: Parameters)
  extends TageModule with HasFoldedHistory {
  val io = IO(new Bundle() {
    val req = Input(Valid(new TageReq))
    val resp = Output(Vec(TageBanks, Valid(new TageResp)))
    val update = Input(new TageUpdate)
  })

  // bypass entries for tage update
  val wrBypassEntries = 4
  val phistLen = PathHistoryLength

  def compute_tag_and_hash(unhashed_idx: UInt, hist: UInt, phist: UInt) = {
    val idx_history = compute_folded_ghist(hist, log2Ceil(nRows))
    val idx = (unhashed_idx ^ idx_history)(log2Ceil(nRows) - 1, 0)
    val tag_history = compute_folded_ghist(hist, tagLen) ^ compute_folded_phist(phist, tagLen)
    // Use another part of pc to make tags
    val tag = ((unhashed_idx >> log2Ceil(nRows)) ^ tag_history) (tagLen - 1, 0)
    (idx, tag)
  }

  def inc_ctr(ctr: UInt, taken: Bool): UInt = satUpdate(ctr, TageCtrBits, taken)

  class TageEntry() extends TageBundle {
    val valid = Bool()
    val tag = UInt(tagLen.W)
    val ctr = UInt(TageCtrBits.W)
  }

  // Why need add instOffsetBits?
  // val tageEntrySz = instOffsetBits + tagLen + TageCtrBits
  val tageEntrySz = 1 + tagLen + TageCtrBits

  // pc is start address of basic block, most 2 branch inst in block
  // def getUnhashedIdx(pc: UInt) = pc >> (instOffsetBits+log2Ceil(TageBanks))
  def getUnhashedIdx(pc: UInt): UInt = pc >> instOffsetBits

  val s1_pc = io.req.bits.pc
  val s1_unhashed_idx = getUnhashedIdx(io.req.bits.pc)

  val s1_idxes, s2_idxes  = Wire(Vec(TageBanks, UInt(log2Ceil(nRows).W)))
  val s1_tags,  s2_tags   = Wire(Vec(TageBanks, UInt(tagLen.W)))

  val hi_us   = Seq.fill(TageBanks)(Module(new Folded1WDataModuleTemplate(Bool(), nRows, numRead=1, isSync=true, width=8)))
  val lo_us   = Seq.fill(TageBanks)(Module(new Folded1WDataModuleTemplate(Bool(), nRows, numRead=1, isSync=true, width=8)))
  val tables  = Seq.fill(TageBanks)(Module(new SRAMTemplate(new TageEntry, set=nRows, way=1, shouldReset=true, holdRead=true, singlePort=false)))


  for (b <- 0 until TageBanks) {
    val (idx, tag) = compute_tag_and_hash(s1_unhashed_idx, io.req.bits.hist << b, io.req.bits.phist)
    s1_idxes(b) := idx
    s1_tags(b)  := tag
    
    tables(b).io.r.req.valid := io.req.valid
    tables(b).io.r.req.bits.setIdx := s1_idxes(b)

    hi_us(b).io.raddr(0) := s1_idxes(b)
    lo_us(b).io.raddr(0) := s1_idxes(b)
    
  }

  s2_idxes := RegEnable(s1_idxes, io.req.valid)
  s2_tags  := RegEnable(s1_tags, io.req.valid)

  val s2_hi_us_r = hi_us.map(_.io.rdata(0))
  val s2_lo_us_r = lo_us.map(_.io.rdata(0))
  
  val s2_table_r = tables.map(_.io.r.resp.data(0))

  val s2_req_rhits = VecInit((0 until TageBanks).map(b => {
    s2_table_r(b).valid && s2_table_r(b).tag === s2_tags(b)
  }))

  (0 until TageBanks).map(b => {
    io.resp(b).valid := s2_req_rhits(b) // && s2_mask(b)
    io.resp(b).bits.ctr := s2_table_r(b).ctr
    io.resp(b).bits.u := Cat(s2_hi_us_r(b),s2_lo_us_r(b))
  })


  // uBitPeriod = 2048, nRows = 128
  val clear_u_ctr = Seq.fill(TageBanks)(RegInit(0.U((log2Ceil(uBitPeriod) + log2Ceil(nRows) + 1).W)))
  clear_u_ctr.foreach(c => c := c + 1.U)

  val doing_clear_u = clear_u_ctr.map(_(log2Ceil(uBitPeriod)-1,0) === 0.U)
  val doing_clear_u_hi = doing_clear_u.zip(clear_u_ctr).map{case (d, ctr) => d && ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 1.U}
  val doing_clear_u_lo = doing_clear_u.zip(clear_u_ctr).map{case (d, ctr) => d && ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 0.U}
  val clear_u_idx = clear_u_ctr.map(_ >> log2Ceil(uBitPeriod))

  // Use fetchpc to compute hash
  val update_idxes  = Wire(Vec(TageBanks, UInt(log2Ceil(nRows).W)))
  val update_tags   = Wire(Vec(TageBanks, UInt(tagLen.W)))

  val update_wdata = Wire(Vec(TageBanks, new TageEntry))

  for (b <- 0 until TageBanks) {
    val (idx, tag) =  compute_tag_and_hash(getUnhashedIdx(io.update.pc), io.update.hist << b, io.update.phist)
    update_idxes(b) := idx
    update_tags(b) := tag

    tables(b).io.w.apply(
      valid = io.update.mask(b),
      data = update_wdata(b),
      setIdx = update_idxes(b),
      waymask = io.update.mask(b)
    )
  }

  val update_hi_wdata = Wire(Vec(TageBanks, Bool()))
  val update_lo_wdata = Wire(Vec(TageBanks, Bool()))
  
  for (b <- 0 until TageBanks) {
    val hi_wen = io.update.uMask(b) || doing_clear_u_hi(b)

    hi_us(b).io.wen := hi_wen
    hi_us(b).io.wdata := Mux(doing_clear_u_hi(b), false.B, update_hi_wdata(b))
    hi_us(b).io.waddr := Mux(doing_clear_u_hi(b), clear_u_idx(b), update_idxes(b))

    val lo_wen = io.update.uMask(b) || doing_clear_u_lo(b)

    lo_us(b).io.wen := lo_wen
    lo_us(b).io.wdata := Mux(doing_clear_u_lo(b), false.B, update_lo_wdata(b))
    lo_us(b).io.waddr := Mux(doing_clear_u_lo(b), clear_u_idx(b), update_idxes(b)),
  }
  
  class WrBypass extends XSModule {
    val io = IO(new Bundle {
      val wen = Input(Bool())
      val update_idx  = Input(UInt(log2Ceil(nRows).W))
      val update_tag  = Input(UInt(tagLen.W))
      val update_ctrs  = Flipped(ValidIO(UInt(TageCtrBits.W)))
      
      val hit   = Output(Bool())
      val ctrs  = ValidIO(UInt(TageCtrBits.W))
    })
    
    val tags        = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(tagLen.W))))
    val idxes       = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(log2Ceil(nRows).W))))
    val ctrs        = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(TageCtrBits.W))))
    val ctr_valids  = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, Bool())))
    val enq_idx     = RegInit(0.U(log2Ceil(wrBypassEntries).W))
    
    val hits = VecInit((0 until wrBypassEntries).map { i =>
      tags(i) === io.update_tag && idxes(i) === io.update_idx
    })
    
    val hit = hits.reduce(_||_)
    val hit_idx = ParallelPriorityEncoder(hits)
    
    io.hit := hit
    io.ctrs.valid := ctr_valids(hit_idx)
    io.ctrs.bits := ctrs(hit_idx)
    
    when (io.wen) {
      when (hit) {
        ctrs(hit_idx) := io.update_ctrs.bits
        ctr_valids(hit_idx) := io.update_ctrs.valid
      }.otherwise {
        ctrs(enq_idx) := io.update_ctrs.bits
        ctr_valids(enq_idx) := io.update_ctrs.valid
      }
    }
    
    when(io.wen && !hit) {
      tags(enq_idx) := io.update_tag
      idxes(enq_idx) := io.update_idx
      enq_idx := (enq_idx + 1.U)(log2Ceil(wrBypassEntries)-1, 0)
    }
  }
  
  val wrbypass = Seq.fill(TageBanks)(Module(new WrBypass))

  // val updateBank = PriorityEncoder(io.update.mask)

  for (b <- 0 until TageBanks) {
    wrbypass(b).io.wen := io.update.mask(b)
    wrbypass(b).io.update_ctrs.valid := io.update.mask(b)
    wrbypass(b).io.update_ctrs.bits := update_wdata(b).ctr

    update_wdata(b).ctr   := Mux(io.update.alloc(b),
      Mux(io.update.taken(b), 4.U,
                              3.U
      ),
      Mux(wrbypass(b).io.hit && wrbypass(b).io.ctrs.valid,
            inc_ctr(wrbypass(b).io.ctrs.bits, io.update.taken(b)),
            inc_ctr(io.update.oldCtr(b), io.update.taken(b))
      )
    )
    update_wdata(b).valid := true.B
    update_wdata(b).tag   := update_tags(b)

    update_hi_wdata(b)    := io.update.u(b)(1)
    update_lo_wdata(b)    := io.update.u(b)(0)

    wrbypass(b).io.update_idx := update_idxes(b)
    wrbypass(b).io.update_tag := update_tags(b)
  }

  for (b <- 0 until TageBanks) {
    XSPerfAccumulate(f"tage_table_wrbypass(${b})_hit", io.update.mask(b) && wrbypass(b).io.hit)
    XSPerfAccumulate(f"tage_table_wrbypass(${b})_enq", io.update.mask(b) && !wrbypass(b).io.hit)
  }
  
  XSPerfAccumulate("tage_table_hits", PopCount(VecInit(io.resp.map(_.valid))))

  if (BPUDebug && debug) {
    val u = io.update
    val b = PriorityEncoder(u.mask)
    val ub = PriorityEncoder(u.uMask)
    val idxes = s1_idxes
    val tags = s1_tags
    for (i <- 0 until TageBanks) {
      XSDebug(io.req.valid,
        p"[${i}]tableReq: pc=0x${Hexadecimal(io.req.bits.pc)}, " +
        p"[${i}]hist=${Hexadecimal(io.req.bits.hist << i)}, idx=$idxes(i), " +
        p"[${i}]tag=$tags(i)\n")
      XSDebug(RegNext(io.req.valid && io.req.bits.mask(i)) && s2_req_rhits(i),
        p"TageTableResp[$i]: idx=${s2_idxes(i)}, hit:${s2_req_rhits(i)}, " +
        p"ctr:${io.resp(i).bits.ctr}, u:${io.resp(i).bits.u}\n")
      XSDebug(io.update.mask(i),
        p"update Table bank $i: pc:${Hexadecimal(u.pc)}, hist:${Hexadecimal(u.hist)}, " +
        p"taken:${u.taken(i)}, alloc:${u.alloc(i)}, oldCtr:${u.oldCtr(i)}\n")
      XSDebug(io.update.mask(i),
        p"update Table bank $i: writing tag:${update_tags(i)}, " +
        p"ctr: ${update_wdata(i).ctr} in idx ${update_idxes(i)}\n")
      val hitCtr = wrbypass(i).io.ctrs.bits
      XSDebug(wrbypass(i).io.hit && wrbypass(i).io.update_ctrs.valid && io.update.mask(i),
        // p"bank $i wrbypass hit wridx:$wrbypass_hit_idx, idx:$update_idx, tag: $update_tag, " +
        p"ctr:$hitCtr, newCtr:${update_wdata(i).ctr}")
    }

    XSDebug(RegNext(io.req.valid) && !s2_req_rhits.reduce(_||_), "TageTableResp: no hits!\n")

    // ------------------------------Debug-------------------------------------
    val valids = Reg(Vec(TageBanks, Vec(nRows, Bool())))
    when (reset.asBool) { valids.foreach(b => b.foreach(r => r := false.B)) }
    (0 until TageBanks).map( b => { when (io.update.mask(b)) { valids(b)(update_idxes(b)) := true.B }})
    XSDebug("Table usage:------------------------\n")
    (0 until TageBanks).map( b => { XSDebug("Bank(%d): %d out of %d rows are valid\n", b.U, PopCount(valids(b)), nRows.U)})
  }

}

abstract class BaseTage(implicit p: Parameters) extends BasePredictor with TageParams with BPUUtils {
}

class FakeTage(implicit p: Parameters) extends BaseTage {
  io.out <> 0.U.asTypeOf(DecoupledIO(new BasePredictorOutput))

  // io.s0_ready := true.B
  io.s1_ready := true.B
  io.s2_ready := true.B
  io.s3_ready := true.B
}

@chiselName
class Tage(implicit p: Parameters) extends BaseTage {
  override val meta_size = 0.U.asTypeOf(Vec(TageBanks, new TageMeta)).getWidth

  val tables = TableInfo.map {
    case (nRows, histLen, tagLen) =>
      val t = Module(new TageTable(nRows, histLen, tagLen, UBitPeriod))
      t.io.req.valid := io.s1_fire
      t.io.req.bits.pc := s1_pc
      t.io.req.bits.hist := io.in.bits.ghist
      t.io.req.bits.phist := io.in.bits.phist
      t.io.req.bits.mask := VecInit(Seq.fill(numBr)(1.U(1.W))).asUInt()
      t
  }

  // Keep the table responses to process in s3
  // val if4_resps = RegEnable(VecInit(tables.map(t => t.io.resp)), enable=s3_fire)
  // val if4_scResps = RegEnable(VecInit(scTables.map(t => t.io.resp)), enable=s3_fire)

  val s2_resps = VecInit(tables.map(t => t.io.resp))

  val s2_bim = io.in.bits.resp_in(0).s2.preds
  val s3_bim = RegEnable(s2_bim, enable=io.s2_fire)

  val debug_pc_s1 = RegEnable(s0_pc, enable=io.s0_fire)
  val debug_pc_s2 = RegEnable(debug_pc_s1, enable=io.s1_fire)
  val debug_pc_s3 = RegEnable(debug_pc_s2, enable=io.s2_fire)

  val debug_hist_s1 = io.in.bits.ghist
  val debug_hist_s2 = RegEnable(debug_hist_s1, enable=io.s1_fire)
  val debug_hist_s3 = RegEnable(debug_hist_s2, enable=io.s2_fire)

  val s2_tageTakens    = Wire(Vec(TageBanks, Bool()))
  val s2_provideds     = Wire(Vec(TageBanks, Bool()))
  val s2_providers     = Wire(Vec(TageBanks, UInt(log2Ceil(TageNTables).W)))
  val s2_finalAltPreds = Wire(Vec(TageBanks, Bool()))
  val s2_providerUs    = Wire(Vec(TageBanks, UInt(2.W)))
  val s2_providerCtrs  = Wire(Vec(TageBanks, UInt(TageCtrBits.W)))

  val s3_tageTakens    = RegEnable(s2_tageTakens, io.s2_fire)
  val s3_provideds     = RegEnable(s2_provideds, io.s2_fire)
  val s3_providers     = RegEnable(s2_providers, io.s2_fire)
  val s3_finalAltPreds = RegEnable(s2_finalAltPreds, io.s2_fire)
  val s3_providerUs    = RegEnable(s2_providerUs, io.s2_fire)
  val s3_providerCtrs  = RegEnable(s2_providerCtrs, io.s2_fire)

  val resp_meta = WireInit(0.U.asTypeOf(Vec(TageBanks, new TageMeta)))

  io.out.resp := io.in.bits.resp_in(0)
  io.out.s3_meta := resp_meta.asUInt

  val ftb_hit = io.in.bits.resp_in(0).s3.preds.hit
  val ftb_entry = io.in.bits.resp_in(0).s3.ftb_entry
  val resp_s3 = io.out.resp.s3

  // Update logic
  val u_valid = io.update.valid
  val update = io.update.bits
  val updateValids = VecInit(update.ftb_entry.brValids.map(_ && u_valid))
  val updateHist = update.ghist
  val updatePhist = update.phist

  val updateMetas = update.meta.asTypeOf(Vec(TageBanks, new TageMeta))

  val updateMask    = WireInit(0.U.asTypeOf(Vec(TageNTables, Vec(TageBanks, Bool()))))
  val updateUMask   = WireInit(0.U.asTypeOf(Vec(TageNTables, Vec(TageBanks, Bool()))))
  val updateTaken   = Wire(Vec(TageNTables, Vec(TageBanks, Bool())))
  val updateAlloc   = Wire(Vec(TageNTables, Vec(TageBanks, Bool())))
  val updateOldCtr  = Wire(Vec(TageNTables, Vec(TageBanks, UInt(TageCtrBits.W))))
  val updateU       = Wire(Vec(TageNTables, Vec(TageBanks, UInt(2.W))))
  updateTaken   := DontCare
  updateAlloc   := DontCare
  updateOldCtr  := DontCare
  updateU       := DontCare

  val updateMisPreds = update.mispred_mask

  // access tag tables and output meta info
  for (w <- 0 until TageBanks) {
    val s2_tageTaken     = WireInit(s2_bim.taken_mask(w))
    var s2_altPred       = s2_bim.taken_mask(w)
    val s2_finalAltPred  = WireInit(s2_bim.taken_mask(w))
    var s2_provided      = false.B
    var s2_provider      = 0.U

    for (i <- 0 until TageNTables) {
      val hit = s2_resps(i)(w).valid
      val ctr = s2_resps(i)(w).bits.ctr
      when (hit) {
        s2_tageTaken := Mux(ctr === 3.U || ctr === 4.U, s2_altPred, ctr(2)) // Use altpred on weak taken
        s2_finalAltPred := s2_altPred
      }
      s2_provided = s2_provided || hit          // Once hit then provide
      s2_provider = Mux(hit, i.U, s2_provider)  // Use the last hit as provider
      s2_altPred = Mux(hit, ctr(2), s2_altPred) // Save current pred as potential altpred
    }
    s2_provideds(w)      := s2_provided
    s2_providers(w)      := s2_provider
    s2_finalAltPreds(w)  := s2_finalAltPred
    s2_tageTakens(w)     := s2_tageTaken
    s2_providerUs(w)     := s2_resps(s2_provider)(w).bits.u
    s2_providerCtrs(w)   := s2_resps(s2_provider)(w).bits.ctr

    when(ftb_hit) {
      resp_s3.preds.taken_mask(w) := s3_tageTakens(w) // && ctrl.tage_enable
    }

    resp_meta(w).provider.valid := s3_provideds(w)
    resp_meta(w).provider.bits  := s3_providers(w)
    resp_meta(w).altDiffers     := s3_finalAltPreds(w) =/= s3_tageTakens(w)
    resp_meta(w).providerU      := s3_providerUs(w)
    resp_meta(w).providerCtr    := s3_providerCtrs(w)
    resp_meta(w).taken          := s3_tageTakens(w)
    resp_meta(w).pred_cycle     := GTimer()

    // Create a mask fo tables which did not hit our query, and also contain useless entries
    // and also uses a longer history than the provider
    val allocatableSlots = RegEnable(VecInit(s2_resps.map(r => !r(w).valid && r(w).bits.u === 0.U)).asUInt &
      ~(LowerMask(UIntToOH(s2_provider), TageNTables) & Fill(TageNTables, s2_provided.asUInt)), io.s2_fire
    )
    val allocLFSR   = LFSR64()(TageNTables - 1, 0)
    val firstEntry  = PriorityEncoder(allocatableSlots)
    val maskedEntry = PriorityEncoder(allocatableSlots & allocLFSR)
    val allocEntry  = Mux(allocatableSlots(maskedEntry), maskedEntry, firstEntry)
    resp_meta(w).allocate.valid := allocatableSlots =/= 0.U
    resp_meta(w).allocate.bits  := allocEntry

    // Update in loop
    val updateValid = updateValids(w) && !(PriorityEncoder(update.preds.taken_mask) < w.U)
    val updateMeta = updateMetas(w)
    val isUpdateTaken = updateValid && update.preds.taken_mask(w)
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
        updateOldCtr(provider)(w) := updateMeta.providerCtr
        updateAlloc(provider)(w)  := false.B
      }
    }
    when (updateValid && updateMisPred) {
      val allocate = updateMeta.allocate
      when (allocate.valid) {
        updateMask(allocate.bits)(w)  := true.B
        updateTaken(allocate.bits)(w) := isUpdateTaken
        updateAlloc(allocate.bits)(w) := true.B
        updateUMask(allocate.bits)(w) := true.B
        updateU(allocate.bits)(w) := 0.U
      }.otherwise {

        val provider = updateMeta.provider
        val decrMask = Mux(provider.valid, ~LowerMask(UIntToOH(provider.bits), TageNTables), 0.U(TageNTables.W))
        for (i <- 0 until TageNTables) {
          when (decrMask(i)) {
            updateUMask(i)(w) := true.B
            updateU(i)(w) := 0.U
          }
        }
      }
    }
  }

  val fallThruAddr = getFallThroughAddr(s3_pc, ftb_entry.carry, ftb_entry.pftAddr)

  when(ftb_hit) {
    io.out.resp.s3.preds.target := Mux((resp_s3.real_taken_mask.asUInt & ftb_entry.brValids.asUInt) =/= 0.U,
      PriorityMux(resp_s3.real_taken_mask.asUInt & ftb_entry.brValids.asUInt, ftb_entry.brTargets),
      Mux(ftb_entry.jmpValid, ftb_entry.jmpTarget, fallThruAddr))
  }


  for (i <- 0 until TageNTables) {
    for (w <- 0 until TageBanks) {
      tables(i).io.update.mask(w) := RegNext(updateMask(i)(w))
      tables(i).io.update.taken(w) := RegNext(updateTaken(i)(w))
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
  for (i <- 0 until TageNTables) {
    val pred_i_provided =
      VecInit(updateMetas map (m => m.provider.valid && m.provider.bits === i.U))
    val commit_i_provided =
      VecInit(updateMetas zip updateValids map {
        case (m, v) => m.provider.valid && m.provider.bits === i.U && v
      })
    tage_perf(s"tage_table_${i}_provided",
      PopCount(pred_i_provided),
      PopCount(commit_i_provided))
  }
  tage_perf("tage_use_bim",
    PopCount(VecInit(updateMetas map (!_.provider.valid))),
    PopCount(VecInit(updateMetas zip updateValids map {
        case (m, v) => !m.provider.valid && v}))
    )
  def unconf(providerCtr: UInt) = providerCtr === 3.U || providerCtr === 4.U
  tage_perf("tage_use_altpred",
    PopCount(VecInit(updateMetas map (
      m => m.provider.valid && unconf(m.providerCtr)))),
    PopCount(VecInit(updateMetas zip updateValids map {
      case (m, v) => m.provider.valid && unconf(m.providerCtr) && v
    })))
  tage_perf("tage_provided",
    PopCount(updateMetas.map(_.provider.valid)),
    PopCount(VecInit(updateMetas zip updateValids map {
      case (m, v) => m.provider.valid && v
    })))

  if (debug) {
    for (b <- 0 until TageBanks) {
      val m = updateMetas(b)
      // val bri = u.metas(b)
      XSDebug(updateValids(b), "update(%d): pc=%x, cycle=%d, hist=%x, taken:%b, misPred:%d, bimctr:%d, pvdr(%d):%d, altDiff:%d, pvdrU:%d, pvdrCtr:%d, alloc(%d):%d\n",
        b.U, update.pc, 0.U, updateHist.predHist, update.preds.taken_mask(b), update.mispred_mask(b),
        0.U, m.provider.valid, m.provider.bits, m.altDiffers, m.providerU, m.providerCtr, m.allocate.valid, m.allocate.bits
      )
    }
    val s3_resps = RegEnable(s2_resps, io.s2_fire)
    XSDebug("req: v=%d, pc=0x%x, hist=%b\n", io.s1_fire, s1_pc, io.in.bits.ghist)
    XSDebug("s2_fire:%d, resp: pc=%x, hist=%b\n", io.s2_fire, debug_pc_s2, debug_hist_s2)
    XSDebug("s3_fireOnLastCycle: resp: pc=%x, target=%x, hist=%b, hits=%b, takens=%b\n",
      debug_pc_s3, io.out.resp.s3.preds.target, debug_hist_s3, s3_provideds.asUInt, s3_tageTakens.asUInt)
    for (i <- 0 until TageNTables) {
      XSDebug("TageTable(%d): valids:%b, resp_ctrs:%b, resp_us:%b\n",
        i.U, VecInit(s3_resps(i).map(_.valid)).asUInt, Cat(s3_resps(i).map(_.bits.ctr)),
        Cat(s3_resps(i).map(_.bits.u)))
    }
    // XSDebug(io.update.valid && updateIsBr, p"update: sc: ${updateSCMeta}\n")
    // XSDebug(true.B, p"scThres: use(${useThreshold}), update(${updateThreshold})\n")
  }
}


class Tage_SC(implicit p: Parameters) extends Tage with HasSC {}
