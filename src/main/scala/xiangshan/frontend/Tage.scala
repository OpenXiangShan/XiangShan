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
  val BankTageNTables = BankTageTableInfos.map(_.size) // Number of tage tables
  val UBitPeriod = 256
  val TageCtrBits = 3
  val uFoldedWidth = 8

  val TotalBits = BankTageTableInfos.map { info =>
    info.map{
      case (s, h, t) => {
        s * (1+t+TageCtrBits)
      }
    }.reduce(_+_)
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
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
  val phist = UInt(PathHistoryLength.W)
}

class TageResp(implicit p: Parameters) extends TageBundle {
  val ctr = UInt(TageCtrBits.W)
  val u = UInt(2.W)
}

class TageUpdate(implicit p: Parameters) extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
  val phist = UInt(PathHistoryLength.W)
  // update tag and ctr
  val mask = Bool()
  val taken = Bool()
  val alloc = Bool()
  val oldCtr = UInt(TageCtrBits.W)
  // update u
  val uMask = Bool()
  val u = UInt(2.W)
}

class TageMeta(val bank: Int)(implicit p: Parameters)
  extends TageBundle with HasSCParameter
{
  val provider = ValidUndirectioned(UInt(log2Ceil(BankTageNTables(bank)).W))
  val prednum = ValidUndirectioned(UInt(log2Ceil(BankTageNTables(bank)).W))
  val altprednum = ValidUndirectioned(UInt(log2Ceil(BankTageNTables(bank)).W))
  val altDiffers = Bool()
  val providerU = UInt(2.W)
  val providerCtr = UInt(TageCtrBits.W)
  val basecnt = UInt(2.W)
  val predcnt = UInt(3.W)
  val altpredhit = Bool()
  val altpredcnt = UInt(3.W)
  val allocate = ValidUndirectioned(UInt(log2Ceil(BankTageNTables(bank)).W))
  val taken = Bool()
  val scMeta = new SCMeta(EnableSC, BankSCNTables(bank))
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

trait TBTParams extends HasXSParameter {
  val BtSize = 2048
  val bypassEntries = 4
}

@chiselName
class TageBTable(implicit p: Parameters) extends XSModule with TBTParams{
  val io = IO(new Bundle {
    val s0_fire = Input(Bool())
    val s0_pc   = Input(UInt(VAddrBits.W))
    val s1_cnt     = Output(Vec(numBr,UInt(2.W)))
    val update_cnt  = Input(Vec(numBr,UInt(2.W)))
   // val update  = Input(new TageUpdate)
    val update = Flipped(Valid(new BranchPredictionUpdate))
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

  //io.s1_cnt := Cat((0 until numBr reverse).map(i => s1_read(i)(1,0))).asUInt()
  io.s1_cnt := bt.io.r.resp.data

  // Update logic
  val u_valid = io.update.valid
  val update = io.update.bits

  val u_idx = bimAddr.getIdx(update.pc)

  // Bypass logic
  val wrbypass_ctrs       = RegInit(0.U.asTypeOf(Vec(bypassEntries, Vec(numBr, UInt(2.W)))))
  val wrbypass_ctr_valids = RegInit(0.U.asTypeOf(Vec(bypassEntries, Vec(numBr, Bool()))))
  val wrbypass_idx        = RegInit(0.U.asTypeOf(Vec(bypassEntries, UInt(log2Up(BtSize).W))))
  val wrbypass_enq_ptr    = RegInit(0.U(log2Up(bypassEntries).W))

  val wrbypass_hits = VecInit((0 until bypassEntries).map(i =>
    !doing_reset && wrbypass_idx(i) === u_idx))
  val wrbypass_hit = wrbypass_hits.reduce(_||_)
  val wrbypass_hit_idx = PriorityEncoder(wrbypass_hits)

  val oldCtrs = VecInit((0 until numBr).map(i =>
    Mux(wrbypass_hit && wrbypass_ctr_valids(wrbypass_hit_idx)(i),
    wrbypass_ctrs(wrbypass_hit_idx)(i), io.update_cnt(i))))
    //wrbypass_ctrs(wrbypass_hit_idx)(i), update.meta(2*i+1, 2*i))))

  def satUpdate(old: UInt, len: Int, taken: Bool): UInt = {
    val oldSatTaken = old === ((1 << len)-1).U
    val oldSatNotTaken = old === 0.U
    Mux(oldSatTaken && taken, ((1 << len)-1).U,
      Mux(oldSatNotTaken && !taken, 0.U,
        Mux(taken, old + 1.U, old - 1.U)))
  }

  val newTakens = update.preds.br_taken_mask
  val newCtrs = VecInit((0 until numBr).map(i =>
    satUpdate(oldCtrs(i), 2, newTakens(i))
  ))

//  val update_mask = LowerMask(PriorityEncoderOH(update.preds.taken_mask.asUInt))
  val need_to_update = VecInit((0 until numBr).map(i => u_valid && update.ftb_entry.brValids(i)/* && update_mask(i)*/))

  when (reset.asBool) { wrbypass_ctr_valids.foreach(_ := VecInit(Seq.fill(numBr)(false.B)))}

  for (i <- 0 until numBr) {
    when(need_to_update.reduce(_||_)) {
      when(wrbypass_hit) {
        when(need_to_update(i)) {
          wrbypass_ctrs(wrbypass_hit_idx)(i) := newCtrs(i)
          wrbypass_ctr_valids(wrbypass_hit_idx)(i) := true.B
        }
      }.otherwise {
        wrbypass_ctr_valids(wrbypass_enq_ptr)(i) := false.B
        when(need_to_update(i)) {
          wrbypass_ctrs(wrbypass_enq_ptr)(i) := newCtrs(i)
          wrbypass_ctr_valids(wrbypass_enq_ptr)(i) := true.B
        }
      }
    }
  }

  when (need_to_update.reduce(_||_) && !wrbypass_hit) {
    wrbypass_idx(wrbypass_enq_ptr) := u_idx
    wrbypass_enq_ptr := (wrbypass_enq_ptr + 1.U)(log2Up(bypassEntries)-1, 0)
  }

  bt.io.w.apply(
    valid = need_to_update.asUInt.orR || doing_reset,
    data = Mux(doing_reset, VecInit(Seq.fill(numBr)(2.U(2.W))), newCtrs),
    setIdx = Mux(doing_reset, resetRow, u_idx),
    waymask = Mux(doing_reset, Fill(numBr, 1.U(1.W)).asUInt(), need_to_update.asUInt())
  )

}


@chiselName
class TageTable
(
  val nRows: Int, val histLen: Int, val tagLen: Int, val uBitPeriod: Int, val tableIdx: Int
)(implicit p: Parameters)
  extends TageModule with HasFoldedHistory {
  val io = IO(new Bundle() {
    val req = Input(Valid(new TageReq))
    val resp = Output(Valid(new TageResp))
    val update = Input(new TageUpdate)
  })
  // val folded_hist = Wire(new FoldedHistory(histLen, log2Ceil(nRows), numBr))
  // // val folded_tag_hist = Wire(new FoldedHistory(histLen, tagLen, numBr))
  // def zeros = VecInit(false.B, false.B)
  // folded_hist.update(zeros, zeros, 0.U(64.W), 0.U(6.W))
  // bypass entries for tage update
  val wrBypassEntries = 8
  val phistLen = if (PathHistoryLength > histLen) histLen else PathHistoryLength

  // def compute_tag_and_hash(unhashed_idx: UInt, hist: UInt, phist: UInt) = {
  //   def F(phist: UInt, len: Int) = {
  //     val lenMask = Fill(len, 1.U(1.W))
  //     val rowMask = Fill(log2Ceil(nRows), 1.U(1.W))
  //     val masked = phist & lenMask
  //     val a1 = masked & rowMask
  //     val a2 = masked >> log2Ceil(nRows)
  //     val a3 = ((a2 << tableIdx) & rowMask) + (a2 >> (log2Ceil(nRows) - tableIdx))
  //     val a4 = a1 ^ a3
  //     val res = ((a3 << tableIdx) & rowMask) + (a3 >> (log2Ceil(nRows) - tableIdx))
  //     res
  //   }
  //   val idx_history = compute_folded_ghist(hist, log2Ceil(nRows))
  //   val idx_phist = F(phist, (if (PathHistoryLength > histLen) histLen else PathHistoryLength))
  //   // val idx = (unhashed_idx ^ (unhashed_idx >> (log2Ceil(nRows)-tableIdx+1)) ^ idx_history ^ idx_phist)(log2Ceil(nRows) - 1, 0)
  //   val idx = (unhashed_idx ^ idx_history)(log2Ceil(nRows) - 1, 0)
  //   val tag_history = compute_folded_ghist(hist, tagLen)
  //   val alt_tag_history = compute_folded_ghist(hist, tagLen-1)
  //   // Use another part of pc to make tags
  //   val tag = ((unhashed_idx >> log2Ceil(nRows)) ^ tag_history ^ (alt_tag_history << 1)) (tagLen - 1, 0)
  //   (idx, tag)
  // }


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

  // val s1_pc = io.req.bits.pc
  val req_unhashed_idx = getUnhashedIdx(io.req.bits.pc)

  val hi_us  = Module(new FoldedSRAMTemplate(Bool(), nRows, width=uFoldedWidth, shouldReset=true, holdRead=true))
  val lo_us  = Module(new FoldedSRAMTemplate(Bool(), nRows, width=uFoldedWidth, shouldReset=true, holdRead=true))
  val table  = Module(new SRAMTemplate(new TageEntry, set=nRows, way=1, shouldReset=true, holdRead=true, singlePort=false))


  val (s0_idx, s0_tag) = compute_tag_and_hash(req_unhashed_idx, io.req.bits.folded_hist)

  table.io.r.req.valid := io.req.valid
  table.io.r.req.bits.setIdx := s0_idx

  hi_us.io.r.req.valid := io.req.valid
  hi_us.io.r.req.bits.setIdx := s0_idx
  lo_us.io.r.req.valid := io.req.valid
  lo_us.io.r.req.bits.setIdx := s0_idx

  val s1_idx = RegEnable(s0_idx, io.req.valid)
  val s1_tag = RegEnable(s0_tag, io.req.valid)

  val hi_us_r = hi_us.io.r.resp.data(0)
  val lo_us_r = lo_us.io.r.resp.data(0)

  val table_r = table.io.r.resp.data(0) // s1

  val req_rhit = table_r.valid && table_r.tag === s1_tag

  io.resp.valid := req_rhit
  io.resp.bits.ctr := table_r.ctr
  io.resp.bits.u := Cat(hi_us_r,lo_us_r)



  // uBitPeriod = 2048, nRows = 128
  val clear_u_ctr = RegInit(0.U((log2Ceil(uBitPeriod) + log2Ceil(nRows) + 1).W))
  clear_u_ctr := clear_u_ctr + 1.U

  val doing_clear_u = clear_u_ctr(log2Ceil(uBitPeriod)-1,0) === 0.U
  val doing_clear_u_hi = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 1.U
  val doing_clear_u_lo = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 0.U
  val clear_u_idx = clear_u_ctr >> log2Ceil(uBitPeriod)

  // Use fetchpc to compute hash
  val update_wdata = Wire(new TageEntry)

  // val (update_idx, update_tag) =  compute_tag_and_hash(getUnhashedIdx(io.update.pc), io.update.hist, io.update.phist)
  val (update_idx, update_tag) =  compute_tag_and_hash(getUnhashedIdx(io.update.pc), io.update.folded_hist)

  table.io.w.apply(
    valid = io.update.mask,
    data = update_wdata,
    setIdx = update_idx,
    waymask = true.B
  )


  val update_hi_wdata = Wire(Bool())
  val update_lo_wdata = Wire(Bool())

  val hi_us_wen = io.update.uMask || doing_clear_u_hi
  val hi_us_wdata = Mux(doing_clear_u_hi, false.B, update_hi_wdata)
  val hi_us_setIdx = Mux(doing_clear_u_hi, clear_u_idx, update_idx)
  hi_us.io.w.apply(
    valid = hi_us_wen,
    data = hi_us_wdata,
    setIdx = hi_us_setIdx,
    waymask = true.B
  )

  val lo_us_wen = io.update.uMask || doing_clear_u_lo
  val lo_us_wdata = Mux(doing_clear_u_lo, false.B, update_lo_wdata)
  val lo_us_setIdx = Mux(doing_clear_u_lo, clear_u_idx, update_idx)
  lo_us.io.w.apply(
    valid = lo_us_wen,
    data = lo_us_wdata,
    setIdx = lo_us_setIdx,
    waymask = true.B
  )


  class WrBypass extends XSModule {
    val io = IO(new Bundle {
      val wen = Input(Bool())
      val update_idx  = Input(UInt(log2Ceil(nRows).W))
      val update_tag  = Input(UInt(tagLen.W))
      val update_ctr  = Input(UInt(TageCtrBits.W))

      val hit   = Output(Bool())
      val ctr  = Output(UInt(TageCtrBits.W))
    })

    val tags        = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(tagLen.W))))
    val idxes       = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(log2Ceil(nRows).W))))
    val ctrs        = RegInit(0.U.asTypeOf(Vec(wrBypassEntries, UInt(TageCtrBits.W))))
    val enq_idx     = RegInit(0.U(log2Ceil(wrBypassEntries).W))

    val hits = VecInit((0 until wrBypassEntries).map { i =>
      tags(i) === io.update_tag && idxes(i) === io.update_idx
    })

    val hit = hits.reduce(_||_)
    val hit_idx = ParallelPriorityEncoder(hits)

    io.hit := hit
    io.ctr := ctrs(hit_idx)

    when (io.wen) {
      when (hit) {
        ctrs(hit_idx) := io.update_ctr
      }.otherwise {
        ctrs(enq_idx) := io.update_ctr
      }
    }

    when(io.wen && !hit) {
      tags(enq_idx) := io.update_tag
      idxes(enq_idx) := io.update_idx
      enq_idx := (enq_idx + 1.U)(log2Ceil(wrBypassEntries)-1, 0)
    }
  }

  val wrbypass = Module(new WrBypass)

  wrbypass.io.wen := io.update.mask
  wrbypass.io.update_ctr := update_wdata.ctr

  update_wdata.ctr   := Mux(io.update.alloc,
    Mux(io.update.taken, 4.U,
                          3.U
    ),
    Mux(wrbypass.io.hit,
          inc_ctr(wrbypass.io.ctr, io.update.taken),
          inc_ctr(io.update.oldCtr, io.update.taken)
    )
  )
  update_wdata.valid := true.B
  update_wdata.tag   := update_tag

  update_hi_wdata    := io.update.u(1)
  update_lo_wdata    := io.update.u(0)

  wrbypass.io.update_idx := update_idx
  wrbypass.io.update_tag := update_tag



  XSPerfAccumulate(f"tage_table_wrbypass_hit", io.update.mask && wrbypass.io.hit)
  XSPerfAccumulate(f"tage_table_wrbypass_enq", io.update.mask && !wrbypass.io.hit)


  XSPerfAccumulate("tage_table_hits", PopCount(io.resp.valid))

  val u = io.update
  val b = PriorityEncoder(u.mask)
  val ub = PriorityEncoder(u.uMask)
  XSDebug(io.req.valid,
    p"tableReq: pc=0x${Hexadecimal(io.req.bits.pc)}, " +
    p"idx=$s0_idx, tag=$s0_tag\n")
  XSDebug(RegNext(io.req.valid) && req_rhit,
    p"TageTableResp: idx=$s1_idx, hit:$req_rhit, " +
    p"ctr:${io.resp.bits.ctr}, u:${io.resp.bits.u}\n")
  XSDebug(io.update.mask,
    p"update Table: pc:${Hexadecimal(u.pc)}}, " +
    p"taken:${u.taken}, alloc:${u.alloc}, oldCtr:${u.oldCtr}\n")
  XSDebug(io.update.mask,
    p"update Table: writing tag:$update_tag, " +
    p"ctr: ${update_wdata.ctr} in idx ${update_idx}\n")
  val hitCtr = wrbypass.io.ctr
  XSDebug(wrbypass.io.hit && io.update.mask,
    // p"bank $i wrbypass hit wridx:$wrbypass_hit_idx, idx:$update_idx, tag: $update_tag, " +
    p"ctr:$hitCtr, newCtr:${update_wdata.ctr}")

  XSDebug(RegNext(io.req.valid) && !req_rhit, "TageTableResp: not hit!\n")

  // ------------------------------Debug-------------------------------------
  val valids = Reg(Vec(nRows, Bool()))
  when (reset.asBool) { valids.foreach(r => r := false.B) }
  when (io.update.mask) { valids(update_idx) := true.B }
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
  io.s3_ready := true.B
}

@chiselName
class Tage(implicit p: Parameters) extends BaseTage {

  val resp_meta = Wire(MixedVec((0 until TageBanks).map(new TageMeta(_))))
  override val meta_size = resp_meta.getWidth
  val bank_tables = BankTageTableInfos.zipWithIndex.map {
    case (info, b) =>
      val tables = info.zipWithIndex.map {
        case ((nRows, histLen, tagLen), i) =>
          val t = Module(new TageTable(nRows, histLen, tagLen, UBitPeriod, i))
          t.io.req.valid := io.s0_fire
          t.io.req.bits.pc := s0_pc
          t.io.req.bits.folded_hist := io.in.bits.folded_hist
          t.io.req.bits.phist := io.in.bits.phist
          t
      }
      tables
  }
  val bt = Module (new TageBTable)
  bt.io.s0_fire := io.s0_fire
  bt.io.s0_pc   := s0_pc
  bt.io.update := io.update

  val tage_fh_info = bank_tables.flatMap(_.map(_.getFoldedHistoryInfo).reduce(_++_)).toSet
  override def getFoldedHistoryInfo = Some(tage_fh_info)
  // Keep the table responses to process in s3

  val s1_resps = MixedVecInit(bank_tables.map(b => VecInit(b.map(t => t.io.resp))))

  //val s1_bim = io.in.bits.resp_in(0).s1.preds
  // val s2_bim = RegEnable(s1_bim, enable=io.s1_fire)

  val debug_pc_s0 = s0_pc
  val debug_pc_s1 = RegEnable(s0_pc, enable=io.s0_fire)
  val debug_pc_s2 = RegEnable(debug_pc_s1, enable=io.s1_fire)
  val debug_pc_s3 = RegEnable(debug_pc_s2, enable=io.s2_fire)

  val s1_tageTakens    = Wire(Vec(TageBanks, Bool()))
  val s1_provideds     = Wire(Vec(TageBanks, Bool()))
  val s1_providers     = Wire(MixedVec(BankTageNTables.map(n=>UInt(log2Ceil(n).W))))
  val s1_finalAltPreds = Wire(Vec(TageBanks, Bool()))
  val s1_providerUs    = Wire(Vec(TageBanks, UInt(2.W)))
  val s1_providerCtrs  = Wire(Vec(TageBanks, UInt(TageCtrBits.W)))
  val s1_prednums      = Wire(MixedVec(BankTageNTables.map(n=>UInt(log2Ceil(n).W))))
  val s1_altprednums   = Wire(MixedVec(BankTageNTables.map(n=>UInt(log2Ceil(n).W))))
  val s1_predcnts      = Wire(Vec(TageBanks, UInt(TageCtrBits.W)))
  val s1_altpredcnts   = Wire(Vec(TageBanks, UInt(TageCtrBits.W)))
  val s1_altpredhits   = Wire(Vec(TageBanks, Bool()))
  val s1_basecnts      = Wire(Vec(TageBanks, UInt(2.W)))

  val s2_tageTakens    = RegEnable(s1_tageTakens, io.s1_fire)
  val s2_provideds     = RegEnable(s1_provideds, io.s1_fire)
  val s2_providers     = RegEnable(s1_providers, io.s1_fire)
  val s2_finalAltPreds = RegEnable(s1_finalAltPreds, io.s1_fire)
  val s2_providerUs    = RegEnable(s1_providerUs, io.s1_fire)
  val s2_providerCtrs  = RegEnable(s1_providerCtrs, io.s1_fire)
  val s2_prednums      = RegEnable(s1_prednums, io.s1_fire)
  val s2_altprednums   = RegEnable(s1_altprednums, io.s1_fire)
  val s2_predcnts      = RegEnable(s1_predcnts, io.s1_fire)
  val s2_altpredcnts   = RegEnable(s1_altpredcnts, io.s1_fire)
  val s2_altpredhits   = RegEnable(s1_altpredhits, io.s1_fire)
  val s2_basecnts      = RegEnable(s1_basecnts, io.s1_fire)

  io.out.resp := io.in.bits.resp_in(0)
  io.out.s3_meta := RegEnable(resp_meta.asUInt, io.s2_fire)

  // val ftb_hit = io.in.bits.resp_in(0).s2.preds.hit
  val ftb_entry = io.in.bits.resp_in(0).s2.ftb_entry
  val resp_s2 = io.out.resp.s2

  // Update logic
  val u_valid = io.update.valid
  val update = io.update.bits
  val updateValids = VecInit((0 until TageBanks).map(w =>
      update.ftb_entry.brValids(w) && u_valid && !update.ftb_entry.always_taken(w) &&
      !(PriorityEncoder(update.preds.br_taken_mask) < w.U)))
  val updatePhist = update.phist
  val updateFHist = update.folded_hist

  val updateMetas = update.meta.asTypeOf(MixedVec((0 until TageBanks).map(new TageMeta(_))))

  val updateMask    = WireInit(0.U.asTypeOf(MixedVec(BankTageNTables.map(Vec(_, Bool())))))
  val updateUMask   = WireInit(0.U.asTypeOf(MixedVec(BankTageNTables.map(Vec(_, Bool())))))
  val updateTaken   = Wire(MixedVec(BankTageNTables.map(Vec(_, Bool()))))
  val updateAlloc   = Wire(MixedVec(BankTageNTables.map(Vec(_, Bool()))))
  val updateOldCtr  = Wire(MixedVec(BankTageNTables.map(Vec(_, UInt(TageCtrBits.W)))))
  val updateU       = Wire(MixedVec(BankTageNTables.map(Vec(_, UInt(2.W)))))
  val updatebcnt    = Wire(Vec(TageBanks, UInt(2.W)))
  val baseupdate    = Wire(Vec(TageBanks,Bool()))
  updateTaken   := DontCare
  updateAlloc   := DontCare
  updateOldCtr  := DontCare
  updateU       := DontCare

  val updateMisPreds = update.mispred_mask

  // access tag tables and output meta info
  for (w <- 0 until TageBanks) {
    val s1_tageTaken     = WireInit(bt.io.s1_cnt(w)(1))
    var s1_altPred       = WireInit(bt.io.s1_cnt(w)(1))
    val s1_finalAltPred  = WireInit(bt.io.s1_cnt(w)(1))
    var s1_provided      = false.B
    var s1_provider      = 0.U
    var s1_altprednum    = 0.U
    var s1_altpredhit    = false.B
    var s1_prednum       = 0.U
    var s1_basecnt       = 0.U

    for (i <- 0 until BankTageNTables(w)) {
      val hit = s1_resps(w)(i).valid
      val ctr = s1_resps(w)(i).bits.ctr
      when (hit) {
        s1_tageTaken := Mux(ctr === 3.U || ctr === 4.U, s1_altPred, ctr(2)) // Use altpred on weak taken
        s1_finalAltPred := s1_altPred
      }
      s1_altpredhit = (s1_provided && hit) || s1_altpredhit        // Once hit then provide
      s1_provided = s1_provided || hit          // Once hit then provide
      s1_provider = Mux(hit, i.U, s1_provider)  // Use the last hit as provider
      s1_altPred = Mux(hit, ctr(2), s1_altPred) // Save current pred as potential altpred
      s1_altprednum = Mux(hit,s1_prednum,s1_altprednum)      // get altpredict table number
      s1_prednum = Mux(hit,i.U,s1_prednum)      // get predict table number
    }
    s1_provideds(w)      := s1_provided
    s1_basecnts(w)       := bt.io.s1_cnt(w)
    s1_providers(w)      := s1_provider
    s1_finalAltPreds(w)  := s1_finalAltPred
    s1_tageTakens(w)     := s1_tageTaken
    s1_providerUs(w)     := s1_resps(w)(s1_provider).bits.u
    s1_providerCtrs(w)   := s1_resps(w)(s1_provider).bits.ctr
    s1_prednums(w)       := s1_prednum
    s1_altprednums(w)    := s1_altprednum
    s1_predcnts(w)       := s1_resps(w)(s1_prednum).bits.ctr
    s1_altpredhits(w)    := s1_altpredhit
    s1_altpredcnts(w)    := s1_resps(w)(s1_altprednum).bits.ctr

    resp_meta(w).provider.valid   := s2_provideds(w)
    resp_meta(w).provider.bits    := s2_providers(w)
    resp_meta(w).prednum.valid    := s2_provideds(w)
    resp_meta(w).prednum.bits     := s2_prednums(w)
    resp_meta(w).altprednum.valid := s2_altpredhits(w)
    resp_meta(w).altprednum.bits  := s2_altprednums(w)
    resp_meta(w).altDiffers       := s2_finalAltPreds(w) =/= s2_tageTakens(w)
    resp_meta(w).providerU        := s2_providerUs(w)
    resp_meta(w).providerCtr      := s2_providerCtrs(w)
    resp_meta(w).predcnt          := s2_predcnts(w)
    resp_meta(w).altpredcnt       := s2_altpredcnts(w)
    resp_meta(w).altpredhit       := s2_altpredhits(w)
    resp_meta(w).taken            := s2_tageTakens(w)
    resp_meta(w).basecnt          := s2_basecnts(w)
    resp_meta(w).pred_cycle       := GTimer()

    // Create a mask fo tables which did not hit our query, and also contain useless entries
    // and also uses a longer history than the provider
    val allocatableSlots =
      RegEnable(
        VecInit(s1_resps(w).map(r => !r.valid && r.bits.u === 0.U)).asUInt &
          ~(LowerMask(UIntToOH(s1_provider), BankTageNTables(w)) &
            Fill(BankTageNTables(w), s1_provided.asUInt)),
        io.s1_fire
      )
    val allocLFSR   = LFSR64()(BankTageNTables(w) - 1, 0)
    val firstEntry  = PriorityEncoder(allocatableSlots)
    val maskedEntry = PriorityEncoder(allocatableSlots & allocLFSR)
    val allocEntry  = Mux(allocatableSlots(maskedEntry), maskedEntry, firstEntry)
    resp_meta(w).allocate.valid := allocatableSlots =/= 0.U
    resp_meta(w).allocate.bits  := allocEntry

    // Update in loop
    val updateValid = updateValids(w)
    val updateMeta = updateMetas(w)
    val isUpdateTaken = updateValid && update.preds.br_taken_mask(w)
    val updateMisPred = updateMisPreds(w)
    val up_altpredhit = updateMeta.altpredhit
    val up_prednum    = updateMeta.prednum.bits
    val up_altprednum = updateMeta.altprednum.bits
    when (updateValid) {
      when (updateMeta.provider.valid) {
        when (updateMisPred && up_altpredhit && (updateMeta.predcnt === 3.U || updateMeta.predcnt === 4.U)){
        updateMask(w)(up_altprednum)   := true.B
        updateUMask(w)(up_altprednum)  := false.B
        updateTaken(w)(up_altprednum)  := isUpdateTaken
        updateOldCtr(w)(up_altprednum) := updateMeta.altpredcnt
        updateAlloc(w)(up_altprednum)  := false.B

        }
        updateMask(w)(up_prednum)   := true.B
        updateUMask(w)(up_prednum)  := true.B

        updateU(w)(up_prednum) := // Mux((updateMeta.predcnt === 3.U || updateMeta.predcnt === 4.U), 0.U,
                                Mux(!updateMeta.altDiffers, updateMeta.providerU,
                                Mux(updateMisPred, Mux(updateMeta.providerU === 0.U, 0.U, updateMeta.providerU - 1.U),
                                Mux(updateMeta.providerU === 3.U, 3.U, updateMeta.providerU + 1.U))//)
        )
        updateTaken(w)(up_prednum)  := isUpdateTaken
        updateOldCtr(w)(up_prednum) := updateMeta.predcnt
        updateAlloc(w)(up_prednum)  := false.B
      }
    }

    // update base table if used base table to predict
    when (updateValid) {
      when(updateMeta.provider.valid) {
        when(~up_altpredhit && updateMisPred && (updateMeta.predcnt === 3.U || updateMeta.predcnt === 4.U)) {
        baseupdate(w) := true.B
        }
        .otherwise{
          baseupdate(w) := false.B
        }
      }
      .otherwise{
        baseupdate(w) := true.B
      }
    }
    .otherwise{
      baseupdate(w) := false.B
    }
    updatebcnt(w) := updateMeta.basecnt

    // if mispredicted and not the case that
    // provider offered correct target but used altpred due to unconfident
    when (updateValid && updateMisPred && ~((updateMeta.predcnt === 3.U && ~isUpdateTaken || updateMeta.predcnt === 4.U && isUpdateTaken) && updateMeta.provider.valid)) {
    //when (updateValid && updateMisPred) {
      val allocate = updateMeta.allocate
      when (allocate.valid) {
        updateMask(w)(allocate.bits)  := true.B
        updateTaken(w)(allocate.bits) := isUpdateTaken
        updateAlloc(w)(allocate.bits) := true.B
        updateUMask(w)(allocate.bits) := true.B
        updateU(w)(allocate.bits) := 0.U
      }.otherwise {

        val provider = updateMeta.provider
        val decrMask = Mux(provider.valid, ~LowerMask(UIntToOH(provider.bits), BankTageNTables(w)), 0.U(BankTageNTables(w).W))
        for (i <- 0 until BankTageNTables(w)) {
          when (decrMask(i)) {
            updateUMask(w)(i) := true.B
            updateU(w)(i) := 0.U
          }
        }
      }
    }
  }

  for (i <- 0 until numBr) {
    resp_s2.preds.br_taken_mask(i) := s2_tageTakens(i)
  }
  // io.out.resp.s3 := RegEnable(resp_s2, io.s2_fire)

  for (w <- 0 until TageBanks) {
    for (i <- 0 until BankTageNTables(w)) {
      bank_tables(w)(i).io.update.mask := RegNext(updateMask(w)(i))
      bank_tables(w)(i).io.update.taken := RegNext(updateTaken(w)(i))
      bank_tables(w)(i).io.update.alloc := RegNext(updateAlloc(w)(i))
      bank_tables(w)(i).io.update.oldCtr := RegNext(updateOldCtr(w)(i))

      bank_tables(w)(i).io.update.uMask := RegNext(updateUMask(w)(i))
      bank_tables(w)(i).io.update.u := RegNext(updateU(w)(i))
      bank_tables(w)(i).io.update.pc := RegNext(update.pc)
      // use fetch pc instead of instruction pc
      bank_tables(w)(i).io.update.folded_hist := RegNext(updateFHist)
      bank_tables(w)(i).io.update.phist := RegNext(updatePhist)
    }
  }
  bt.io.update  := RegNext(io.update)
  bt.io.update.valid := RegNext(baseupdate.reduce(_||_))
  bt.io.update_cnt := RegNext(updatebcnt)

  def pred_perf(name: String, cnt: UInt)   = XSPerfAccumulate(s"${name}_at_pred", cnt)
  def commit_perf(name: String, cnt: UInt) = XSPerfAccumulate(s"${name}_at_commit", cnt)
  def tage_perf(name: String, pred_cnt: UInt, commit_cnt: UInt) = {
    pred_perf(name, pred_cnt)
    commit_perf(name, commit_cnt)
  }

  // Debug and perf info
  for (b <- 0 until TageBanks) {
    for (i <- 0 until BankTageNTables(b)) {
      val pred_i_provided =
        s2_provideds(b) && s2_providers(b) === i.U
      val commit_i_provided =
        updateMetas(b).provider.valid && updateMetas(b).provider.bits === i.U && updateValids(b)
      tage_perf(
        s"bank_${b}_tage_table_${i}_provided",
        PopCount(pred_i_provided),
        PopCount(commit_i_provided)
      )
    }
    tage_perf(
      s"bank_${b}_tage_use_bim",
      PopCount(!s2_provideds(b)),
      PopCount(!updateMetas(b).provider.valid && updateValids(b))
    )
    def unconf(providerCtr: UInt) = providerCtr === 3.U || providerCtr === 4.U
    tage_perf(
      s"bank_${b}_tage_use_altpred",
      PopCount(s2_provideds(b) && unconf(s2_providerCtrs(b))),
      PopCount(updateMetas(b).provider.valid &&
        unconf(updateMetas(b).providerCtr) && updateValids(b))
    )
    tage_perf(
      s"bank_${b}_tage_provided",
      PopCount(s2_provideds(b)),
      PopCount(updateMetas(b).provider.valid && updateValids(b))
    )
  }

  for (b <- 0 until TageBanks) {
    val m = updateMetas(b)
    // val bri = u.metas(b)
    XSDebug(updateValids(b), "update(%d): pc=%x, cycle=%d, taken:%b, misPred:%d, bimctr:%d, pvdr(%d):%d, altDiff:%d, pvdrU:%d, pvdrCtr:%d, alloc(%d):%d\n",
      b.U, update.pc, 0.U, update.preds.br_taken_mask(b), update.mispred_mask(b),
      0.U, m.provider.valid, m.provider.bits, m.altDiffers, m.providerU, m.providerCtr, m.allocate.valid, m.allocate.bits
    )
  }
  val s2_resps = RegEnable(s1_resps, io.s1_fire)
  XSDebug("req: v=%d, pc=0x%x\n", io.s0_fire, s0_pc)
  XSDebug("s1_fire:%d, resp: pc=%x\n", io.s1_fire, debug_pc_s1)
  XSDebug("s2_fireOnLastCycle: resp: pc=%x, target=%x, hits=%b, takens=%b\n",
    debug_pc_s2, io.out.resp.s2.target, s2_provideds.asUInt, s2_tageTakens.asUInt)

  for (b <- 0 until TageBanks) {
    for (i <- 0 until BankTageNTables(b)) {
      XSDebug("bank(%d)_tage_table(%d): valid:%b, resp_ctr:%d, resp_us:%d\n",
        b.U, i.U, s2_resps(b)(i).valid, s2_resps(b)(i).bits.ctr, s2_resps(b)(i).bits.u)
    }
  }
    // XSDebug(io.update.valid && updateIsBr, p"update: sc: ${updateSCMeta}\n")
    // XSDebug(true.B, p"scThres: use(${useThreshold}), update(${updateThreshold})\n")
}


class Tage_SC(implicit p: Parameters) extends Tage with HasSC {}
