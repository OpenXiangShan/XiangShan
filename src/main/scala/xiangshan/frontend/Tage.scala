package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

import scala.math.min

trait HasTageParameter extends HasXSParameter with HasBPUParameter{
  //                   Sets  Hist   Tag
  val TableInfo = Seq(( 128,    2,    7),
                      ( 128,    4,    7),
                      ( 256,    8,    8),
                      ( 256,   16,    8),
                      ( 128,   32,    9),
                      ( 128,   64,    9))
                      // (  64,   64,   11),
                      // (  64,  101,   12),
                      // (  64,  160,   12),
                      // (  64,  254,   13),
                      // (  32,  403,   14),
                      // (  32,  640,   15))
  val TageNTables = TableInfo.size
  val UBitPeriod = 2048
  val TageBanks = PredictWidth // FetchWidth
  val TageCtrBits = 3
  val SCHistLens = 0 :: TableInfo.map{ case (_,h,_) => h}.toList
  val SCNTables = 6
  val SCCtrBits = 6
  val SCNRows = 1024
  val SCTableInfo = Seq.fill(SCNTables)((SCNRows, SCCtrBits)) zip SCHistLens map {case ((n, cb), h) => (n, cb, h)}
  val TotalBits = TableInfo.map {
    case (s, h, t) => {
      s * (1+t+TageCtrBits) * PredictWidth
    }
  }.reduce(_+_)
}

abstract class TageBundle extends XSBundle with HasTageParameter with PredictorUtils
abstract class TageModule extends XSModule with HasTageParameter with PredictorUtils { val debug = false }




class TageReq extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
  val mask = UInt(PredictWidth.W)
}

class TageResp extends TageBundle {
  val ctr = UInt(TageCtrBits.W)
  val u = UInt(2.W)
}

class TageUpdate extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val fetchIdx = UInt(log2Up(TageBanks).W)
  val hist = UInt(HistoryLength.W)
  // update tag and ctr
  val mask = Vec(TageBanks, Bool())
  val taken = Vec(TageBanks, Bool())
  val alloc = Vec(TageBanks, Bool())
  val oldCtr = Vec(TageBanks, UInt(TageCtrBits.W))
  // update u
  val uMask = Vec(TageBanks, Bool())
  val u = Vec(TageBanks, UInt(2.W))
}

class FakeTageTable() extends TageModule {
  val io = IO(new Bundle() {
    val req = Input(Valid(new TageReq))
    val resp = Output(Vec(TageBanks, Valid(new TageResp)))
    val update = Input(new TageUpdate)
  })
  io.resp := DontCare

}

class TageTable(val nRows: Int, val histLen: Int, val tagLen: Int, val uBitPeriod: Int) extends TageModule {
  val io = IO(new Bundle() {
    val req = Input(Valid(new TageReq))
    val resp = Output(Vec(TageBanks, Valid(new TageResp)))
    val update = Input(new TageUpdate)
  })
  // override val debug = true
  // bypass entries for tage update
  val wrBypassEntries = 8

  def compute_folded_hist(hist: UInt, l: Int) = {
    val nChunks = (histLen + l - 1) / l
    val hist_chunks = (0 until nChunks) map {i =>
      hist(min((i+1)*l, histLen)-1, i*l)
    }
    hist_chunks.reduce(_^_)
  }

  def compute_tag_and_hash(unhashed_idx: UInt, hist: UInt) = {
    val idx_history = compute_folded_hist(hist, log2Ceil(nRows))
    val idx = (unhashed_idx ^ idx_history)(log2Ceil(nRows)-1,0)
    val tag_history = compute_folded_hist(hist, tagLen)
    // Use another part of pc to make tags
    val tag = ((unhashed_idx >> log2Ceil(nRows)) ^ tag_history)(tagLen-1,0)
    (idx, tag)
  }

  def inc_ctr(ctr: UInt, taken: Bool): UInt = satUpdate(ctr, TageCtrBits, taken)

  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nRows).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nRows-1).U) { doing_reset := false.B }

  class TageEntry() extends TageBundle {
    val valid = Bool()
    val tag = UInt(tagLen.W)
    val ctr = UInt(TageCtrBits.W)
  }

  val tageEntrySz = 1 + tagLen + TageCtrBits

  // use real address to index
  // val unhashed_idxes = VecInit((0 until TageBanks).map(b => ((io.req.bits.pc >> 1.U) + b.U) >> log2Up(TageBanks).U))
  val unhashed_idx = io.req.bits.pc >> 1.U

  // val idxes_and_tags = (0 until TageBanks).map(b => compute_tag_and_hash(unhashed_idxes(b.U), io.req.bits.hist))
  val (idx, tag) = compute_tag_and_hash(unhashed_idx, io.req.bits.hist)
  // val idxes = VecInit(idxes_and_tags.map(_._1))
  // val tags = VecInit(idxes_and_tags.map(_._2))

  val idxLatch = RegEnable(idx, enable=io.req.valid)
  val tagLatch = RegEnable(tag, enable=io.req.valid)

  val hi_us = List.fill(TageBanks)(Module(new SRAMTemplate(Bool(), set=nRows, shouldReset=false, holdRead=true, singlePort=false)))
  val lo_us = List.fill(TageBanks)(Module(new SRAMTemplate(Bool(), set=nRows, shouldReset=false, holdRead=true, singlePort=false)))
  val table = List.fill(TageBanks)(Module(new SRAMTemplate(new TageEntry, set=nRows, shouldReset=false, holdRead=true, singlePort=false)))

  val hi_us_r = WireInit(0.U.asTypeOf(Vec(TageBanks, Bool())))
  val lo_us_r = WireInit(0.U.asTypeOf(Vec(TageBanks, Bool())))
  val table_r = WireInit(0.U.asTypeOf(Vec(TageBanks, new TageEntry)))

  val baseBank = io.req.bits.pc(log2Up(TageBanks), 1)
  val baseBankLatch = RegEnable(baseBank, enable=io.req.valid)

  val bankIdxInOrder = VecInit((0 until TageBanks).map(b => (baseBankLatch +& b.U)(log2Up(TageBanks)-1, 0)))

  val realMask = circularShiftLeft(io.req.bits.mask, TageBanks, baseBank)
  val maskLatch = RegEnable(io.req.bits.mask, enable=io.req.valid)

  (0 until TageBanks).map(
    b => {
      Seq(hi_us, lo_us, table).map(
        t => {
          t(b).reset := reset.asBool
          t(b).io.r.req.valid := io.req.valid && realMask(b)
          t(b).io.r.req.bits.setIdx := idx
        }
      )

      hi_us_r(b) := hi_us(b).io.r.resp.data(0)
      lo_us_r(b) := lo_us(b).io.r.resp.data(0)
      table_r(b) := table(b).io.r.resp.data(0)
    }
  )

  val req_rhits = VecInit((0 until TageBanks).map(b => table_r(bankIdxInOrder(b)).valid && table_r(bankIdxInOrder(b)).tag === tagLatch))

  (0 until TageBanks).map(b => {
    io.resp(b).valid := req_rhits(b) && maskLatch(b)
    io.resp(b).bits.ctr := table_r(bankIdxInOrder(b)).ctr
    io.resp(b).bits.u := Cat(hi_us_r(bankIdxInOrder(b)),lo_us_r(bankIdxInOrder(b)))
  })


  val clear_u_ctr = RegInit(0.U((log2Ceil(uBitPeriod) + log2Ceil(nRows) + 1).W))
  when (doing_reset) { clear_u_ctr := 1.U } .otherwise { clear_u_ctr := clear_u_ctr + 1.U }

  val doing_clear_u = clear_u_ctr(log2Ceil(uBitPeriod)-1,0) === 0.U
  val doing_clear_u_hi = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 1.U
  val doing_clear_u_lo = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 0.U
  val clear_u_idx = clear_u_ctr >> log2Ceil(uBitPeriod)

  // Use fetchpc to compute hash
  val (update_idx, update_tag) = compute_tag_and_hash((io.update.pc >> 1.U) - io.update.fetchIdx, io.update.hist)

  val update_wdata = Wire(Vec(TageBanks, new TageEntry))


  (0 until TageBanks).map(b => {
    table(b).io.w.req.valid := io.update.mask(b) || doing_reset
    table(b).io.w.req.bits.setIdx := Mux(doing_reset, reset_idx, update_idx)
    table(b).io.w.req.bits.data := Mux(doing_reset, 0.U.asTypeOf(new TageEntry), update_wdata(b))
  })

  val update_hi_wdata = Wire(Vec(TageBanks, Bool()))
  (0 until TageBanks).map(b => {
    hi_us(b).io.w.req.valid := io.update.uMask(b) || doing_reset || doing_clear_u_hi
    hi_us(b).io.w.req.bits.setIdx := Mux(doing_reset, reset_idx, Mux(doing_clear_u_hi, clear_u_idx, update_idx))
    hi_us(b).io.w.req.bits.data := Mux(doing_reset || doing_clear_u_hi, 0.U, update_hi_wdata(b))
  })

  val update_lo_wdata = Wire(Vec(TageBanks, Bool()))
  (0 until TageBanks).map(b => {
    lo_us(b).io.w.req.valid := io.update.uMask(b) || doing_reset || doing_clear_u_lo
    lo_us(b).io.w.req.bits.setIdx := Mux(doing_reset, reset_idx, Mux(doing_clear_u_lo, clear_u_idx, update_idx))
    lo_us(b).io.w.req.bits.data := Mux(doing_reset || doing_clear_u_lo, 0.U, update_lo_wdata(b))
  })

  val wrbypass_tags    = Reg(Vec(wrBypassEntries, UInt(tagLen.W)))
  val wrbypass_idxs    = Reg(Vec(wrBypassEntries, UInt(log2Ceil(nRows).W)))
  val wrbypass_ctrs    = Reg(Vec(wrBypassEntries, Vec(TageBanks, UInt(TageCtrBits.W))))
  val wrbypass_ctr_valids = Reg(Vec(wrBypassEntries, Vec(TageBanks, Bool())))
  val wrbypass_enq_idx = RegInit(0.U(log2Ceil(wrBypassEntries).W))

  when (reset.asBool) { wrbypass_ctr_valids.foreach(_.foreach(_ := false.B))}

  val wrbypass_hits    = VecInit((0 until wrBypassEntries) map { i =>
    !doing_reset &&
    wrbypass_tags(i) === update_tag &&
    wrbypass_idxs(i) === update_idx
  })

  val wrbypass_rhits   = VecInit((0 until wrBypassEntries) map { i =>
    io.req.valid &&
    wrbypass_tags(i) === tag &&
    wrbypass_idxs(i) === idx
  })

  val wrbypass_hit      = wrbypass_hits.reduce(_||_)
  val wrbypass_rhit     = wrbypass_rhits.reduce(_||_)
  val wrbypass_hit_idx  = PriorityEncoder(wrbypass_hits)
  val wrbypass_rhit_idx = PriorityEncoder(wrbypass_rhits)

  val wrbypass_rctr_hits = VecInit((0 until TageBanks).map( b => wrbypass_ctr_valids(wrbypass_rhit_idx)(b)))

  val rhit_ctrs = RegEnable(wrbypass_ctrs(wrbypass_rhit_idx), wrbypass_rhit)

  when (RegNext(wrbypass_rhit)) {
    for (b <- 0 until TageBanks) {
      when (RegNext(wrbypass_rctr_hits(b.U + baseBank))) {
        io.resp(b).bits.ctr := rhit_ctrs(bankIdxInOrder(b))
      }
    }
  }


  val updateBank = PriorityEncoder(io.update.mask)

  for (w <- 0 until TageBanks) {
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

    update_hi_wdata(w)    := io.update.u(w)(1)
    update_lo_wdata(w)    := io.update.u(w)(0)
  }

  when (io.update.mask.reduce(_||_)) {
    when (wrbypass_hits.reduce(_||_)) {
      wrbypass_ctrs(wrbypass_hit_idx)(updateBank) := update_wdata(updateBank).ctr
      wrbypass_ctr_valids(wrbypass_enq_idx)(updateBank) := true.B
    } .otherwise {
      wrbypass_ctrs(wrbypass_enq_idx)(updateBank) := update_wdata(updateBank).ctr
      (0 until TageBanks).foreach(b => wrbypass_ctr_valids(wrbypass_enq_idx)(b) := false.B) // reset valid bits
      wrbypass_ctr_valids(wrbypass_enq_idx)(updateBank) := true.B
      wrbypass_tags(wrbypass_enq_idx) := update_tag
      wrbypass_idxs(wrbypass_enq_idx) := update_idx
      wrbypass_enq_idx := (wrbypass_enq_idx + 1.U)(log2Ceil(wrBypassEntries)-1,0)
    }
  }

  if (BPUDebug && debug) {
    val u = io.update
    val b = PriorityEncoder(u.mask)
    val ub = PriorityEncoder(u.uMask)
    XSDebug(io.req.valid, "tableReq: pc=0x%x, hist=%x, idx=%d, tag=%x, baseBank=%d, mask=%b, realMask=%b\n",
      io.req.bits.pc, io.req.bits.hist, idx, tag, baseBank, io.req.bits.mask, realMask)
    for (i <- 0 until TageBanks) {
      XSDebug(RegNext(io.req.valid) && req_rhits(i), "TageTableResp[%d]: idx=%d, hit:%d, ctr:%d, u:%d\n", i.U, idxLatch, req_rhits(i), io.resp(i).bits.ctr, io.resp(i).bits.u)
    }

    XSDebug(RegNext(io.req.valid), "TageTableResp: hits:%b, maskLatch is %b\n", req_rhits.asUInt, maskLatch)
    XSDebug(RegNext(io.req.valid) && !req_rhits.reduce(_||_), "TageTableResp: no hits!\n")

    XSDebug(io.update.mask.reduce(_||_), "update Table: pc:%x, fetchIdx:%d, hist:%x, bank:%d, taken:%d, alloc:%d, oldCtr:%d\n",
      u.pc, u.fetchIdx, u.hist, b, u.taken(b), u.alloc(b), u.oldCtr(b))
    XSDebug(io.update.mask.reduce(_||_), "update Table: writing tag:%b, ctr%d in idx:%d\n",
      update_wdata(b).tag, update_wdata(b).ctr, update_idx)
    XSDebug(io.update.mask.reduce(_||_), "update u: pc:%x, fetchIdx:%d, hist:%x, bank:%d, writing in u:%b\n",
      u.pc, u.fetchIdx, u.hist, ub, io.update.u(ub))

    val updateBank = PriorityEncoder(io.update.mask)
    XSDebug(wrbypass_hit && wrbypass_ctr_valids(wrbypass_hit_idx)(updateBank),
      "wrbypass hits, wridx:%d, tag:%x, idx:%d, hitctr:%d, bank:%d\n",
      wrbypass_hit_idx, update_tag, update_idx, wrbypass_ctrs(wrbypass_hit_idx)(updateBank), updateBank)

    when (wrbypass_rhit && wrbypass_ctr_valids(wrbypass_rhit_idx).reduce(_||_)) {
      for (b <- 0 until TageBanks) {
        XSDebug(wrbypass_ctr_valids(wrbypass_rhit_idx)(b),
          "wrbypass rhits, wridx:%d, tag:%x, idx:%d, hitctr:%d, bank:%d\n",
          wrbypass_rhit_idx, tag, idx, wrbypass_ctrs(wrbypass_rhit_idx)(b), b.U)
      }
    }

    // ------------------------------Debug-------------------------------------
    val valids = Reg(Vec(TageBanks, Vec(nRows, Bool())))
    when (reset.asBool) { valids.foreach(b => b.foreach(r => r := false.B)) }
    (0 until TageBanks).map( b => { when (io.update.mask(b)) { valids(b)(update_idx) := true.B }})
    XSDebug("Table usage:------------------------\n")
    (0 until TageBanks).map( b => { XSDebug("Bank(%d): %d out of %d rows are valid\n", b.U, PopCount(valids(b)), nRows.U)})
  }

}

abstract class BaseTage extends BasePredictor with HasTageParameter {
  class TAGEResp extends Resp {
    val takens = Vec(PredictWidth, Bool())
    val hits = Vec(PredictWidth, Bool())
  }
  class TAGEMeta extends Meta{
  }
  class FromBIM extends FromOthers {
    val ctrs = Vec(PredictWidth, UInt(2.W))
  }
  class TageIO extends DefaultBasePredictorIO {
    val resp = Output(new TAGEResp)
    val meta = Output(Vec(PredictWidth, new TageMeta))
    val bim = Input(new FromBIM)
    val s3Fire = Input(Bool())
  }

  override val io = IO(new TageIO)
}

class FakeTage extends BaseTage {
  io.resp <> DontCare
  io.meta <> DontCare
}


class Tage extends BaseTage {

  val tables = TableInfo.map {
    case (nRows, histLen, tagLen) => {
      val t = if(EnableBPD) Module(new TageTable(nRows, histLen, tagLen, UBitPeriod)) else Module(new FakeTageTable)
      t.io.req.valid := io.pc.valid && !io.flush
      t.io.req.bits.pc := io.pc.bits
      t.io.req.bits.hist := io.hist
      t.io.req.bits.mask := io.inMask
      t
    }
  }

  val scTables = SCTableInfo.map {
    case (nRows, ctrBits, histLen) => {
      val t = if (EnableSC) Module(new SCTable(nRows/TageBanks, ctrBits, histLen)) else Module(new FakeSCTable)
      val req = t.io.req
      req.valid := io.pc.valid && !io.flush
      req.bits.pc := io.pc.bits
      req.bits.hist := io.hist
      req.bits.mask := io.inMask
      t
    }
  }

  val scThreshold = RegInit(SCThreshold(5))
  val useThreshold = WireInit(scThreshold.thres)
  val updateThreshold = WireInit((useThreshold << 3) + 21.U)

  // override val debug = true

  // Keep the table responses to process in s3
  val resps = VecInit(tables.map(t => RegEnable(t.io.resp, enable=io.s3Fire)))
  val scResps = VecInit(scTables.map(t => RegEnable(t.io.resp, enable=io.s3Fire)))
  // val flushLatch = RegNext(io.flush)

  val s2_bim = RegEnable(io.bim, enable=io.pc.valid) // actually it is s2Fire
  val s3_bim = RegEnable(s2_bim, enable=io.s3Fire)

  val debug_pc_s2 = RegEnable(io.pc.bits, enable=io.pc.valid)
  val debug_pc_s3 = RegEnable(debug_pc_s2, enable=io.s3Fire)

  val debug_hist_s2 = RegEnable(io.hist, enable=io.pc.valid)
  val debug_hist_s3 = RegEnable(debug_hist_s2, enable=io.s3Fire)

  val u = io.update.bits.ui
  val updateValid = io.update.valid
  val updateHist = io.update.bits.hist

  val updateIsBr = u.pd.isBr
  val updateMeta = u.brInfo.tageMeta
  val updateMisPred = u.isMisPred && updateIsBr

  val updateMask = WireInit(0.U.asTypeOf(Vec(TageNTables, Vec(TageBanks, Bool()))))
  val updateUMask = WireInit(0.U.asTypeOf(Vec(TageNTables, Vec(TageBanks, Bool()))))
  val updateTaken = Wire(Vec(TageNTables, Vec(TageBanks, Bool())))
  val updateAlloc = Wire(Vec(TageNTables, Vec(TageBanks, Bool())))
  val updateOldCtr = Wire(Vec(TageNTables, Vec(TageBanks, UInt(TageCtrBits.W))))
  val updateU = Wire(Vec(TageNTables, Vec(TageBanks, UInt(2.W))))
  updateTaken := DontCare
  updateAlloc := DontCare
  updateOldCtr := DontCare
  updateU := DontCare

  val scUpdateMask = WireInit(0.U.asTypeOf(Vec(SCNTables, Vec(TageBanks, Bool()))))
  val scUpdateTagePred = Wire(Bool())
  val scUpdateTaken = Wire(Bool())
  val scUpdateOldCtrs = Wire(Vec(SCNTables, SInt(SCCtrBits.W)))
  scUpdateTagePred := DontCare
  scUpdateTaken := DontCare
  scUpdateOldCtrs := DontCare

  val updateSCMeta = u.brInfo.tageMeta.scMeta
  val updateTageMisPred = updateMeta.taken =/= u.taken && updateIsBr

  val updateBank = u.pc(log2Ceil(TageBanks), 1)

  // access tag tables and output meta info
  for (w <- 0 until TageBanks) {
    val tageTaken = WireInit(s3_bim.ctrs(w)(1).asBool)
    var altPred = s3_bim.ctrs(w)(1)
    val finalAltPred = WireInit(s3_bim.ctrs(w)(1))
    var provided = false.B
    var provider = 0.U
    io.resp.takens(w) := s3_bim.ctrs(w)(1)

    for (i <- 0 until TageNTables) {
      val hit = resps(i)(w).valid
      val ctr = resps(i)(w).bits.ctr
      when (hit) {
        io.resp.takens(w) := Mux(ctr === 3.U || ctr === 4.U, altPred, ctr(2)) // Use altpred on weak taken
        tageTaken := Mux(ctr === 3.U || ctr === 4.U, altPred, ctr(2))
        finalAltPred := altPred
      }
      provided = provided || hit          // Once hit then provide
      provider = Mux(hit, i.U, provider)  // Use the last hit as provider
      altPred = Mux(hit, ctr(2), altPred) // Save current pred as potential altpred
    }
    io.resp.hits(w) := provided
    io.meta(w).provider.valid := provided
    io.meta(w).provider.bits := provider
    io.meta(w).altDiffers := finalAltPred =/= io.resp.takens(w)
    io.meta(w).providerU := resps(provider)(w).bits.u
    io.meta(w).providerCtr := resps(provider)(w).bits.ctr
    io.meta(w).taken := tageTaken

    // Create a mask fo tables which did not hit our query, and also contain useless entries
    // and also uses a longer history than the provider
    val allocatableSlots = (VecInit(resps.map(r => !r(w).valid && r(w).bits.u === 0.U)).asUInt &
      ~(LowerMask(UIntToOH(provider), TageNTables) & Fill(TageNTables, provided.asUInt))
    )
    val allocLFSR = LFSR64()(TageNTables - 1, 0)
    val firstEntry = PriorityEncoder(allocatableSlots)
    val maskedEntry = PriorityEncoder(allocatableSlots & allocLFSR)
    val allocEntry = Mux(allocatableSlots(maskedEntry), maskedEntry, firstEntry)
    io.meta(w).allocate.valid := allocatableSlots =/= 0.U
    io.meta(w).allocate.bits := allocEntry

    val scMeta = io.meta(w).scMeta
    scMeta := DontCare
    val scTableSums = VecInit(
      (0 to 1) map { i => {
          // val providerCtr = resps(provider)(w).bits.ctr.zext()
          // val pvdrCtrCentered = (((providerCtr - 4.S) << 1) + 1.S) << 3
          // sum += pvdrCtrCentered
          if (EnableSC) {
            (0 until SCNTables) map { j => 
              scTables(j).getCenteredValue(scResps(j)(w).ctr(i))
            } reduce (_+_) // TODO: rewrite with adder tree
          }
          else 0.S
        }
      }
    )

    if (EnableSC) {
      scMeta.tageTaken := tageTaken
      scMeta.scUsed := provided
      scMeta.scPred := tageTaken
      scMeta.sumAbs := 0.U
      when (provided) {
        val providerCtr = resps(provider)(w).bits.ctr.zext()
        val pvdrCtrCentered = ((((providerCtr - 4.S) << 1).asSInt + 1.S) << 3).asSInt
        val totalSum = scTableSums(tageTaken.asUInt) + pvdrCtrCentered
        val sumAbs = totalSum.abs().asUInt
        val sumBelowThreshold = totalSum.abs.asUInt < useThreshold
        val scPred = totalSum >= 0.S
        scMeta.sumAbs := sumAbs
        scMeta.ctrs   := VecInit(scResps.map(r => r(w).ctr(tageTaken.asUInt)))
        for (i <- 0 until SCNTables) {
          XSDebug(RegNext(io.s3Fire), p"SCTable(${i.U})(${w.U}): ctr:(${scResps(i)(w).ctr(0)},${scResps(i)(w).ctr(1)})\n")
        }
        XSDebug(RegNext(io.s3Fire), p"SC(${w.U}): pvdCtr(${providerCtr}), pvdCentred(${pvdrCtrCentered}), totalSum(${totalSum}), abs(${sumAbs}) useThres(${useThreshold}), scPred(${scPred})\n")
        // Use prediction from Statistical Corrector
        when (!sumBelowThreshold) {
          XSDebug(RegNext(io.s3Fire), p"SC(${w.U}) overriden pred to ${scPred}\n")
          scMeta.scPred := scPred
          io.resp.takens(w) := scPred
        }
      }
    }

    val isUpdateTaken = updateValid && updateBank === w.U &&
      u.taken && updateIsBr
    when (updateIsBr && updateValid && updateBank === w.U) {
      when (updateMeta.provider.valid) {
        val provider = updateMeta.provider.bits

        updateMask(provider)(w) := true.B
        updateUMask(provider)(w) := true.B

        updateU(provider)(w) := Mux(!updateMeta.altDiffers, updateMeta.providerU,
          Mux(updateMisPred, Mux(updateMeta.providerU === 0.U, 0.U, updateMeta.providerU - 1.U),
                              Mux(updateMeta.providerU === 3.U, 3.U, updateMeta.providerU + 1.U))
        )
        updateTaken(provider)(w) := isUpdateTaken
        updateOldCtr(provider)(w) := updateMeta.providerCtr
        updateAlloc(provider)(w) := false.B
      }
    }
  }

  when (updateValid && updateTageMisPred) {
    val idx = updateBank
    val allocate = updateMeta.allocate
    when (allocate.valid) {
      updateMask(allocate.bits)(idx) := true.B
      updateTaken(allocate.bits)(idx) := u.taken
      updateAlloc(allocate.bits)(idx) := true.B
      updateUMask(allocate.bits)(idx) := true.B
      updateU(allocate.bits)(idx) := 0.U
    }.otherwise {
      val provider = updateMeta.provider
      val decrMask = Mux(provider.valid, ~LowerMask(UIntToOH(provider.bits), TageNTables), 0.U(TageNTables.W))
      for (i <- 0 until TageNTables) {
        when (decrMask(i)) {
          updateUMask(i)(idx) := true.B
          updateU(i)(idx) := 0.U
        }
      }
    }
  }

  if (EnableSC) {
    when (updateValid && updateSCMeta.scUsed.asBool && updateIsBr) {
      val scPred = updateSCMeta.scPred
      val tageTaken = updateSCMeta.tageTaken
      val sumAbs = updateSCMeta.sumAbs.asUInt
      val scOldCtrs = updateSCMeta.ctrs
      when (scPred =/= tageTaken && sumAbs < useThreshold - 2.U) {
        val newThres = scThreshold.update(scPred =/= u.taken)
        scThreshold := newThres
        XSDebug(p"scThres update: old d${useThreshold} --> new ${newThres.thres}\n")
      }
      when (scPred =/= u.taken || sumAbs < updateThreshold) {
        scUpdateMask.foreach(t => t(updateBank) := true.B)
        scUpdateTagePred := tageTaken
        scUpdateTaken := u.taken
        (scUpdateOldCtrs zip scOldCtrs).foreach{case (t, c) => t := c}
        XSDebug(p"scUpdate: bank(${updateBank}), scPred(${scPred}), tageTaken(${tageTaken}), scSumAbs(${sumAbs}), mispred: sc(${updateMisPred}), tage(${updateTageMisPred})\n")
        XSDebug(p"update: sc: ${updateSCMeta}\n")
      }
    }
  }

  for (i <- 0 until TageNTables) {
    for (w <- 0 until TageBanks) {
      tables(i).io.update.mask(w) := updateMask(i)(w)
      tables(i).io.update.taken(w) := updateTaken(i)(w)
      tables(i).io.update.alloc(w) := updateAlloc(i)(w)
      tables(i).io.update.oldCtr(w) := updateOldCtr(i)(w)

      tables(i).io.update.uMask(w) := updateUMask(i)(w)
      tables(i).io.update.u(w) := updateU(i)(w)
    }
    // use fetch pc instead of instruction pc
    tables(i).io.update.pc := u.pc
    tables(i).io.update.hist := updateHist
    tables(i).io.update.fetchIdx := u.brInfo.fetchIdx
  }

  for (i <- 0 until SCNTables) {
    scTables(i).io.update.mask := scUpdateMask(i)
    scTables(i).io.update.tagePred := scUpdateTagePred
    scTables(i).io.update.taken    := scUpdateTaken
    scTables(i).io.update.oldCtr   := scUpdateOldCtrs(i)
    scTables(i).io.update.pc := u.pc
    scTables(i).io.update.hist := updateHist
    scTables(i).io.update.fetchIdx := u.brInfo.fetchIdx
  }



  if (BPUDebug && debug) {
    val m = updateMeta
    val bri = u.brInfo
    XSDebug(io.pc.valid, "req: pc=0x%x, hist=%x\n", io.pc.bits, io.hist)
    XSDebug(io.s3Fire, "s3Fire:%d, resp: pc=%x, hist=%x\n", io.s3Fire, debug_pc_s2, debug_hist_s2)
    XSDebug(RegNext(io.s3Fire), "s3FireOnLastCycle: resp: pc=%x, hist=%x, hits=%b, takens=%b\n",
      debug_pc_s3, debug_hist_s3, io.resp.hits.asUInt, io.resp.takens.asUInt)
    for (i <- 0 until TageNTables) {
      XSDebug(RegNext(io.s3Fire), "TageTable(%d): valids:%b, resp_ctrs:%b, resp_us:%b\n", i.U, VecInit(resps(i).map(_.valid)).asUInt, Cat(resps(i).map(_.bits.ctr)), Cat(resps(i).map(_.bits.u)))
    }
    XSDebug(io.update.valid, "update: pc=%x, fetchpc=%x, cycle=%d, hist=%x, taken:%d, misPred:%d, histPtr:%d, bimctr:%d, pvdr(%d):%d, altDiff:%d, pvdrU:%d, pvdrCtr:%d, alloc(%d):%d\n",
      u.pc, u.pc - (bri.fetchIdx << 1.U), bri.debug_tage_cycle,  updateHist, u.taken, u.isMisPred, bri.histPtr, bri.bimCtr, m.provider.valid, m.provider.bits, m.altDiffers, m.providerU, m.providerCtr, m.allocate.valid, m.allocate.bits)
    XSDebug(io.update.valid && updateIsBr, p"update: sc: ${updateSCMeta}\n")
    XSDebug(true.B, p"scThres: use(${useThreshold}), update(${updateThreshold})\n")
  }
}