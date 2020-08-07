package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

import scala.math.min

trait HasTageParameter extends HasXSParameter{
  //                   Sets  Hist   Tag
  val TableInfo = Seq(( 128,    2,    7),
                      ( 128,    4,    7),
                      ( 256,    8,    8),
                      ( 256,   16,    8),
                      ( 128,   32,    9),
                      ( 128,   64,    9))
  val TageNTables = TableInfo.size
  val UBitPeriod = 2048
  val TageBanks = PredictWidth // FetchWidth

  val TotalBits = TableInfo.map {
    case (s, h, t) => {
      s * (1+t+3) * PredictWidth
    }
  }.reduce(_+_)
}

abstract class TageBundle extends XSBundle with HasTageParameter
abstract class TageModule extends XSModule with HasTageParameter




class TageReq extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
  val mask = UInt(PredictWidth.W)
}

class TageResp extends TageBundle {
  val ctr = UInt(3.W)
  val u = UInt(2.W)
}

class TageUpdate extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
  // update tag and ctr
  val mask = Vec(TageBanks, Bool())
  val taken = Vec(TageBanks, Bool())
  val alloc = Vec(TageBanks, Bool())
  val oldCtr = Vec(TageBanks, UInt(3.W))
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

  // bypass entries for tage update
  val wrBypassEntries = PredictWidth

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

  def inc_ctr(ctr: UInt, taken: Bool): UInt = {
    Mux(!taken, Mux(ctr === 0.U, 0.U, ctr - 1.U),
                Mux(ctr === 7.U, 7.U, ctr + 1.U))
  }
  // circular shifting
  def circularShiftLeft(source: UInt, len: Int, shamt: UInt): UInt = {
    val res = Wire(UInt(len.W))
    val higher = source << shamt
    val lower = source >> (len.U - shamt)
    res := higher | lower
    res
  }

  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nRows).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nRows-1).U) { doing_reset := false.B }

  class TageEntry() extends TageBundle {
    val valid = Bool()
    val tag = UInt(tagLen.W)
    val ctr = UInt(3.W)
  }

  val tageEntrySz = 1 + tagLen + 3

  // use real address to index
  val unhashed_idxes = VecInit((0 until TageBanks).map(b => ((io.req.bits.pc >> 1.U) + b.U) >> log2Up(TageBanks).U))

  val idxes_and_tags = (0 until TageBanks).map(b => compute_tag_and_hash(unhashed_idxes(b.U), io.req.bits.hist))
  val idxes = VecInit(idxes_and_tags.map(_._1))
  val tags = VecInit(idxes_and_tags.map(_._2))

  val idxLatch = RegEnable(idxes, enable=io.req.valid)
  val tagLatch = RegEnable(tags, enable=io.req.valid)

  val hi_us = List.fill(TageBanks)(Module(new SRAMTemplate(Bool(), set=nRows, shouldReset=false, holdRead=true, singlePort=false)))
  val lo_us = List.fill(TageBanks)(Module(new SRAMTemplate(Bool(), set=nRows, shouldReset=false, holdRead=true, singlePort=false)))
  val table = List.fill(TageBanks)(Module(new SRAMTemplate(new TageEntry, set=nRows, shouldReset=false, holdRead=true, singlePort=false)))

  val hi_us_r = WireInit(0.U.asTypeOf(Vec(TageBanks, Bool())))
  val lo_us_r = WireInit(0.U.asTypeOf(Vec(TageBanks, Bool())))
  val table_r = WireInit(0.U.asTypeOf(Vec(TageBanks, new TageEntry)))

  val baseBank = io.req.bits.pc(log2Up(TageBanks), 1)


  // This is different from that in BTB and BIM
  // We want to pass the correct index and tag into the TAGE table
  // if baseBank == 9, then we want to pass idxes_and_tags(0) to bank 9,
  //                         0  1        8  9  10      15
  // so the correct order is 7, 8, ..., 15, 0,  1, ..., 6
  val iAndTIdxInOrder = VecInit((0 until TageBanks).map(b => ((TageBanks.U +& b.U) - baseBank)(log2Up(TageBanks)-1, 0)))
  val iAndTIdxInOrderLatch = RegEnable(iAndTIdxInOrder, enable=io.req.valid)

  val realMask = circularShiftLeft(io.req.bits.mask, TageBanks, baseBank)
  val realMaskLatch = RegEnable(realMask, enable=io.req.valid)

  (0 until TageBanks).map(
    b => {
      hi_us(b).reset := reset.asBool
      lo_us(b).reset := reset.asBool
      table(b).reset := reset.asBool
      hi_us(b).io.r.req.valid := io.req.valid && realMask(b)
      lo_us(b).io.r.req.valid := io.req.valid && realMask(b)
      table(b).io.r.req.valid := io.req.valid && realMask(b)
      lo_us(b).io.r.req.bits.setIdx := idxes(iAndTIdxInOrder(b.U))
      hi_us(b).io.r.req.bits.setIdx := idxes(iAndTIdxInOrder(b.U))
      table(b).io.r.req.bits.setIdx := idxes(iAndTIdxInOrder(b.U))

      // Reorder done
      hi_us_r(iAndTIdxInOrderLatch(b)) := hi_us(b).io.r.resp.data(0)
      lo_us_r(iAndTIdxInOrderLatch(b)) := lo_us(b).io.r.resp.data(0)
      table_r(iAndTIdxInOrderLatch(b)) := table(b).io.r.resp.data(0)
    }
  )

  val req_rhits = VecInit((0 until TageBanks).map(b => table_r(b).valid && table_r(b).tag === tagLatch(b)))

  (0 until TageBanks).map(b => {
    io.resp(b).valid := req_rhits(b) && realMask(b)
    io.resp(b).bits.ctr := table_r(b).ctr
    io.resp(b).bits.u := Cat(hi_us_r(b),lo_us_r(b))
  })


  val clear_u_ctr = RegInit(0.U((log2Ceil(uBitPeriod) + log2Ceil(nRows) + 1).W))
  when (doing_reset) { clear_u_ctr := 1.U } .otherwise { clear_u_ctr := clear_u_ctr + 1.U }

  val doing_clear_u = clear_u_ctr(log2Ceil(uBitPeriod)-1,0) === 0.U
  val doing_clear_u_hi = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 1.U
  val doing_clear_u_lo = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 0.U
  val clear_u_idx = clear_u_ctr >> log2Ceil(uBitPeriod)

  val (update_idx, update_tag) = compute_tag_and_hash(io.update.pc >> (1.U + log2Up(TageBanks).U), io.update.hist)

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
  val wrbypass         = Reg(Vec(wrBypassEntries, Vec(TageBanks, UInt(3.W))))
  val wrbypass_enq_idx = RegInit(0.U(log2Ceil(wrBypassEntries).W))

  val wrbypass_hits    = VecInit((0 until wrBypassEntries) map { i =>
    !doing_reset &&
    wrbypass_tags(i) === update_tag &&
    wrbypass_idxs(i) === update_idx
  })
  val wrbypass_hit     = wrbypass_hits.reduce(_||_)
  val wrbypass_hit_idx = PriorityEncoder(wrbypass_hits)

  for (w <- 0 until TageBanks) {
    update_wdata(w).ctr   := Mux(io.update.alloc(w),
      Mux(io.update.taken(w), 4.U,
                              3.U
      ),
      Mux(wrbypass_hit,       inc_ctr(wrbypass(wrbypass_hit_idx)(w), io.update.taken(w)),
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
      wrbypass(wrbypass_hit_idx) := VecInit(update_wdata.map(_.ctr))
    } .otherwise {
      wrbypass     (wrbypass_enq_idx) := VecInit(update_wdata.map(_.ctr))
      wrbypass_tags(wrbypass_enq_idx) := update_tag
      wrbypass_idxs(wrbypass_enq_idx) := update_idx
      wrbypass_enq_idx := (wrbypass_enq_idx + 1.U)(log2Ceil(wrBypassEntries)-1,0)
    }
  }
  XSDebug(io.req.valid, "tableReq: pc=0x%x, hist=%b, base_idx=%d, base_tag=%x\n",
    io.req.bits.pc, io.req.bits.hist, idxes(0.U), tags(0.U))
  for (i <- 0 until TageBanks) {
    XSDebug(RegNext(io.req.valid), "TageTableResp[%d]: idx=%d, hit:%d, ctr:%d, u:%d\n", i.U, idxLatch(i), req_rhits(i), table_r(i).ctr, Cat(hi_us_r(i),lo_us_r(i)).asUInt)
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
      t.io.req.valid := io.pc.valid
      t.io.req.bits.pc := io.pc.bits
      t.io.req.bits.hist := io.hist
      t.io.req.bits.mask := io.inMask
      t
    }
  }

  // Keep the table responses to process in s3
  val resps = VecInit(tables.map(t => RegEnable(t.io.resp, enable=io.s3Fire)))

  val s2_bim = RegEnable(io.bim, enable=io.pc.valid) // actually it is s2Fire
  val s3_bim = RegEnable(s2_bim, enable=io.s3Fire)

  val debug_pc_s2 = RegEnable(io.pc.bits, enable=io.pc.valid)
  val debug_pc_s3 = RegEnable(debug_pc_s2, enable=io.s3Fire)

  val u = io.update.bits.ui
  val updateValid = io.update.valid
  val updateHist = io.update.bits.hist

  val updateMeta = u.brInfo.tageMeta
  val updateMisPred = u.isMisPred && u.pd.isBr

  val updateMask = WireInit(0.U.asTypeOf(Vec(TageNTables, Vec(TageBanks, Bool()))))
  val updateUMask = WireInit(0.U.asTypeOf(Vec(TageNTables, Vec(TageBanks, Bool()))))
  val updateTaken = Wire(Vec(TageNTables, Vec(TageBanks, Bool())))
  val updateAlloc = Wire(Vec(TageNTables, Vec(TageBanks, Bool())))
  val updateOldCtr = Wire(Vec(TageNTables, Vec(TageBanks, UInt(3.W))))
  val updateU = Wire(Vec(TageNTables, Vec(TageBanks, UInt(2.W))))
  updateTaken := DontCare
  updateAlloc := DontCare
  updateOldCtr := DontCare
  updateU := DontCare

  val updateBank = u.pc >> 1.U

  // access tag tables and output meta info
  for (w <- 0 until TageBanks) {
    var altPred = s3_bim.ctrs(w)(1)
    val finalAltPred = WireInit(s3_bim.ctrs(w)(1))
    var provided = false.B
    var provider = 0.U
    io.resp.hits(w) := false.B
    io.resp.takens(w) := s3_bim.ctrs(w)(1)

    for (i <- 0 until TageNTables) {
      val hit = resps(i)(w).valid
      io.resp.hits(w) := hit
      val ctr = resps(i)(w).bits.ctr
      when (hit) {
        io.resp.takens(w) := Mux(ctr === 3.U || ctr === 4.U, altPred, ctr(2)) // Use altpred on weak taken
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


    val isUpdateTaken = updateValid && updateBank === w.U &&
      u.taken && u.pd.isBr
    when (u.pd.isBr && updateValid && updateBank === w.U) {
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

  when (updateValid && updateMisPred) {
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
      val decrMask = Mux(provider.valid, ~LowerMask(UIntToOH(provider.bits), TageNTables), 0.U)
      for (i <- 0 until TageNTables) {
        when (decrMask(i)) {
          updateUMask(i)(idx) := true.B
          updateU(i)(idx) := 0.U
        }
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
  }



  val m = updateMeta
  XSDebug(io.pc.valid, "req: pc=0x%x, hist=%b\n", io.pc.bits, io.hist)
  XSDebug(io.update.valid, "redirect: provider(%d):%d, altDiffers:%d, providerU:%d, providerCtr:%d, allocate(%d):%d\n",
    m.provider.valid, m.provider.bits, m.altDiffers, m.providerU, m.providerCtr, m.allocate.valid, m.allocate.bits)
  XSDebug(true.B, "s3Fire:%d, resp: pc=%x, hits=%b, takens=%b\n",
    io.s3Fire, debug_pc_s3, io.resp.hits.asUInt, io.resp.takens.asUInt)
}