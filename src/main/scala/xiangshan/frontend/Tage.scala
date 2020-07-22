package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

import scala.math.min

trait HasTageParameter {
  //                   Sets  Hist   Tag
  val TableInfo = Seq(( 128,    2,    7),
                      ( 128,    4,    7),
                      ( 256,    8,    8),
                      ( 256,   16,    8),
                      ( 128,   32,    9),
                      ( 128,   64,    9))
  val TageNTables = TableInfo.size
  val UBitPeriod = 2048
  val BankWidth = 8 // FetchWidth

  val TotalBits = TableInfo.map {
    case (s, h, t) => {
      s * (1+t+3) * BankWidth
    }
  }.reduce(_+_)
}

abstract class TageBundle extends XSBundle with HasTageParameter
abstract class TageModule extends XSModule with HasTageParameter

class TageReq extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
}

class TageResp extends TageBundle {
  val ctr = UInt(3.W)
  val u = UInt(2.W)
}

class TageUpdate extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
  // update tag and ctr
  val mask = Vec(BankWidth, Bool())
  val taken = Vec(BankWidth, Bool())
  val alloc = Vec(BankWidth, Bool())
  val oldCtr = Vec(BankWidth, UInt(3.W))
  // update u
  val uMask = Vec(BankWidth, Bool())
  val u = Vec(BankWidth, UInt(2.W))
}

class FakeTageTable() extends TageModule {
  val io = IO(new Bundle() {
    val req = Input(Valid(new TageReq))
    val resp = Output(Vec(BankWidth, Valid(new TageResp)))
    val update = Input(new TageUpdate)
  })
  io.resp := DontCare

}

class TageTable(val nRows: Int, val histLen: Int, val tagLen: Int, val uBitPeriod: Int) extends TageModule {
  val io = IO(new Bundle() {
    val req = Input(Valid(new TageReq))
    val resp = Output(Vec(BankWidth, Valid(new TageResp)))
    val update = Input(new TageUpdate)
  })

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

  def inc_ctr(ctr: UInt, taken: Bool): UInt = {
    Mux(!taken, Mux(ctr === 0.U, 0.U, ctr - 1.U),
                Mux(ctr === 7.U, 7.U, ctr + 1.U))
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

  val (hashed_idx, tag) = compute_tag_and_hash(io.req.bits.pc >> (2 + log2Ceil(FetchWidth)), io.req.bits.hist)

  val hi_us = List.fill(BankWidth)(Module(new SRAMTemplate(Bool(), set=nRows, shouldReset=false, holdRead=true, singlePort=false)))
  val lo_us = List.fill(BankWidth)(Module(new SRAMTemplate(Bool(), set=nRows, shouldReset=false, holdRead=true, singlePort=false)))
  val table = List.fill(BankWidth)(Module(new SRAMTemplate(new TageEntry, set=nRows, shouldReset=false, holdRead=true, singlePort=false)))

  val hi_us_r = Wire(Vec(BankWidth, Bool()))
  val lo_us_r = Wire(Vec(BankWidth, Bool()))
  val table_r = Wire(Vec(BankWidth, new TageEntry))

  (0 until BankWidth).map(
    b => {
      hi_us(b).reset := reset.asBool
      lo_us(b).reset := reset.asBool
      table(b).reset := reset.asBool
      hi_us(b).io.r.req.valid := io.req.valid
      lo_us(b).io.r.req.valid := io.req.valid
      table(b).io.r.req.valid := io.req.valid
      hi_us(b).io.r.req.bits.setIdx := hashed_idx
      lo_us(b).io.r.req.bits.setIdx := hashed_idx
      table(b).io.r.req.bits.setIdx := hashed_idx
      
      hi_us_r(b) := hi_us(b).io.r.resp.data(0)
      lo_us_r(b) := lo_us(b).io.r.resp.data(0)
      table_r(b) := table(b).io.r.resp.data(0)

      // io.resp(b).valid := table_r(b).valid && table_r(b).tag === tag // Missing reset logic
      // io.resp(b).bits.ctr := table_r(b).ctr
      // io.resp(b).bits.u := Cat(hi_us_r(b),lo_us_r(b))
    }
  )

  val req_rhits = VecInit(table_r.map(e => e.valid && e.tag === tag && !doing_reset))

  (0 until BankWidth).map(b => {
    io.resp(b).valid := req_rhits(b)
    io.resp(b).bits.ctr := table_r(b).ctr
    io.resp(b).bits.u := Cat(hi_us_r(b),lo_us_r(b))
  })


  val clear_u_ctr = RegInit(0.U((log2Ceil(uBitPeriod) + log2Ceil(nRows) + 1).W))
  when (doing_reset) { clear_u_ctr := 1.U } .otherwise { clear_u_ctr := clear_u_ctr + 1.U }

  val doing_clear_u = clear_u_ctr(log2Ceil(uBitPeriod)-1,0) === 0.U
  val doing_clear_u_hi = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 1.U
  val doing_clear_u_lo = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 0.U
  val clear_u_idx = clear_u_ctr >> log2Ceil(uBitPeriod)

  val (update_idx, update_tag) = compute_tag_and_hash(io.update.pc, io.update.hist)

  val update_wdata = Wire(Vec(BankWidth, new TageEntry))

  
  (0 until BankWidth).map(b => {
    table(b).io.w.req.valid := io.update.mask(b) || doing_reset
    table(b).io.w.req.bits.setIdx := Mux(doing_reset, reset_idx, update_idx)
    table(b).io.w.req.bits.data := Mux(doing_reset, 0.U.asTypeOf(new TageEntry), update_wdata(b))
  })

  val update_hi_wdata = Wire(Vec(BankWidth, Bool()))
  (0 until BankWidth).map(b => {
    hi_us(b).io.w.req.valid := io.update.uMask(b) || doing_reset || doing_clear_u_hi
    hi_us(b).io.w.req.bits.setIdx := Mux(doing_reset, reset_idx, Mux(doing_clear_u_hi, clear_u_idx, update_idx))
    hi_us(b).io.w.req.bits.data := Mux(doing_reset || doing_clear_u_hi, 0.U, update_hi_wdata(b))
  })

  val update_lo_wdata = Wire(Vec(BankWidth, Bool()))
  (0 until BankWidth).map(b => {
    lo_us(b).io.w.req.valid := io.update.uMask(b) || doing_reset || doing_clear_u_lo
    lo_us(b).io.w.req.bits.setIdx := Mux(doing_reset, reset_idx, Mux(doing_clear_u_lo, clear_u_idx, update_idx))
    lo_us(b).io.w.req.bits.data := Mux(doing_reset || doing_clear_u_lo, 0.U, update_lo_wdata(b))
  })

  val wrbypass_tags    = Reg(Vec(wrBypassEntries, UInt(tagLen.W)))
  val wrbypass_idxs    = Reg(Vec(wrBypassEntries, UInt(log2Ceil(nRows).W)))
  val wrbypass         = Reg(Vec(wrBypassEntries, Vec(BankWidth, UInt(3.W))))
  val wrbypass_enq_idx = RegInit(0.U(log2Ceil(wrBypassEntries).W))

  val wrbypass_hits    = VecInit((0 until wrBypassEntries) map { i =>
    !doing_reset &&
    wrbypass_tags(i) === update_tag &&
    wrbypass_idxs(i) === update_idx
  })
  val wrbypass_hit     = wrbypass_hits.reduce(_||_)
  val wrbypass_hit_idx = PriorityEncoder(wrbypass_hits)

  for (w <- 0 until BankWidth) {
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

}

class FakeTAGE extends TageModule {
  val io = IO(new Bundle() {
    val req = Input(Valid(new TageReq))
    val out = new Bundle {
      val hits = Output(UInt(FetchWidth.W))
      val takens = Output(Vec(FetchWidth, Bool()))
    }
    val meta = Output(Vec(FetchWidth, (new TageMeta)))
    val redirectInfo = Input(new RedirectInfo)
  })
  
  io.out.hits := 0.U(FetchWidth.W)
  io.out.takens := DontCare
  io.meta := DontCare
}


class Tage extends TageModule {
  val io = IO(new Bundle() {
    val req = Input(Valid(new TageReq))
    val out = new Bundle {
      val hits = Output(UInt(FetchWidth.W))
      val takens = Output(Vec(FetchWidth, Bool()))
    }
    val meta = Output(Vec(FetchWidth, (new TageMeta)))
    val redirectInfo = Input(new RedirectInfo)
  })

  val tables = TableInfo.map {
    case (nRows, histLen, tagLen) => {
      val t = if(EnableBPD) Module(new TageTable(nRows, histLen, tagLen, UBitPeriod)) else Module(new FakeTageTable)
      t.io.req <> io.req
      t
    }
  }
  val resps = VecInit(tables.map(_.io.resp))

  val updateMeta = io.redirectInfo.redirect.tageMeta
  //val updateMisPred = UIntToOH(io.redirectInfo.redirect.fetchIdx) &
  //  Fill(FetchWidth, (io.redirectInfo.misPred && io.redirectInfo.redirect.btbType === BTBtype.B).asUInt)
  val updateMisPred = io.redirectInfo.misPred && io.redirectInfo.redirect.btbType === BTBtype.B

  val updateMask = WireInit(0.U.asTypeOf(Vec(TageNTables, Vec(BankWidth, Bool()))))
  val updateUMask = WireInit(0.U.asTypeOf(Vec(TageNTables, Vec(BankWidth, Bool()))))
  val updateTaken = Wire(Vec(TageNTables, Vec(BankWidth, Bool())))
  val updateAlloc = Wire(Vec(TageNTables, Vec(BankWidth, Bool())))
  val updateOldCtr = Wire(Vec(TageNTables, Vec(BankWidth, UInt(3.W))))
  val updateU = Wire(Vec(TageNTables, Vec(BankWidth, UInt(2.W))))
  updateTaken := DontCare
  updateAlloc := DontCare
  updateOldCtr := DontCare
  updateU := DontCare

  // access tag tables and output meta info
  val outHits = Wire(Vec(FetchWidth, Bool()))
  for (w <- 0 until BankWidth) {
    var altPred = false.B
    val finalAltPred = WireInit(false.B)
    var provided = false.B
    var provider = 0.U
    outHits(w) := false.B
    io.out.takens(w) := false.B

    for (i <- 0 until TageNTables) {
      val hit = resps(i)(w).valid
      val ctr = resps(i)(w).bits.ctr
      when (hit) {
        io.out.takens(w) := Mux(ctr === 3.U || ctr === 4.U, altPred, ctr(2)) // Use altpred on weak taken
        finalAltPred := altPred
      }
      provided = provided || hit          // Once hit then provide
      provider = Mux(hit, i.U, provider)  // Use the last hit as provider
      altPred = Mux(hit, ctr(2), altPred) // Save current pred as potential altpred
    }
    outHits(w) := provided
    io.meta(w).provider.valid := provided
    io.meta(w).provider.bits := provider
    io.meta(w).altDiffers := finalAltPred =/= io.out.takens(w)
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

    val isUpdateTaken = io.redirectInfo.valid && io.redirectInfo.redirect.fetchIdx === w.U &&
      io.redirectInfo.redirect.taken && io.redirectInfo.redirect.btbType === BTBtype.B
    when (io.redirectInfo.redirect.btbType === BTBtype.B && io.redirectInfo.valid &&  io.redirectInfo.redirect.fetchIdx === w.U) {
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

  when (io.redirectInfo.valid && updateMisPred) {
    val idx = io.redirectInfo.redirect.fetchIdx
    val allocate = updateMeta.allocate
    when (allocate.valid) {
      updateMask(allocate.bits)(idx) := true.B
      updateTaken(allocate.bits)(idx) := io.redirectInfo.redirect.taken
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
    for (w <- 0 until BankWidth) {
      tables(i).io.update.mask(w) := updateMask(i)(w)
      tables(i).io.update.taken(w) := updateTaken(i)(w)
      tables(i).io.update.alloc(w) := updateAlloc(i)(w)
      tables(i).io.update.oldCtr(w) := updateOldCtr(i)(w)
      
      tables(i).io.update.uMask(w) := updateUMask(i)(w)
      tables(i).io.update.u(w) := updateU(i)(w)
    }
    // use fetch pc instead of instruction pc
    tables(i).io.update.pc := io.redirectInfo.redirect.pc - (io.redirectInfo.redirect.fetchIdx << 2.U)
    tables(i).io.update.hist := io.redirectInfo.redirect.hist
  }

  io.out.hits := outHits.asUInt

  XSDebug(io.req.valid, "req: pc=0x%x, hist=%b\n", io.req.bits.pc, io.req.bits.hist)
  val m = updateMeta
  XSDebug(io.redirectInfo.valid, "redirect: provider(%d):%d, altDiffers:%d, providerU:%d, providerCtr:%d, allocate(%d):%d\n", m.provider.valid, m.provider.bits, m.altDiffers, m.providerU, m.providerCtr, m.allocate.valid, m.allocate.bits)

}