package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

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

class TageTable(val nRows: Int, val histLen: Int, val tagLen: Int, val uBitPeriod: Int) extends TageModule {
  val io = IO(new Bundle() {
    val req = Input(Valid(new TageReq))
    val resp = Output(Vec(BankWidth, Valid(new TageResp)))
    val update = Input(new TageUpdate)
  })

  // bypass entries for tage update
  val wrBypassEntries = 4

}

class Tage extends TageModule {
  val io = IO(new Bundle() {
    val req = Input(Valid(new TageReq))
    val out = new Bundle {
      val hits = Output(UInt(FetchWidth.W))
      val takens = Output(Vec(FetchWidth, Bool()))
    }
    val meta = Output(Vec(FetchWidth, (new TageMeta)))
    val redirectInfo = Flipped(new RedirectInfo)
  })

  val tables = TableInfo.map {
    case (nRows, histLen, tagLen) => {
      val t = Module(new TageTable(nRows, histLen, tagLen, UBitPeriod))
      t.io.req <> io.req
      t
    }
  }
  val resps = VecInit(tables.map(_.io.resp))

  val updateMeta = io.redirectInfo.redirect.tageMeta
  //val updateMisPred = UIntToOH(io.redirectInfo.redirect.fetchIdx) &
  //  Fill(FetchWidth, (io.redirectInfo.misPred && io.redirectInfo.redirect._type === BTBtype.B).asUInt)
  val updateMisPred = io.redirectInfo.misPred && io.redirectInfo.redirect._type === BTBtype.B

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
  val outHits = Vec(FetchWidth, Bool())
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
        io.out.takens(w) := Mux(ctr === 3.U || ctr === 4.U, altPred, ctr(2))
        finalAltPred := altPred
      }
      provided = provided || hit
      provider = Mux(hit, i.U, provider)
      altPred = Mux(hit, ctr(2), altPred)
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
      io.redirectInfo.redirect.taken && io.redirectInfo.redirect._type === BTBtype.B
    when (io.redirectInfo.redirect._type === BTBtype.B && io.redirectInfo.valid &&  io.redirectInfo.redirect.fetchIdx === w.U) {
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
    tables(i).io.update.pc := io.redirectInfo.redirect.pc - io.redirectInfo.redirect.fetchIdx << 2.U
    tables(i).io.update.hist := io.redirectInfo.redirect.hist
  }

  io.out.hits := outHits.asUInt

}