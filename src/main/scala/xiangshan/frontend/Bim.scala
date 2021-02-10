package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.experimental.chiselName

trait BimParams extends HasXSParameter {
  val BimBanks = PredictWidth
  val BimSize = 4096
  val nRows = BimSize / BimBanks
  val bypassEntries = 4
}

@chiselName
class BIM extends BasePredictor with BimParams {
  class BIMResp extends Resp {
    val ctrs = Vec(PredictWidth, UInt(2.W))
  }
  class BIMMeta extends Meta {
    val ctrs = Vec(PredictWidth, UInt(2.W))
  }
  class BIMFromOthers extends FromOthers {}

  class BIMIO extends DefaultBasePredictorIO {
    val resp = Output(new BIMResp)
    val meta = Output(new BIMMeta)
  }

  override val io = IO(new BIMIO)
  override val debug = true

  val bimAddr = new TableAddr(log2Up(BimSize), BimBanks)

  val bim = List.fill(BimBanks) {
    Module(new SRAMTemplate(UInt(2.W), set = nRows, shouldReset = false, holdRead = true))
  }

  val doing_reset = RegInit(true.B)
  val resetRow = RegInit(0.U(log2Ceil(nRows).W))
  resetRow := resetRow + doing_reset
  when (resetRow === (nRows-1).U) { doing_reset := false.B }

  val if1_packetAlignedPC = packetAligned(io.pc.bits)
  val if2_pc = RegEnable(if1_packetAlignedPC, io.pc.valid)

  val if1_mask = io.inMask
  val if1_row  = bimAddr.getBankIdx(if1_packetAlignedPC)

  for (b <- 0 until BimBanks) {
    bim(b).io.r.req.valid       := if1_mask(b) && io.pc.valid
    bim(b).io.r.req.bits.setIdx := if1_row
  }

  val if2_bimRead = VecInit(bim.map(_.io.r.resp.data(0)))

  for (b <- 0 until BimBanks) {
    io.resp.ctrs(b)  := if2_bimRead(b)
    io.meta.ctrs(b)  := if2_bimRead(b)
  }

  val u = io.update.bits

  val updateRow = bimAddr.getBankIdx(u.ftqPC)


  val wrbypass_ctrs       = Reg(Vec(bypassEntries, Vec(BimBanks, UInt(2.W))))
  val wrbypass_ctr_valids = Reg(Vec(bypassEntries, Vec(BimBanks, Bool())))
  val wrbypass_rows     = Reg(Vec(bypassEntries, UInt(log2Up(nRows).W)))
  val wrbypass_enq_idx  = RegInit(0.U(log2Up(bypassEntries).W))

  val wrbypass_hits = VecInit((0 until bypassEntries).map( i => 
    !doing_reset && wrbypass_rows(i) === updateRow))
  val wrbypass_hit = wrbypass_hits.reduce(_||_)
  val wrbypass_hit_idx = PriorityEncoder(wrbypass_hits)

  val oldCtrs = VecInit((0 until BimBanks).map(b => 
                  Mux(wrbypass_hit && wrbypass_ctr_valids(wrbypass_hit_idx)(b),
                    wrbypass_ctrs(wrbypass_hit_idx)(b), u.metas(b).bimCtr)))
  
  val newTakens = VecInit((0 until BimBanks).map(b => u.cfiIndex.valid && u.cfiIndex.bits === b.U))
  val newCtrs = VecInit((0 until BimBanks).map(b => satUpdate(oldCtrs(b), 2, newTakens(b))))
  // val oldSaturated = newCtr === oldCtr
  
  val needToUpdate = VecInit((0 until PredictWidth).map(i => io.update.valid && u.br_mask(i) && u.valids(i)))

  when (reset.asBool) { wrbypass_ctr_valids.foreach(_.foreach(_ := false.B))}
  
  for (b <- 0 until BimBanks) {
    when (needToUpdate(b)) {
      when (wrbypass_hit) {
        wrbypass_ctrs(wrbypass_hit_idx)(b) := newCtrs(b)
        wrbypass_ctr_valids(wrbypass_hit_idx)(b) := true.B
      } .otherwise {
        wrbypass_ctrs(wrbypass_enq_idx)(b) := newCtrs(b)
        (0 until BimBanks).foreach(b => wrbypass_ctr_valids(wrbypass_enq_idx)(b) := false.B) // reset valid bits
        wrbypass_ctr_valids(wrbypass_enq_idx)(b) := true.B
        wrbypass_rows(wrbypass_enq_idx) := updateRow
        wrbypass_enq_idx := (wrbypass_enq_idx + 1.U)(log2Up(bypassEntries)-1,0)
      }
    }
  }

  for (b <- 0 until BimBanks) {
    bim(b).io.w.req.valid := needToUpdate(b) || doing_reset
    bim(b).io.w.req.bits.setIdx := Mux(doing_reset, resetRow, updateRow)
    bim(b).io.w.req.bits.data := Mux(doing_reset, 2.U(2.W), newCtrs(b))
  }

  if (BPUDebug && debug) {
    XSDebug(doing_reset, "Reseting...\n")
    XSDebug("[update] v=%d pc=%x valids=%b, tgt=%x\n", io.update.valid, u.ftqPC, u.valids.asUInt, u.target)
    
    XSDebug("[update] brMask=%b, taken=%b isMisPred=%b\n", u.br_mask.asUInt, newTakens.asUInt, u.mispred.asUInt)
    for (i <- 0 until BimBanks) {
      XSDebug(true.B, p"bimCtr(${i.U})=${Binary(u.metas(i).bimCtr)} oldCtr=${Binary(oldCtrs(i))} newCtr=${Binary(newCtrs(i))}\n")
    }
    XSDebug("needToUpdate=%b updateRow=%x\n", needToUpdate.asUInt, updateRow)
    XSDebug("[wrbypass] hit=%d hits=%b\n", wrbypass_hit, wrbypass_hits.asUInt)
  }
  
}