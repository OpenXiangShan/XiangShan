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

  val updateBank = bimAddr.getBank(u.pc)
  val updateRow = bimAddr.getBankIdx(u.pc)


  val wrbypass_ctrs       = Reg(Vec(bypassEntries, Vec(BimBanks, UInt(2.W))))
  val wrbypass_ctr_valids = Reg(Vec(bypassEntries, Vec(BimBanks, Bool())))
  val wrbypass_rows     = Reg(Vec(bypassEntries, UInt(log2Up(nRows).W)))
  val wrbypass_enq_idx  = RegInit(0.U(log2Up(bypassEntries).W))

  val wrbypass_hits = VecInit((0 until bypassEntries).map( i => 
    !doing_reset && wrbypass_rows(i) === updateRow))
  val wrbypass_hit = wrbypass_hits.reduce(_||_)
  val wrbypass_hit_idx = PriorityEncoder(wrbypass_hits)

  val oldCtr = Mux(wrbypass_hit && wrbypass_ctr_valids(wrbypass_hit_idx)(updateBank), wrbypass_ctrs(wrbypass_hit_idx)(updateBank), u.bpuMeta.bimCtr)
  val newTaken = u.taken
  val newCtr = satUpdate(oldCtr, 2, newTaken)
  // val oldSaturated = newCtr === oldCtr
  
  val needToUpdate = io.update.valid && u.pd.isBr && !u.isReplay

  when (reset.asBool) { wrbypass_ctr_valids.foreach(_.foreach(_ := false.B))}
  
  when (needToUpdate) {
    when (wrbypass_hit) {
      wrbypass_ctrs(wrbypass_hit_idx)(updateBank) := newCtr
      wrbypass_ctr_valids(wrbypass_hit_idx)(updateBank) := true.B
    } .otherwise {
      wrbypass_ctrs(wrbypass_enq_idx)(updateBank) := newCtr
      (0 until BimBanks).foreach(b => wrbypass_ctr_valids(wrbypass_enq_idx)(b) := false.B) // reset valid bits
      wrbypass_ctr_valids(wrbypass_enq_idx)(updateBank) := true.B
      wrbypass_rows(wrbypass_enq_idx) := updateRow
      wrbypass_enq_idx := (wrbypass_enq_idx + 1.U)(log2Up(bypassEntries)-1,0)
    }
  }

  for (b <- 0 until BimBanks) {
    bim(b).io.w.req.valid := needToUpdate && b.U === updateBank || doing_reset
    bim(b).io.w.req.bits.setIdx := Mux(doing_reset, resetRow, updateRow)
    bim(b).io.w.req.bits.data := Mux(doing_reset, 2.U(2.W), newCtr)
  }

  if (!env.FPGAPlatform && env.EnablePerfDebug) {
    val bimResp = Wire(Vec(PredictWidth, Bool()))
    for(i <- 0 until PredictWidth) {
      bimResp(i) := io.resp.ctrs(i)(1)
    }
    ExcitingUtils.addSource(bimResp, "bimResp")
  }

  if (BPUDebug && debug) {
    XSDebug(doing_reset, "Reseting...\n")
    XSDebug("[update] v=%d pc=%x pnpc=%x tgt=%x", io.update.valid, u.pc, u.pnpc, u.target)
    XSDebug("[update] taken=%d isMisPred=%d", u.taken, u.isMisPred)
    XSDebug(false, true.B, p"brTag=${u.brTag} pd.isBr=${u.pd.isBr} brInfo.bimCtr=${Binary(u.bpuMeta.bimCtr)}\n")
    XSDebug("needToUpdate=%d updateBank=%x updateRow=%x newCtr=%b oldCtr=%b\n", needToUpdate, updateBank, updateRow, newCtr, oldCtr)
    XSDebug("[wrbypass] hit=%d hits=%b\n", wrbypass_hit, wrbypass_hits.asUInt)
  }
  
}