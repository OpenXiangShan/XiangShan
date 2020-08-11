package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

trait LTBParams extends HasXSParameter {
  //  +-----------+---------+--------------+-----------+
  //  |    tag    |   idx   |    4 bits    | 0 (1 bit) |
  //  +-----------+---------+--------------+-----------+
  val tagLen = 24
  val nRows = 16
  val idxLen = log2Up(nRows)
  val cntBits = 10
}

abstract class LTBBundle extends XSBundle with LTBParams
abstract class LTBModule extends XSModule with LTBParams

// class LoopMeta extends LTBBundle {
// }

class LoopEntry extends LTBBundle {
  val tag = UInt(tagLen.W)
  // how many times has the same loop trip count been seen in a row?
  val conf = UInt(3.W)
  // usefulness count, an entry can be replaced only if age counter is null
  val age = UInt(3.W) // TODO: delete this
  // loop trip count, the number of taken loop-branch before the last not-taken
  val tripCnt = UInt(cntBits.W)
  // the number of times loop-branch has been taken speculatively in a row
  val specCnt = UInt(cntBits.W)
  // the number of times loop-branch has been taken un-speculatively in a row
  val nSpecCnt = UInt(cntBits.W)

  def isLearned = conf === 7.U
  def isConf = conf =/= 0.U
  def isUnconf = conf === 0.U
}

class LTBColumnReq extends LTBBundle {
  val pc = UInt(VAddrBits.W) // only for debug!!!
  val idx = UInt(idxLen.W)
  val tag = UInt(tagLen.W)
}

class LTBColumnResp extends LTBBundle {
  // exit the loop
  val exit = Bool()
  val meta = UInt(cntBits.W)
}

class LTBColumnUpdate extends LTBBundle {
  val misPred = Bool()
  val pc = UInt(VAddrBits.W)
  val meta = UInt(cntBits.W)
  val taken = Bool()
}

// each column/bank of Loop Termination Buffer
class LTBColumn extends LTBModule {
  val io = IO(new Bundle() {
    // if3 send req
    val req = Input(Valid(new LTBColumnReq))
    // send out resp to if4
    val resp = Output(new LTBColumnResp)
    val update = Input(Valid(new LTBColumnUpdate))
    val repair = Input(Bool()) // roll back specCnts in the other 15 LTBs
  })

  val ltb = Reg(Vec(nRows, new LoopEntry))
  val ltbAddr = new TableAddr(idxLen + 4, PredictWidth)
  val updateIdx = ltbAddr.getBankIdx(io.update.bits.pc)
  val updateTag = ltbAddr.getTag(io.update.bits.pc)(tagLen - 1, 0)

  val doingReset = RegInit(true.B)
  val resetIdx = RegInit(0.U(idxLen.W))
  resetIdx := resetIdx + doingReset
  when (resetIdx === (nRows - 1).U) { doingReset := false.B }

  // during branch prediction
  val if3_idx = io.req.bits.idx
  val if3_tag = io.req.bits.tag
  val if3_pc = io.req.bits.pc // only for debug
  val if3_entry = WireInit(ltb(if3_idx))
  when (io.update.valid && io.update.bits.misPred) {
    when (updateIdx === if3_idx) {
      if3_entry.specCnt := 0.U
    }.otherwise {
      if3_entry.specCnt := ltb(if3_idx).nSpecCnt
    }
  }

  val if4_entry = RegEnable(if3_entry, io.req.valid)
  val if4_idx = RegEnable(if3_idx, io.req.valid)
  val if4_tag = RegEnable(if3_tag, io.req.valid)
  val if4_specCnt = Mux(io.update.valid && io.update.bits.misPred, Mux(updateIdx === if4_idx, 0.U, ltb(if4_idx).nSpecCnt), if4_entry.specCnt)
  io.resp.meta := if4_specCnt
  io.resp.exit := if4_tag === if4_entry.tag && if4_specCnt === if4_entry.tripCnt && if4_entry.isLearned

  // speculatively update specCnt
  when (RegNext(io.req.valid) && if4_entry.tag === if4_tag) {
    when (if4_specCnt === if4_entry.tripCnt && if4_entry.isLearned) {
      ltb(if4_idx).age := 7.U
      ltb(if4_idx).specCnt := 0.U
    }.otherwise {
      ltb(if4_idx).age := Mux(if4_entry.age === 7.U, 7.U, if4_entry.age + 1.U)
      ltb(if4_idx).specCnt := if4_specCnt + 1.U
    }
  }

  // when resolving a branch
  val updateSpecCnt = io.update.bits.meta
  val entry = ltb(updateIdx)
  val tagMatch = entry.tag === updateTag
  val cntMatch = entry.tripCnt === updateSpecCnt
  val wEntry = WireInit(entry)

  when (io.update.valid && !doingReset) {
    // When a branch resolves and is found to not be in the LTB,
    // it is inserted into the LTB if determined to be a loop-branch and if it is mispredicted by the default predictor.
    when (!tagMatch && io.update.bits.misPred) {
      wEntry.tag := updateTag
      wEntry.conf := 0.U
      wEntry.age := 7.U
      wEntry.tripCnt := Fill(cntBits, 1.U(1.W))
      wEntry.specCnt := 0.U
      wEntry.nSpecCnt := 0.U
      ltb(updateIdx) := wEntry
    }.elsewhen (tagMatch) {
      // During resolution, a taken branch found in the LTB has its nSpecCnt incremented by one.
      when (io.update.bits.taken) {
        wEntry.nSpecCnt := entry.nSpecCnt + 1.U
        wEntry.specCnt := Mux(io.update.bits.misPred, entry.nSpecCnt + 1.U, entry.specCnt)
      // A not-taken loop-branch found in the LTB during branch resolution updates its trip count and conf.
      }.otherwise {
        wEntry.conf := Mux(entry.nSpecCnt === entry.tripCnt, Mux(entry.isLearned, 7.U, entry.conf + 1.U), 0.U)
        wEntry.tripCnt := entry.nSpecCnt + 1.U
        wEntry.specCnt := Mux(io.update.bits.misPred, 0.U, entry.specCnt - entry.nSpecCnt - 1.U)
        wEntry.nSpecCnt := 0.U
      }
      ltb(updateIdx) := wEntry
    }
  }

  // Reseting
  when (doingReset) {
    ltb(resetIdx) := 0.U.asTypeOf(new LoopEntry)
  }

  // when a branch misprediction occurs, all of the nSpecCnts copy their values into the specCnts
  for (i <- 0 until nRows) {
    when (io.update.valid && io.update.bits.misPred && i.U =/= updateIdx || io.repair) {
      ltb(i).specCnt := ltb(i).nSpecCnt
    }
  }

  //debug info
  XSDebug(doingReset, "Reseting...\n")
  XSDebug("[IF3][req] v=%d pc=%x idx=%x tag=%x\n", io.req.valid, io.req.bits.pc, io.req.bits.idx, io.req.bits.tag)
  XSDebug("[IF3][if3_entry] tag=%x conf=%d age=%d tripCnt=%d specCnt=%d nSpecCnt=%d\n", if3_entry.tag, if3_entry.conf, if3_entry.age, if3_entry.tripCnt, if3_entry.specCnt, if3_entry.nSpecCnt)
  XSDebug("[IF4] idx=%x tag=%x specCnt=%d\n", if4_idx, if4_tag, if4_specCnt)
  XSDebug(RegNext(io.req.valid) && if4_entry.tag === if4_tag, "[IF4][speculative update] new specCnt=%d\n",
    Mux(if4_specCnt === if4_entry.tripCnt && if4_entry.isLearned, 0.U, if4_specCnt + 1.U))
  XSDebug("[update] v=%d misPred=%d pc=%x meta=%d taken=%d\n", io.update.valid, io.update.bits.misPred, io.update.bits.pc, io.update.bits.meta, io.update.bits.taken)
  XSDebug("[entry ] tag=%x conf=%d age=%d tripCnt=%d specCnt=%d nSpecCnt=%d\n", entry.tag, entry.conf, entry.age, entry.tripCnt, entry.specCnt, entry.nSpecCnt)
  XSDebug("[wEntry] tag=%x conf=%d age=%d tripCnt=%d specCnt=%d nSpecCnt=%d\n", wEntry.tag, wEntry.conf, wEntry.age, wEntry.tripCnt, wEntry.specCnt, wEntry.nSpecCnt)
  XSDebug(io.update.valid && io.update.bits.misPred || io.repair, "MisPred or repairing, all of the nSpecCnts copy their values into the specCnts\n")

}

class LoopPredictor extends BasePredictor with LTBParams {
  class LoopResp extends Resp {
    val exit = Vec(PredictWidth, Bool())
  }
  class LoopMeta extends Meta {
    val specCnts = Vec(PredictWidth, UInt(cntBits.W))
  }

  class LoopIO extends DefaultBasePredictorIO {
    val resp = Output(new LoopResp)
    val meta = Output(new LoopMeta)
  }

  override val io = IO(new LoopIO)
  
  val ltbs = Seq.fill(PredictWidth) { Module(new LTBColumn) }

  val ltbAddr = new TableAddr(idxLen + 4, PredictWidth)

  val baseBank = ltbAddr.getBank(io.pc.bits)
  val baseRow = ltbAddr.getBankIdx(io.pc.bits)
  val baseTag = ltbAddr.getTag(io.pc.bits)
  val nextRowStartsUp = baseRow.andR // TODO: use parallel andR
  val isInNextRow = VecInit((0 until PredictWidth).map(_.U < baseBank))
  val tagIncremented = VecInit((0 until PredictWidth).map(i => isInNextRow(i.U) && nextRowStartsUp))
  val realTags = VecInit((0 until PredictWidth).map(i => Mux(tagIncremented(i), baseTag + 1.U, baseTag)(tagLen - 1, 0)))
  for (i <- 0 until PredictWidth) {
    ltbs(i).io.req.valid := io.pc.valid && !io.flush
    ltbs(i).io.req.bits.pc := io.pc.bits + (i.U << 1) // only for debug
    ltbs(i).io.req.bits.idx := Mux(isInNextRow(i), baseRow + 1.U, baseRow)
    ltbs(i).io.req.bits.tag := realTags(i)
    // ltbs(i).io.if4_fire := io.if4_fire
    // ltbs(i).io.update := io.update
    ltbs(i).io.update.valid := i.U === ltbAddr.getBank(io.update.bits.ui.pc) && io.update.valid
    ltbs(i).io.update.bits.misPred := io.update.bits.ui.isMisPred
    ltbs(i).io.update.bits.pc := io.update.bits.ui.pc
    ltbs(i).io.update.bits.meta := io.update.bits.ui.brInfo.specCnt
    ltbs(i).io.update.bits.taken := io.update.bits.ui.taken
    ltbs(i).io.repair := i.U =/= ltbAddr.getBank(io.update.bits.ui.pc) && io.update.valid && io.update.bits.ui.isMisPred
  }

  val baseBankLatch = RegEnable(baseBank, io.pc.valid)
  val bankIdxInOrder = VecInit((0 until PredictWidth).map(i => (baseBankLatch +& i.U)(log2Up(PredictWidth) - 1, 0)))
  val ltbResps = VecInit((0 until PredictWidth).map(i => ltbs(i).io.resp))

  (0 until PredictWidth).foreach(i => io.resp.exit(i) := ltbResps(bankIdxInOrder(i)).exit)
  (0 until PredictWidth).foreach(i => io.meta.specCnts(i) := ltbResps(bankIdxInOrder(i)).meta)

  // debug info
  XSDebug("[IF3][req] fire=%d flush=%d fetchpc=%x baseBank=%x baseRow=%x baseTag=%x\n", io.pc.valid, io.flush, io.pc.bits, baseBank, baseRow, baseTag)
  XSDebug("[IF3][req] isInNextRow=%b tagInc=%b\n", isInNextRow.asUInt, tagIncremented.asUInt)
  for (i <- 0 until PredictWidth) {
    XSDebug(io.pc.valid, "[IF3][req] pc=%x idx=%x tag=%x\n", ltbs(i).io.req.bits.pc, ltbs(i).io.req.bits.idx, ltbs(i).io.req.bits.tag)
  }
  XSDebug("[IF4] baseBankLatch=%x bankIdxInOrder=", baseBankLatch)
  for (i <- 0 until PredictWidth) {
    XSDebug(false, true.B, "%x ", bankIdxInOrder(i))
  }
  XSDebug(false, true.B, "\n")
  for (i <- 0 until PredictWidth) {
    XSDebug(RegNext(io.pc.valid) && (i.U === 0.U || i.U === 8.U), "[IF4][resps]")
    XSDebug(false, RegNext(io.pc.valid), " %d:%d %d", i.U, io.resp.exit(i), io.meta.specCnts(i))
    XSDebug(false, RegNext(io.pc.valid) && (i.U === 7.U || i.U === 15.U), "\n")
  }
}