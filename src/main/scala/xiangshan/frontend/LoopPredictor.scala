package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

trait LTBParams extends HasXSParameter {
  //  +-----------+---------+--------------+-----------+
  //  |    tag    |   idx   |    4 bits    | 0 (1 bit) |
  //  +-----------+---------+--------------+-----------+
  val tagLen = 10
  val nRows = 16
  val idxLen = log2Up(nRows)
  val cntBits = 10
}

abstract class LTBBundle extends XSBundle with LTBParams
abstract class LTBModule extends XSModule with LTBParams

class LoopMeta extends LTBBundle {
  // the number of times loop-branch has been taken speculatively in a row
  val specCnt = UInt(cntBits.W)
}

class LoopEntry extends LoopMeta {
  val tag = UInt(tagLen.W)
  // how many times has the same loop trip count been seen in a row?
  val conf = UInt(3.W)
  // usefulness count, an entry can be replaced only if age counter is null
  val age = UInt(3.W) // TODO: delete this
  // loop trip count, the number of taken loop-branch before the last not-taken
  val tripCnt = UInt(cntBits.W)
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
  val meta = new LoopMeta
}

class LTBColumnUpdate extends LTBBundle {
  val misPred = Bool()
  val pc = UInt(VAddrBits.W)
  val meta = new LoopMeta
  val taken = Bool()
}

// each column/bank of Loop Termination Buffer
class LTBColumn extends LTBModule {
  val io = IO(new Bundle() {
    // if3 send req
    val req = Input(Valid(new LTBColumnReq))
    // send out resp to if4
    val resp = Output(new LTBColumnResp)
    val if4_fire = Input(Bool())
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
  // val if3_idx = ltbAddr.getBankIdx(io.req.bits.pc)
  // val if3_tag = ltbAddr.getTag(io.req.bits.pc)(tagLen - 1, 0)
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
  io.resp.meta.specCnt := if4_specCnt
  io.resp.exit := if4_tag === if4_entry.tag && if4_specCnt === if4_entry.tripCnt && if4_entry.isLearned

  // speculatively update specCnt
  when (io.if4_fire && if4_entry.tag === if4_tag) {
    when (if4_specCnt === if4_entry.tripCnt && if4_entry.isLearned) {
      ltb(if4_idx).age := 7.U
      ltb(if4_idx).specCnt := 0.U
    }.otherwise {
      ltb(if4_idx).age := Mux(if4_entry.age === 7.U, 7.U, if4_entry.age + 1.U)
      ltb(if4_idx).specCnt := if4_specCnt + 1.U
    }
  }

  // when resolving a branch
  val updateSpecCnt = io.update.bits.meta.specCnt
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

}

class LoopPredictor extends LTBModule {
  val io = IO(new Bundle() {
    val if3_pc = Input(UInt(VAddrBits.W))
    val if3_fire = Input(Bool())
    val if4_out = Output(Vec(PredictWidth, (new LTBColumnResp)))
    val if4_fire = Input(Bool())
    // send update only if it's a branch instr (or a loop-branch???)
    val update = Input(Valid(new LTBColumnUpdate))
  })

  val ltbs = Seq.fill(PredictWidth) { Module(new LTBColumn) }

  val ltbAddr = new TableAddr(idxLen + 4, PredictWidth)

  val baseBank = ltbAddr.getBank(io.if3_pc)
  val baseRow = ltbAddr.getBankIdx(io.if3_pc)
  val baseTag = ltbAddr.getTag(io.if3_pc)
  val nextRowStartsUp = baseRow.andR // TODO: use parallel andR
  val isInNextRow = VecInit((0 until PredictWidth).map(_.U < baseBank))
  val tagIncremented = VecInit((0 until PredictWidth).map(i => isInNextRow(i.U) && nextRowStartsUp))
  val realTags = VecInit((0 until PredictWidth).map(i => Mux(tagIncremented(i), baseTag + 1.U, baseTag)(tagLen - 1, 0)))
  for (i <- 0 until PredictWidth) {
    ltbs(i).io.req.valid := io.if3_fire
    ltbs(i).io.req.bits.pc := io.if3_pc + (i.U << 1) // only for debug
    ltbs(i).io.req.bits.idx := Mux(isInNextRow(i), baseRow + 1.U, baseRow)
    ltbs(i).io.req.bits.tag := realTags(i)
    ltbs(i).io.if4_fire := io.if4_fire
    ltbs(i).io.update := io.update
    ltbs(i).io.update.valid := i.U === ltbAddr.getBank(io.update.bits.pc)
    ltbs(i).io.repair := i.U =/= ltbAddr.getBank(io.update.bits.pc) && io.update.valid && io.update.bits.misPred
  }

  val baseBankLatch = RegEnable(baseBank, io.if3_fire)
  val bankIdxInOrder = VecInit((0 until PredictWidth).map(i => (baseBankLatch +& i.U)(log2Up(PredictWidth) - 1, 0)))
  val ltbResps = VecInit((0 until PredictWidth).map(i => ltbs(i).io.resp))

  (0 until PredictWidth).foreach(i => io.if4_out(i) := ltbResps(bankIdxInOrder(i)))

}