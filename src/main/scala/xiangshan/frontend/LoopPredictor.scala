package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.ExcitingUtils._
import xiangshan.backend.brq.BrqPtr
import chisel3.experimental.chiselName

trait LTBParams extends HasXSParameter with HasBPUParameter {
  //  +-----------+---------+--------------+-----------+
  //  |    tag    |   idx   |    4 bits    | 0 (1 bit) |
  //  +-----------+---------+--------------+-----------+
  val tagLen = 24
  val nRows = 16
  val idxLen = log2Up(nRows)
  val cntBits = 10
}

abstract class LTBBundle extends XSBundle with LTBParams
abstract class LTBModule extends XSModule with LTBParams { val debug = true }

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
  // brTag of the latest not-taken/loop-exit branch
  val brTag = new BrqPtr
  val unusable = Bool()

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
  val brTag = new BrqPtr
}

// each column/bank of Loop Termination Buffer
@chiselName
class LTBColumn extends LTBModule {
  val io = IO(new Bundle() {
    // if3 send req
    val req = Input(new LTBColumnReq)
    val if2_fire = Input(Bool())
    val if3_fire = Input(Bool())
    val if4_fire = Input(Bool())
    val outMask = Input(Bool())
    // send out resp to if4
    val resp = Output(new LTBColumnResp)
    val update = Input(Valid(new LTBColumnUpdate))
    val repair = Input(Bool()) // roll back specCnts in the other 15 LTBs
  })

  class LTBMem extends LTBModule {
    val io = IO(new Bundle {
      val rIdx = Input(UInt(idxLen.W))
      val rdata = Output(new LoopEntry)
      val urIdx = Input(UInt(idxLen.W))
      val urdata = Output(new LoopEntry)
      val wen = Input(Bool())
      val wIdx = Input(UInt(idxLen.W))
      val wdata = Input(new LoopEntry)
      val swen = Input(Bool())
      val swIdx = Input(UInt(idxLen.W))
      val swdata = Input(new LoopEntry)
      val copyCnt = Input(Vec(nRows, Bool()))
    })
    
    // val mem = RegInit(0.U.asTypeOf(Vec(nRows, new LoopEntry)))
    val mem = Mem(nRows, new LoopEntry)
    io.rdata  := mem(io.rIdx)
    io.urdata := mem(io.urIdx)
    val wdata = WireInit(io.wdata)
    val swdata = WireInit(io.swdata)
    for (i <- 0 until nRows) {
      val copyValid = io.copyCnt(i)
      when (copyValid && io.swIdx === i.U && io.swen) {
        swdata.specCnt := mem(i).nSpecCnt
      }
      val wd = WireInit(mem(i)) // default for copycnt
      val wen = WireInit(io.copyCnt(i) || io.wen && io.wIdx === i.U || io.swen && io.swIdx === i.U)
      when (!copyValid) {
        when (io.swen) {
          wd := swdata
        }.elsewhen (io.wen) {
          wd := wdata
        }
      }
      when (wen) {
        mem.write(i.U, wd)
      }
    }
  }
  // val ltb = Reg(Vec(nRows, new LoopEntry))
  val ltb = Module(new LTBMem).io
  val ltbAddr = new TableAddr(idxLen + 4, PredictWidth)
  val updateIdx = ltbAddr.getBankIdx(io.update.bits.pc)
  // val updateTag = ltbAddr.getTag(io.update.bits.pc)(tagLen - 1, 0)
  // val updateBrTag = io.update.bits.brTag

  val doingReset = RegInit(true.B)
  val resetIdx = RegInit(0.U(idxLen.W))
  resetIdx := resetIdx + doingReset
  when (resetIdx === (nRows - 1).U) { doingReset := false.B }

  // during branch prediction
  val if3_idx = io.req.idx
  val if3_tag = io.req.tag
  val if3_pc = io.req.pc // only for debug
  ltb.rIdx := if3_idx
  val if3_entry = WireInit(ltb.rdata)

  val if4_entry = Reg(new LoopEntry)
  // val if4_entry_reg = RegEnable(if3_entry, io.if3_fire)
  // val if4_entry = WireInit(if4_entry_reg)
  val if4_idx = RegEnable(if3_idx, io.if3_fire)
  val if4_tag = RegEnable(if3_tag, io.if3_fire)
  val if4_pc = RegEnable(if3_pc, io.if3_fire)

  val valid = RegInit(false.B)
  when (io.if4_fire) { valid := false.B }
  when (io.if3_fire) { valid := true.B }
  when (io.update.valid && io.update.bits.misPred) { valid := false.B }

  io.resp.meta := if4_entry.specCnt + 1.U
  // io.resp.exit := if4_tag === if4_entry.tag && (if4_entry.specCnt + 1.U) === if4_entry.tripCnt && valid && !if4_entry.unusable
  io.resp.exit := if4_tag === if4_entry.tag && (if4_entry.specCnt + 1.U) === if4_entry.tripCnt && valid && if4_entry.isConf

  // when resolving a branch
  val updateValid = RegNext(io.update.valid)
  val update = RegNext(io.update.bits)

  val updateTag = RegNext(ltbAddr.getTag(io.update.bits.pc)(tagLen - 1, 0))
  val updateBrTag = RegNext(update.brTag)

  ltb.urIdx := updateIdx
  val if3_uEntry = ltb.urdata

  val if4_uEntry = RegNext(if3_uEntry)
  val if4_uIdx = RegNext(updateIdx)
  val if4_uTag = RegNext(updateTag)

  val tagMatch = if4_uEntry.tag === updateTag
  val cntMatch = if4_uEntry.tripCnt === update.meta

  val wEntry = WireInit(if4_uEntry)

  ltb.wIdx := if4_uIdx
  ltb.wdata := wEntry
  ltb.wen := false.B

  // if4 update and write
  when (updateValid && !doingReset) {
    // When a branch resolves and is found to not be in the LTB,
    // it is inserted into the LTB if determined to be sa loop-branch and if it is mispredicted by the default predictor.
    when (!tagMatch && update.misPred) {
      // 没有判断conf是否等于0，以及age是否等于0
      XSDebug("Replace a entry\n")
      wEntry.tag := updateTag
      wEntry.conf := 0.U
      wEntry.age := 7.U
      wEntry.tripCnt := Fill(cntBits, 1.U(1.W))
      wEntry.specCnt := Mux(update.taken, 1.U, 0.U)
      wEntry.nSpecCnt := Mux(update.taken, 1.U, 0.U)
      wEntry.brTag := updateBrTag
      wEntry.unusable := false.B
      // ltb(updateIdx) := wEntry
      ltb.wen := true.B
    }.elsewhen (tagMatch) {
      // During resolution, a taken branch found in the LTB has its nSpecCnt incremented by one.
      when (update.taken) {
        XSDebug("if MisPred, recover specCnt, otherwise, keep client\n")
        wEntry.nSpecCnt := if4_uEntry.nSpecCnt + 1.U
        wEntry.specCnt := Mux(update.misPred/* && !entry.brTag.needBrFlush(updateBrTag)*/, if4_uEntry.nSpecCnt + 1.U, if4_uEntry.specCnt)
        wEntry.conf := Mux(update.misPred, 0.U, if4_uEntry.conf)
        // wEntry.tripCnt := Fill(cntBits, 1.U(1.W))
        wEntry.tripCnt := Mux(update.misPred, Fill(cntBits, 1.U(1.W)), if4_uEntry.tripCnt)
      // A not-taken loop-branch found in the LTB during branch resolution updates its trip count and conf.
      }.otherwise {
        XSDebug("Check tripCnt\n")
        // wEntry.conf := Mux(entry.nSpecCnt === entry.tripCnt, Mux(entry.isLearned, 7.U, entry.conf + 1.U), 0.U)
        // wEntry.conf := Mux(update.misPred, 0.U, Mux(entry.isLearned, 7.U, entry.conf + 1.U))
        wEntry.conf := Mux((if4_uEntry.nSpecCnt + 1.U) === if4_uEntry.tripCnt, Mux(if4_uEntry.isLearned, 7.U, if4_uEntry.conf + 1.U), 0.U)
        wEntry.tripCnt := if4_uEntry.nSpecCnt + 1.U
        // wEntry.tripCnt := update.meta
        wEntry.specCnt := Mux(update.misPred, /*entry.specCnt - update.meta*/0.U, if4_uEntry.specCnt/* - entry.nSpecCnt - 1.U*/)

        wEntry.nSpecCnt := 0.U
        wEntry.brTag := updateBrTag
        wEntry.unusable := update.misPred && (update.meta > if4_uEntry.tripCnt)
      }
      // ltb(updateIdx) := wEntry
      ltb.wen := true.B
    }
  }

  // speculatively update specCnt
  ltb.swen := valid && if4_entry.tag === if4_tag || doingReset
  ltb.swIdx := Mux(doingReset, resetIdx, if4_idx)
  val swEntry = WireInit(if4_entry)
  ltb.swdata := Mux(doingReset, 0.U.asTypeOf(new LoopEntry), swEntry)
  when (io.if4_fire && if4_entry.tag === if4_tag && io.outMask) {
    when ((if4_entry.specCnt + 1.U) === if4_entry.tripCnt && if4_entry.isConf) { // use nSpecCnts
      swEntry.age := 7.U
      // swEntry.specCnt := if4_entry.specCnt + 1.U
      swEntry.specCnt := 0.U
    }.otherwise {
      swEntry.age := Mux(if4_entry.age === 7.U, 7.U, if4_entry.age + 1.U)
      swEntry.specCnt := if4_entry.specCnt + 1.U
    }
  }

  // Bypass
  when(io.if3_fire) {
    when(ltb.swen && if3_idx === if4_idx) {
      XSDebug("Bypass swEntry\n")
      if4_entry := swEntry
    }.elsewhen(ltb.wen && if3_idx === if4_uIdx) {
      XSDebug("Bypass wEntry\n")
      if4_entry := wEntry
    }.otherwise {
      if4_entry := if3_entry
    }
  }.otherwise {
    when(ltb.swen) {
      XSDebug("spec Update\n")
      if4_entry := swEntry
    }.elsewhen(ltb.wen && if4_idx === if4_uIdx) {
      XSDebug("Keeping\n")
      if4_entry := wEntry
    }
  }

  // if4_uEntry bypass
  // when(io.if3_fire) {
    when(ltb.swen && updateIdx === if4_idx) {
      XSDebug("nSpec Bypass swEntry\n")
      if4_uEntry := swEntry
    }.elsewhen(ltb.wen && updateIdx === if4_uIdx) {
      XSDebug("nSpec Bypass wEntry\n")
      if4_uEntry := wEntry
    }.otherwise {
      if4_uEntry := if3_uEntry
    }
  // }.otherwise {
  //   when(ltb.swen && if4_idx === if4_uIdx) {
  //     XSDebug("Update spec update\n")
  //     if4_uEntry := swEntry
  //   }.elsewhen(ltb.wen) {
  //     XSDebug("Update nSpec update\n")
  //     if4_uEntry := wEntry
  //   }
  // }



  // Reseting
  // when (doingReset) {
  //   ltb(resetIdx) := 0.U.asTypeOf(new LoopEntry)
  // }

  // when a branch misprediction occurs, all of the nSpecCnts copy their values into the specCnts
  for (i <- 0 until nRows) {
    ltb.copyCnt(i) := io.update.valid && io.update.bits.misPred && i.U =/= updateIdx || io.repair
  }

  // bypass for if4_entry.specCnt
  when (updateValid && !doingReset && valid && if4_uIdx === if4_idx) {
    when (!tagMatch && update.misPred || tagMatch) {
      swEntry.nSpecCnt := wEntry.nSpecCnt
    }
  }
  when (io.repair && !doingReset && valid) {
    swEntry.specCnt := if4_entry.nSpecCnt
  }

  if (BPUDebug && debug) {
    //debug info
    XSDebug(doingReset, "Reseting...\n")
    XSDebug("if3_fire=%d if4_fire=%d valid=%d\n", io.if3_fire, io.if4_fire,valid)
    XSDebug("[req] v=%d pc=%x idx=%x tag=%x\n", valid, if3_pc, if3_idx, if3_tag)
    XSDebug("[if4_entry] tag=%x conf=%d age=%d tripCnt=%d specCnt=%d nSpecCnt=%d\n", 
      if4_entry.tag, if4_entry.conf, if4_entry.age, if4_entry.tripCnt, if4_entry.specCnt, if4_entry.nSpecCnt)
    XSDebug("[if3_entry] tag=%x conf=%d age=%d tripCnt=%d specCnt=%d nSpecCnt=%d\n", 
      if3_entry.tag, if3_entry.conf, if3_entry.age, if3_entry.tripCnt, if3_entry.specCnt, if3_entry.nSpecCnt)
    XSDebug(false, true.B, p" brTag=${if4_entry.brTag} unusable=${if4_entry.unusable}\n")
    XSDebug("swen=%d, ltb.swIdx, io.if4_fire=%d, if4_entry.tag=%x, if4_tag=%x, io.outMask=%d\n", valid && if4_entry.tag === if4_tag || doingReset, io.if4_fire, if4_entry.tag, if4_tag, io.outMask)
    XSDebug(io.if4_fire && if4_entry.tag === if4_tag && io.outMask, "[speculative update] new specCnt=%d\n",
      Mux((if4_entry.specCnt + 1.U) === if4_entry.tripCnt, 0.U, if4_entry.specCnt + 1.U))
    XSDebug("[if3_update] v=%d misPred=%d pc=%x idx=%x tag=%x meta=%d taken=%d tagMatch=%d cntMatch=%d\n", io.update.valid, io.update.bits.misPred, io.update.bits.pc, updateIdx, updateTag, io.update.bits.meta, io.update.bits.taken, tagMatch, cntMatch)
    XSDebug("[if4_update] v=%d misPred=%d pc=%x idx=%x tag=%x meta=%d taken=%d tagMatch=%d cntMatch=%d\n", updateValid, update.misPred, update.pc, if4_uIdx, if4_uTag, update.meta, update.taken, tagMatch, cntMatch)
    XSDebug(false, true.B, p" brTag=${updateBrTag}\n")
    XSDebug("[if3_uEntry] tag=%x conf=%d age=%d tripCnt=%d specCnt=%d nSpecCnt=%d\n", if3_uEntry.tag, if3_uEntry.conf, if3_uEntry.age, if3_uEntry.tripCnt, if3_uEntry.specCnt, if3_uEntry.nSpecCnt)
    XSDebug(false, true.B, p" brTag=${if3_uEntry.brTag} unusable=${if3_uEntry.unusable}\n")
    XSDebug("[if4_uEntry] tag=%x conf=%d age=%d tripCnt=%d specCnt=%d nSpecCnt=%d\n", if4_uEntry.tag, if4_uEntry.conf, if4_uEntry.age, if4_uEntry.tripCnt, if4_uEntry.specCnt, if4_uEntry.nSpecCnt)
    XSDebug(false, true.B, p" brTag=${if4_uEntry.brTag} unusable=${if4_uEntry.unusable}\n")
    XSDebug("[wEntry] tag=%x conf=%d age=%d tripCnt=%d specCnt=%d nSpecCnt=%d, wen=%d\n", wEntry.tag, wEntry.conf, wEntry.age, wEntry.tripCnt, wEntry.specCnt, wEntry.nSpecCnt, ltb.wen)
    XSDebug(false, true.B, p" brTag=${wEntry.brTag} unusable=${wEntry.unusable}\n")
    XSDebug(io.update.valid && io.update.bits.misPred || io.repair, "MisPred or repairing, all of the nSpecCnts copy their values into the specCnts\n")
  }

}

@chiselName
class LoopPredictor extends BasePredictor with LTBParams {
  class LoopResp extends Resp {
    val exit = Vec(PredictWidth, Bool())
  }
  class LoopMeta extends Meta {
    val specCnts = Vec(PredictWidth, UInt(cntBits.W))
  }
  class LoopRespIn extends XSBundle {
    val taken = Bool()
    val jmpIdx = UInt(log2Up(PredictWidth).W)
  }

  class LoopIO extends DefaultBasePredictorIO {
    val if3_fire = Input(Bool())
    val respIn = Input(new LoopRespIn)
    val resp = Output(new LoopResp)
    val meta = Output(new LoopMeta)
  }

  override val io = IO(new LoopIO)
  
  val ltbs = Seq.fill(PredictWidth) { Module(new LTBColumn) }

  val ltbAddr = new TableAddr(idxLen + 4, PredictWidth)

  // if3
  val if2_fire = io.pc.valid
  val pc = RegEnable(io.pc.bits, if2_fire) // This is if3_pc
  val tag = ltbAddr.getTag(pc)
  val bank = ltbAddr.getBank(pc)
  val bankIdx = ltbAddr.getBankIdx(pc)

  val updatePC = io.update.bits.pc
  val updateBank = ltbAddr.getBank(updatePC)

  // 只要把同一个packAligned PC的每一项传进16个ltb中即可
  val packetAlignedPC = packetAligned(pc)

  for (i <- 0 until PredictWidth) {
    ltbs(i).io.if2_fire := io.pc.valid
    ltbs(i).io.if3_fire := io.if3_fire
    ltbs(i).io.if4_fire := out_fire
    ltbs(i).io.req.idx := bankIdx
    ltbs(i).io.req.tag := tag
    // ltbs(i).io.outMask := outMask(i)

    ltbs(i).io.update.valid := i.U === updateBank && io.update.valid && io.update.bits.pd.isBr
    ltbs(i).io.update.bits.misPred := io.update.bits.isMisPred
    ltbs(i).io.update.bits.pc := updatePC
    ltbs(i).io.update.bits.meta := io.update.bits.bpuMeta.specCnt
    ltbs(i).io.update.bits.taken := io.update.bits.taken
    ltbs(i).io.update.bits.brTag := io.update.bits.brTag
    ltbs(i).io.repair := i.U =/= updateBank && io.update.valid && io.update.bits.isMisPred
  }

  // if4
  val if3_fire = io.if3_fire
  val inMask = io.inMask // This is if4_mask

  for (i <- 0 until PredictWidth) {
    ltbs(i).io.req.pc := packetAlignedPC
    ltbs(i).io.outMask := inMask(i)
  }

  val ltbResps = VecInit((0 until PredictWidth).map(i => ltbs(i).io.resp))

  for (i <- 0 until PredictWidth) {
    io.resp.exit(i) := ltbResps(i).exit
    io.meta.specCnts(i) := ltbResps(i).meta
  }

  if (!env.FPGAPlatform && env.EnablePerfDebug) {
    ExcitingUtils.addSource(io.resp.exit.reduce(_||_), "perfCntLoopExit", Perf)

    val loopAns = Wire(Vec(PredictWidth, new PredictorAnswer))

    loopAns.zipWithIndex.foreach{ case(x,i) =>
      x.hit := io.resp.exit(i)
      x.taken := false.B
      x.target := DontCare
    }

    ExcitingUtils.addSource(loopAns, "loopAns")
  }

  if (BPUDebug && debug) {
    // debug info
    XSDebug("[IF2][req] fire=%d fetchpc=%x\n", if2_fire, io.pc.bits)
    XSDebug("[IF3][req] fire=%d fetchpc=%x\n", if3_fire, pc)
    XSDebug("[IF4][req] fire=%d bank=%d packetAlignedPC=%x bankIdx=%x tag=%x\n", out_fire, bank, packetAlignedPC, bankIdx, tag)
    XSDebug("[IF4][req] inMask=%b\n", inMask)

    XSDebug("[IF4][req] updatePC=%x updateBank=%d, updateValid=%d, isBr=%d, isReplay=%d\n", updatePC, updateBank, io.update.valid, io.update.bits.pd.isBr, io.update.bits.isReplay)
    XSDebug("[IF4][req] isMisPred=%d updateSpecCnt=%d, taken=%d\n", io.update.bits.isMisPred, io.update.bits.bpuMeta.specCnt, io.update.bits.taken)

    // XSDebug(false, true.B, "\n")
    for (i <- 0 until PredictWidth) {
      XSDebug(out_fire && (i.U === 0.U || i.U === 4.U || i.U === 8.U || i.U === 12.U), "[IF4][resps]")
      XSDebug(false, out_fire, "[i:%d, e:%d, s:%d] ", i.U, io.resp.exit(i), io.meta.specCnts(i))
      XSDebug(false, out_fire && (i.U === 3.U || i.U === 7.U || i.U === 11.U || i.U === 15.U), "\n")
    }
  }
}
