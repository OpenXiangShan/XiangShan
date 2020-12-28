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
  val updateTag = ltbAddr.getTag(io.update.bits.pc)(tagLen - 1, 0)
  val updateBrTag = io.update.bits.brTag

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

  // val updateBypassValid = RegNext(io.update.valid)
  // val updateBypass = RegNext(io.update.bits)
  // val bypassTagMatch = if4_entry.tag === ltbAddr.getTag(updateBypass.pc)(tagLen - 1, 0)

  // // Bypass
  // when(updateBypassValid && !doingReset && ltbAddr.getBankIdx(updateBypass.pc) === if4_idx) {
  //   when (!tagMatch && io.update.bits.misPred) {
  //     // 没有判断conf是否等于0，以及age是否等于0
  //     XSDebug("Repalce a entry\n")
  //     wEntry.tag := updateTag
  //     wEntry.conf := 0.U
  //     wEntry.age := 7.U
  //     wEntry.tripCnt := Fill(cntBits, 1.U(1.W))
  //     wEntry.specCnt := Mux(io.update.bits.taken, 1.U, 0.U)
  //     wEntry.nSpecCnt := Mux(io.update.bits.taken, 1.U, 0.U)
  //     wEntry.brTag := updateBrTag
  //     wEntry.unusable := false.B
  //     // ltb(updateIdx) := wEntry
  //     ltb.wen := true.B
  //   }

  //   XSDebug("if4_entry bypass\n")
  //   when(updateBypass.taken) {
  //     if4_entry.nSpecCnt := if4_entry.nSpecCnt + 1.U
  //   }.otherwise {
  //     if4_entry.conf := Mux((if4_entry_reg.nSpecCnt + 1.U) === if4_entry_reg.tripCnt, Mux(if4_entry_reg.isLearned, 7.U, if4_entry_reg.conf + 1.U), 0.U)
  //     if4_entry.tripCnt := if4_entry.nSpecCnt + 1.U
  //     if4_entry.specCnt := Mux(updateBypass.misPred, 0.U, if4_entry.specCnt - updateBypass.meta)
  //     if4_entry.nSpecCnt := 0.U
  //   }
  // }

  val valid = RegInit(false.B)
  when (io.if4_fire) { valid := false.B }
  when (io.if3_fire) { valid := true.B }
  when (io.update.valid && io.update.bits.misPred) { valid := false.B }

  io.resp.meta := if4_entry.specCnt + 1.U
  // io.resp.exit := if4_tag === if4_entry.tag && (if4_entry.specCnt + 1.U) === if4_entry.tripCnt && valid && !if4_entry.unusable
  io.resp.exit := if4_tag === if4_entry.tag && (if4_entry.specCnt + 1.U) === if4_entry.tripCnt && valid && if4_entry.isConf

  // when resolving a branch
  ltb.urIdx := updateIdx
  val entry = ltb.urdata
  val tagMatch = entry.tag === updateTag
  val cntMatch = entry.tripCnt === io.update.bits.meta
  val wEntry = WireInit(entry)

  ltb.wIdx := updateIdx
  ltb.wdata := wEntry
  ltb.wen := false.B

  when (io.update.valid && !doingReset) {
    // When a branch resolves and is found to not be in the LTB,
    // it is inserted into the LTB if determined to be a loop-branch and if it is mispredicted by the default predictor.
    when (!tagMatch && io.update.bits.misPred) {
      // 没有判断conf是否等于0，以及age是否等于0
      XSDebug("Repalce a entry\n")
      wEntry.tag := updateTag
      wEntry.conf := 0.U
      wEntry.age := 7.U
      wEntry.tripCnt := Fill(cntBits, 1.U(1.W))
      wEntry.specCnt := Mux(io.update.bits.taken, 1.U, 0.U)
      wEntry.nSpecCnt := Mux(io.update.bits.taken, 1.U, 0.U)
      wEntry.brTag := updateBrTag
      wEntry.unusable := false.B
      // ltb(updateIdx) := wEntry
      ltb.wen := true.B
    }.elsewhen (tagMatch) {
      // During resolution, a taken branch found in the LTB has its nSpecCnt incremented by one.
      when (io.update.bits.taken) {
        XSDebug("if MisPred, recover specCnt, otherwise, keep client\n")
        wEntry.nSpecCnt := entry.nSpecCnt + 1.U
        wEntry.specCnt := Mux(io.update.bits.misPred/* && !entry.brTag.needBrFlush(updateBrTag)*/, entry.nSpecCnt + 1.U, entry.specCnt)
        wEntry.conf := Mux(io.update.bits.misPred, 0.U, entry.conf)
        // wEntry.tripCnt := Fill(cntBits, 1.U(1.W))
        wEntry.tripCnt := Mux(io.update.bits.misPred, Fill(cntBits, 1.U(1.W)), entry.tripCnt)
      // A not-taken loop-branch found in the LTB during branch resolution updates its trip count and conf.
      }.otherwise {
        XSDebug("Check tripCnt\n")
        // wEntry.conf := Mux(entry.nSpecCnt === entry.tripCnt, Mux(entry.isLearned, 7.U, entry.conf + 1.U), 0.U)
        // wEntry.conf := Mux(io.update.bits.misPred, 0.U, Mux(entry.isLearned, 7.U, entry.conf + 1.U))
        wEntry.conf := Mux((entry.nSpecCnt + 1.U) === entry.tripCnt, Mux(entry.isLearned, 7.U, entry.conf + 1.U), 0.U)
        wEntry.tripCnt := entry.nSpecCnt + 1.U
        // wEntry.tripCnt := io.update.bits.meta
        wEntry.specCnt := Mux(io.update.bits.misPred, /*entry.specCnt - io.update.bits.meta*/0.U, entry.specCnt - io.update.bits.meta/* - entry.nSpecCnt - 1.U*/)

        wEntry.nSpecCnt := 0.U
        wEntry.brTag := updateBrTag
        wEntry.unusable := io.update.bits.misPred && (io.update.bits.meta > entry.tripCnt)
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
      swEntry.specCnt := if4_entry.specCnt + 1.U
    }.otherwise {
      swEntry.age := Mux(if4_entry.age === 7.U, 7.U, if4_entry.age + 1.U)
      swEntry.specCnt := if4_entry.specCnt + 1.U
    }
  }

  // Bypass
  when (ltb.swen && if3_idx === if4_idx) {
    XSDebug("if3_entry := swEntry\n")
    if3_entry := swEntry
  }.elsewhen (ltb.wen && if3_idx === updateIdx) {
    XSDebug("if3_entry := wEntry\n")
    if3_entry := wEntry
  }

  when(io.if3_fire) {
    if4_entry := if3_entry
  }.elsewhen(ltb.swen) {
    if4_entry := swEntry
  }.elsewhen(ltb.wen && if4_idx === updateIdx) {
    if4_entry := wEntry
  }

  // Reseting
  // when (doingReset) {
  //   ltb(resetIdx) := 0.U.asTypeOf(new LoopEntry)
  // }

  // when a branch misprediction occurs, all of the nSpecCnts copy their values into the specCnts
  for (i <- 0 until nRows) {
    ltb.copyCnt(i) := io.update.valid && io.update.bits.misPred && i.U =/= updateIdx || io.repair
  }

  // bypass for if4_entry.specCnt
  when (io.update.valid && !doingReset && valid && updateIdx === if4_idx) {
    when (!tagMatch && io.update.bits.misPred || tagMatch) {
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
    XSDebug("[update] v=%d misPred=%d pc=%x idx=%x tag=%x meta=%d taken=%d tagMatch=%d cntMatch=%d", io.update.valid, io.update.bits.misPred, io.update.bits.pc, updateIdx, updateTag, io.update.bits.meta, io.update.bits.taken, tagMatch, cntMatch)
    XSDebug(false, true.B, p" brTag=${updateBrTag}\n")
    XSDebug("[entry ] tag=%x conf=%d age=%d tripCnt=%d specCnt=%d nSpecCnt=%d", entry.tag, entry.conf, entry.age, entry.tripCnt, entry.specCnt, entry.nSpecCnt)
    XSDebug(false, true.B, p" brTag=${entry.brTag} unusable=${entry.unusable}\n")
    XSDebug("[wEntry] tag=%x conf=%d age=%d tripCnt=%d specCnt=%d nSpecCnt=%d, wen=%d", wEntry.tag, wEntry.conf, wEntry.age, wEntry.tripCnt, wEntry.specCnt, wEntry.nSpecCnt, ltb.wen)
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

  // Latch for 1 cycle
  // val pc = RegEnable(io.pc.bits, io.pc.valid)
  // val inMask = RegEnable(io.inMask, io.pc.valid)
  // val pc = io.pc.bits
  // val inMask = io.inMask
  // val if3_fire = io.pc.valid

  // val baseBank = ltbAddr.getBank(pc)
  // val baseRow = ltbAddr.getBankIdx(pc)
  // val baseTag = ltbAddr.getTag(pc)
  // val nextRowStartsUp = baseRow.andR // TODO: use parallel andR
  // val isInNextRow = VecInit((0 until PredictWidth).map(_.U < baseBank))
  // val tagIncremented = VecInit((0 until PredictWidth).map(i => isInNextRow(i.U) && nextRowStartsUp))
  // val realTags = VecInit((0 until PredictWidth).map(i => Mux(tagIncremented(i), baseTag + 1.U, baseTag)(tagLen - 1, 0)))
  // // val bankIdxInOrder = VecInit((0 until PredictWidth).map(i => (baseBank +& i.U)(log2Up(PredictWidth) - 1, 0)))
  // // val realMask = circularShiftLeft(inMask, PredictWidth, baseBank)
  // val outMask = inMask & (Fill(PredictWidth, !io.respIn.taken) | (Fill(PredictWidth, 1.U(1.W)) >> (~io.respIn.jmpIdx)))
  // val realMask = Wire(UInt(PredictWidth.W))
  // val offsetIdx = offsetInBank(io.pc.bits) // 这个pc在一个bank中的第几位
  // val bankIdxInOrder = VecInit((0 until PredictWidth).map(i => Mux(offsetIdx <= i.U, baseBank + i.U - offsetIdx, 0.U)(log2Up(PredictWidth) - 1, 0)))

  // if3
  val if2_fire = io.pc.valid
  val pc = RegEnable(io.pc.bits, if2_fire) // This is if3_pc
  val tag = ltbAddr.getTag(pc)
  val bank = ltbAddr.getBank(pc)
  val bankIdx = ltbAddr.getBankIdx(pc)

  val updatePC = io.update.bits.pc
  val updateBank = ltbAddr.getBank(updatePC)

  // 只要把同一个bankAligned PC的每一项传进16个ltb中即可
  val bankAlignedPC = align(pc, PredictWidth)

  for (i <- 0 until PredictWidth) {
    ltbs(i).io.if2_fire := io.pc.valid
    ltbs(i).io.if3_fire := io.if3_fire
    ltbs(i).io.if4_fire := io.outFire
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

  val startsAtOddBank = RegEnable(bankInGroup(bankAlignedPC)(0).asBool, if3_fire)
  val reorderMask = Mux(startsAtOddBank, Cat(inMask(PredictWidth/2-1, 0), inMask(PredictWidth-1 ,PredictWidth/2)), inMask)

  for (i <- 0 until PredictWidth) {
    ltbs(i).io.req.pc := bankAlignedPC
    ltbs(i).io.outMask := reorderMask(i)
  }

  val ltbResps = VecInit((0 until PredictWidth).map(i => ltbs(i).io.resp))

  for(i <- 0 until PredictWidth/2) {
    when(startsAtOddBank) {
      io.resp.exit(i) := ltbResps(i + PredictWidth/2).exit
      io.meta.specCnts(i) := ltbResps(i + PredictWidth/2).meta
    }.otherwise {
      io.resp.exit(i) := ltbResps(i).exit
      io.meta.specCnts(i) := ltbResps(i).meta

    }
  }

  for(i <- PredictWidth/2 until PredictWidth) {
    when(startsAtOddBank) {
      io.resp.exit(i) := ltbResps(i - PredictWidth/2).exit
      io.meta.specCnts(i) := ltbResps(i - PredictWidth/2).meta
    }.otherwise {
      io.resp.exit(i) := ltbResps(i).exit
      io.meta.specCnts(i) := ltbResps(i).meta
    }
  }

  ExcitingUtils.addSource(io.resp.exit.reduce(_||_), "perfCntLoopExit", Perf)

  if (BPUDebug && debug) {
    // debug info
    XSDebug("[IF2][req] fire=%d flush=%d fetchpc=%x\n", if2_fire, io.flush, io.pc.bits)
    XSDebug("[IF3][req] fire=%d flush=%d fetchpc=%x\n", if3_fire, io.flush, pc)
    XSDebug("[IF4][req] fire=%d bank=%d bankAlignedPC=%x bankIdx=%x tag=%x\n", io.outFire, bank, bankAlignedPC, bankIdx, tag)
    XSDebug("[IF4][req] inMask=%b, reorderMask=%b\n", inMask, reorderMask)

    XSDebug("[IF4][req] updatePC=%x updateBank=%d, updateValid=%d, isBr=%d, isReplay=%d\n", updatePC, updateBank, io.update.valid, io.update.bits.pd.isBr, io.update.bits.isReplay)
    XSDebug("[IF4][req] isMisPred=%d updateSpecCnt=%d, taken=%d\n", io.update.bits.isMisPred, io.update.bits.bpuMeta.specCnt, io.update.bits.taken)

    // XSDebug(false, true.B, "\n")
    for (i <- 0 until PredictWidth) {
      XSDebug(io.outFire && (i.U === 0.U || i.U === 8.U), "[IF4][resps]")
      XSDebug(false, io.outFire, "[i:%d, e:%d, s:%d] ", i.U, io.resp.exit(i), io.meta.specCnts(i))
      XSDebug(false, io.outFire && (i.U === 7.U || i.U === 15.U), "\n")
    }
  }
}