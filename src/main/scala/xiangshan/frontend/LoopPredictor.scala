package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.ExcitingUtils._
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
  // brTag of the latest not-taken/loop-exit branch
  // val unusable = Bool()

  def isLearned = conf === 7.U
  def isConf = conf =/= 0.U && conf =/= 7.U
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
  val specCnt = UInt(cntBits.W)
}

class LTBColumnUpdate extends LTBBundle {
  // val misPred = Bool()
  val pc = UInt(VAddrBits.W)
  // val specCnt = UInt(cntBits.W)
  // val taken = Bool()
}

class LTBColumnRedirect extends LTBBundle {
  val mispred = Bool()
  val pc = UInt(VAddrBits.W)
  val specCnt = UInt(cntBits.W)
  val taken = Bool()
  val isReplay = Bool()
}

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
    val redirect = Input(Valid(new LTBColumnRedirect))
    val repair = Input(Bool()) // roll back specCnts in the other 15 LTBs
  })

  val ltb = Mem(nRows, new LoopEntry)
  val ltbAddr = new TableAddr(idxLen + 4, PredictWidth)

  val updateValid = RegNext(io.update.valid)
  val updatePC = RegNext(io.update.bits.pc)
  val updateIdx = ltbAddr.getBankIdx(io.update.bits.pc)
  val updateTag = RegNext(ltbAddr.getTag(io.update.bits.pc)(tagLen - 1, 0))
  // val update = RegNext(io.update.bits)

  val redirectValid = RegNext(io.redirect.valid)
  val redirectPC = RegNext(io.redirect.bits.pc)
  val redirectIdx = ltbAddr.getBankIdx(io.redirect.bits.pc)
  val redirectTag = RegNext(ltbAddr.getTag(io.redirect.bits.pc)(tagLen - 1, 0))
  val redirect = RegNext(io.redirect.bits)
  val isReplay = RegNext(io.redirect.bits.isReplay)

  // during branch prediction
  val if3_idx = io.req.idx
  val if3_tag = io.req.tag
  val if3_pc = io.req.pc // only for debug

  val if4_idx = RegEnable(if3_idx, io.if3_fire)
  val if4_tag = RegEnable(if3_tag, io.if3_fire)
  val if4_pc = RegEnable(if3_pc, io.if3_fire)

  val if3_entry = WireInit(ltb.read(if3_idx))
  val if4_entry = Reg(new LoopEntry)

  val valid = RegInit(false.B)
  when (io.if4_fire) { valid := false.B }
  when (io.if3_fire) { valid := true.B }
  when (redirectValid && redirect.mispred) { valid := false.B }

  io.resp.specCnt := if4_entry.specCnt
  io.resp.exit := io.outMask && if4_tag === if4_entry.tag && if4_entry.specCnt === if4_entry.tripCnt && valid && if4_entry.isLearned

  // Reset
  val doingReset = RegInit(true.B)
  val resetIdx = RegInit(0.U(idxLen.W))
  resetIdx := resetIdx + doingReset
  when (resetIdx === (nRows - 1).U) { doingReset := false.B }

  // speculatively update specCnt
  val swen = valid && if4_entry.tag === if4_tag || doingReset
  val swEntry = WireInit(if4_entry)
  when (io.if4_fire && if4_entry.tag === if4_tag && io.outMask) {
    when (if4_entry.specCnt === if4_entry.tripCnt && if4_entry.isLearned) {
      swEntry.age := 7.U
      swEntry.specCnt := 0.U
    }.otherwise {
      swEntry.age := Mux(if4_entry.age === 7.U, 7.U, if4_entry.age + 1.U)
      swEntry.specCnt := if4_entry.specCnt + 1.U
    }
  }

  when(swen) {ltb.write(Mux(doingReset, resetIdx, if4_idx), Mux(doingReset, 0.U.asTypeOf(new LoopEntry), swEntry))}


  val if3_uEntry = RegNext(ltb.read(updateIdx))
  val if4_uEntry = RegNext(if3_uEntry)
  val if4_uIdx = RegNext(updateIdx)
  val if4_uTag = RegNext(updateTag)

  val if3_rEntry = RegNext(ltb.read(redirectIdx))
  val if4_rEntry = RegNext(if3_rEntry)
  val if4_rIdx = RegNext(redirectIdx)
  val if4_rTag = RegNext(redirectTag)

  val tagMatch = if4_rEntry.tag === redirectTag
  val cntMatch = if4_rEntry.tripCnt === redirect.specCnt

  val wEntry = WireInit(if4_rEntry)
  val wen = WireInit(false.B)
  when(wen) {ltb.write(if4_rIdx, wEntry)}

  val loop_entry_is_learned = WireInit(false.B)
  val loop_learned_entry_conflict = WireInit(false.B)
  val loop_conf_entry_evicted = WireInit(false.B)

  when(redirectValid && redirect.mispred && !isReplay && !doingReset) {
    wen := true.B
    when(tagMatch) {
      when(if4_rEntry.isLearned) {
        XSDebug("[redirect] 0\n")
        wEntry.conf := 0.U
        wEntry.specCnt := 0.U
      }.elsewhen(if4_rEntry.isConf) {
        when(cntMatch) {
          XSDebug("[redirect] 1\n")
          wEntry.conf := if4_rEntry.conf + 1.U
          loop_entry_is_learned := true.B
          wEntry.specCnt := 0.U
        }.otherwise {
          XSDebug("[redirect] 2\n")
          wEntry.conf := 0.U
          wEntry.specCnt := 0.U
          wEntry.tripCnt := redirect.specCnt
        }
      }.elsewhen(if4_rEntry.isUnconf) {
        when(cntMatch) {
          XSDebug("[redirect] 3\n")
          wEntry.conf := 1.U
          wEntry.age := 7.U
          wEntry.specCnt := 0.U
        }.otherwise {
          XSDebug("[redirect] 4\n")
          wEntry.tripCnt := redirect.specCnt
          wEntry.age := 7.U
          wEntry.specCnt := 0.U
        }
      }
    }.otherwise {
      when(if4_rEntry.isLearned) {
        XSDebug("[redirect] 5\n")
        // do nothing? or release this entry
        loop_learned_entry_conflict := true.B
      }.elsewhen(if4_rEntry.isConf) {
        when(if4_rEntry.age === 0.U) {
          XSDebug("[redirect] 6\n")
          wEntry.tag := redirectTag
          loop_conf_entry_evicted := true.B
          wEntry.conf := 1.U
          wEntry.specCnt := 0.U
          wEntry.tripCnt := redirect.specCnt
        }.otherwise {
          XSDebug("[redirect] 7\n")
          wEntry.age := if4_rEntry.age - 1.U
        }
      }.elsewhen(if4_rEntry.isUnconf) {
        XSDebug("[redirect] 8\n")
        wEntry.tag := redirectTag
        wEntry.conf := 1.U
        wEntry.age := 7.U
        wEntry.specCnt := 0.U
        wEntry.tripCnt := redirect.specCnt
      }
    }
  }.elsewhen(redirectValid && !doingReset){
    XSDebug("[redirect] 9\n")
    wen := true.B
    wEntry.specCnt := redirect.specCnt
  }

  when(io.repair) {
    wEntry.specCnt := redirect.specCnt
    wen := true.B
  }.elsewhen(redirectValid && redirect.mispred) {
    wEntry.specCnt := 0.U
    wen := true.B
  }

  // Bypass
  when(io.if3_fire) {
    when(swen && if3_idx === if4_idx) {
      XSDebug("Bypass swEntry\n")
      if4_entry := swEntry
    }.elsewhen(wen && if3_idx === if4_rIdx) {
      XSDebug("Bypass wEntry\n")
      if4_entry := wEntry
    }.otherwise {
      if4_entry := if3_entry
    }
  }.otherwise {
    when(swen) {
      XSDebug("spec Update\n")
      if4_entry := swEntry
    }.elsewhen(wen && if4_idx === if4_rIdx) {
      XSDebug("Keeping\n")
      if4_entry := wEntry
    }
  }

  // if4_uEntry bypass
  // when(io.if3_fire) {
    when(swen && updateIdx === if4_idx) {
      XSDebug("nSpec Bypass swEntry\n")
      if4_rEntry := swEntry
    }.elsewhen(wen && updateIdx === if4_rIdx) {
      XSDebug("nSpec Bypass wEntry\n")
      if4_rEntry := wEntry
    }.otherwise {
      if4_rEntry := if3_rEntry
    }

  when (doingReset) {
    ltb.write(resetIdx, 0.U.asTypeOf(new LoopEntry))
  }

  if (BPUDebug && debug) {
    // Perf counters
    XSPerf("loop_entry_is_learned ", loop_entry_is_learned)
    XSPerf("loop_learned_entry_conflict ", loop_learned_entry_conflict)
    XSPerf("loop_conf_entry_evicted ", loop_conf_entry_evicted)

    //debug info
    XSDebug(doingReset, "Reseting...\n")
    XSDebug(io.repair, "Repair...\n")
    XSDebug("if3_fire=%d if4_fire=%d valid=%d\n", io.if3_fire, io.if4_fire,valid)
    XSDebug("[req] v=%d pc=%x idx=%x tag=%x\n", valid, if3_pc, if3_idx, if3_tag)
    XSDebug("[if4_entry] tag=%x conf=%d age=%d tripCnt=%d specCnt=%d\n", 
      if4_entry.tag, if4_entry.conf, if4_entry.age, if4_entry.tripCnt, if4_entry.specCnt)
    XSDebug("[if3_entry] tag=%x conf=%d age=%d tripCnt=%d specCnt=%d\n", 
      if3_entry.tag, if3_entry.conf, if3_entry.age, if3_entry.tripCnt, if3_entry.specCnt)
    // XSDebug(false, true.B, p"unusable=${if4_entry.unusable}\n")

    // XSDebug("[specTable] ")
    // for(i <- 0 until nRows) {
    //   XSDebug(false, true.B, "[i:%d v:%d tag:%x] ", i.U, specTable(i).v, specTable(i).tag)
    // }
    // XSDebug(false, true.B, "\n")

    XSDebug("swen=%d, wen=%d, ltb.swIdx, io.if4_fire=%d, if4_entry.tag=%x, if4_tag=%x, io.outMask=%d\n", swen, wen, io.if4_fire, if4_entry.tag, if4_tag, io.outMask)
    XSDebug(io.if4_fire && if4_entry.tag === if4_tag && io.outMask, "[speculative update] new specCnt=%d\n",
      Mux((if4_entry.specCnt + 1.U) === if4_entry.tripCnt, 0.U, if4_entry.specCnt + 1.U))
    XSDebug("[if3_update] v=%d pc=%x idx=%x tag=%x\n", io.update.valid, io.update.bits.pc, updateIdx, ltbAddr.getTag(io.update.bits.pc)(tagLen - 1, 0))
    XSDebug("[if4_update] v=%d pc=%x\n", updateValid, updatePC)

    XSDebug("[if3_redirect] v=%d misPred=%d pc=%x idx=%x specCnt=%d taken=%d\n", io.redirect.valid, io.redirect.bits.mispred, io.redirect.bits.pc, redirectIdx, io.redirect.bits.specCnt, io.redirect.bits.taken)
    XSDebug("[if4_redirect] v=%d misPred=%d pc=%x idx=%x tag=%x specCnt=%d taken=%d tagMatch=%d cntMatch=%d\n", redirectValid, redirect.mispred, redirectPC, if4_uIdx, if4_uTag, redirect.specCnt, redirect.taken, tagMatch, cntMatch)
    XSDebug("[if3_rEntry] tag=%x conf=%d age=%d tripCnt=%d specCnt=%d\n", if3_rEntry.tag, if3_rEntry.conf, if3_rEntry.age, if3_rEntry.tripCnt, if3_rEntry.specCnt)
    // XSDebug(false, true.B, p"unusable=${if3_rEntry.unusable}\n")
    XSDebug("[if4_rEntry] tag=%x conf=%d age=%d tripCnt=%d specCnt=%d\n", if4_rEntry.tag, if4_rEntry.conf, if4_rEntry.age, if4_rEntry.tripCnt, if4_rEntry.specCnt)
    // XSDebug(false, true.B, p"unusable=${if4_rEntry.unusable}\n")
    XSDebug("[wEntry] tag=%x conf=%d age=%d tripCnt=%d specCnt=%d\n", wEntry.tag, wEntry.conf, wEntry.age, wEntry.tripCnt, wEntry.specCnt)
    // XSDebug(false, true.B, p"unusable=${wEntry.unusable}\n")
    XSDebug(io.redirect.valid && io.redirect.bits.mispred || io.repair, "MisPred or repairing, all of the nSpecCnts copy their values into the specCnts\n")
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
    val redirect =  Flipped(ValidIO(new Redirect))
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

  val updateValid = io.update.valid
  val update = io.update.bits

  val redirectValid = io.redirect.valid
  val redirect = io.redirect.bits.cfiUpdate
  val redirectPC = redirect.pc
  val redirectBank = ltbAddr.getBank(redirectPC)
 
  // 只要把同一个packAligned PC的每一项传进16个ltb中即可
  val packetAlignedPC = packetAligned(pc)

  for (i <- 0 until PredictWidth) {
    ltbs(i).io.if2_fire := io.pc.valid
    ltbs(i).io.if3_fire := io.if3_fire
    ltbs(i).io.if4_fire := out_fire
    ltbs(i).io.req.idx := bankIdx
    ltbs(i).io.req.tag := tag
    // ltbs(i).io.outMask := outMask(i)

    ltbs(i).io.update.valid := updateValid && !update.mispred(i) && update.br_mask(i)
    ltbs(i).io.update.bits.pc := update.ftqPC

    ltbs(i).io.redirect.valid := redirectValid && redirect.pd.isBr && redirectBank === i.U
    ltbs(i).io.redirect.bits.pc := redirectPC
    ltbs(i).io.redirect.bits.specCnt := redirect.specCnt(i)
    ltbs(i).io.redirect.bits.mispred := redirect.isMisPred
    ltbs(i).io.redirect.bits.taken := redirect.taken
    ltbs(i).io.redirect.bits.isReplay := io.redirect.bits.flushItself

    ltbs(i).io.repair := redirectValid && redirectBank =/= i.U
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
    io.resp.exit(i) := ltbResps(i).exit && ctrl.loop_enable
    io.meta.specCnts(i) := ltbResps(i).specCnt
  }

  XSPerf("LoopExit", io.resp.exit.reduce(_||_))

  if (BPUDebug && debug) {
    // debug info
    XSDebug("[IF2][req] fire=%d fetchpc=%x\n", if2_fire, io.pc.bits)
    XSDebug("[IF3][req] fire=%d fetchpc=%x\n", if3_fire, pc)
    XSDebug("[IF4][req] fire=%d bank=%d packetAlignedPC=%x bankIdx=%x tag=%x\n", out_fire, bank, packetAlignedPC, bankIdx, tag)
    XSDebug("[IF4][req] inMask=%b\n", inMask)

    XSDebug("[IF4][req] updatePC=%x, updateValid=%d, isBr=%b\n", update.ftqPC, updateValid, update.br_mask.asUInt)
    XSDebug("[IF4][req] redirectPC=%x redirectBank=%d, redirectValid=%d, isBr=%d, isReplay=%d\n", redirect.pc, redirectBank, redirectValid, redirect.pd.isBr, io.redirect.bits.flushItself)
    XSDebug("[IF4][req] isMisPred=%d\n", redirect.isMisPred)

    XSDebug(redirectValid, "[redirect SpecCnt] ")
    for(i <- 0 until PredictWidth) {
      XSDebug(false, redirectValid, "[i:%d, %d] ", i.U, redirect.specCnt(i))
    }
    XSDebug(false, redirectValid, "\n")

    // XSDebug(false, true.B, "\n")
    for (i <- 0 until PredictWidth) {
      XSDebug(out_fire && (i.U === 0.U || i.U === 4.U || i.U === 8.U || i.U === 12.U), "[IF4][resps]")
      XSDebug(false, out_fire, "[i:%d, e:%d, s:%d] ", i.U, io.resp.exit(i), io.meta.specCnts(i))
      XSDebug(false, out_fire && (i.U === 3.U || i.U === 7.U || i.U === 11.U || i.U === 15.U), "\n")
    }
  }
}
