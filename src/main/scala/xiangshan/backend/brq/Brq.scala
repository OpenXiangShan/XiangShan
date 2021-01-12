package xiangshan.backend.brq

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.ExcitingUtils._


class BrqPtr extends CircularQueuePtr(BrqPtr.BrqSize) with HasCircularQueuePtrHelper {

  // this.age < that.age
  final def < (that: BrqPtr): Bool = {
    Mux(this.flag === that.flag,
      this.value > that.value,
      this.value < that.value
    )
  }

  def needBrFlush(redirect: Valid[Redirect]): Bool = {
    isAfter(this, redirect.bits.brTag) || (redirect.bits.flushItself() && redirect.bits.brTag === this)
  }

  def needFlush(redirect: Valid[Redirect]): Bool = {
    redirect.bits.isUnconditional() || needBrFlush(redirect)
  }

  override def toPrintable: Printable = p"f:$flag v:$value"

}

object BrqPtr extends HasXSParameter {
  def apply(f: Bool, v: UInt): BrqPtr = {
    val ptr = Wire(new BrqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class BrqEnqIO extends XSBundle {
  val needAlloc = Vec(RenameWidth, Input(Bool()))
  val req = Vec(RenameWidth, Flipped(DecoupledIO(new CtrlFlow)))
  val resp = Vec(RenameWidth, Output(new BrqPtr))
}

class BrqIO extends XSBundle{
  val redirect = Input(ValidIO(new Redirect))
  // receive branch/jump calculated target
  val exuRedirectWb = Vec(exuParameters.AluCnt + exuParameters.JmpCnt, Flipped(ValidIO(new ExuOutput)))
  // from decode, branch insts enq
  val enq = new BrqEnqIO
  // to roq
  val out = ValidIO(new ExuOutput)
  // misprediction, flush pipeline
  val redirectOut = Output(Valid(new Redirect))
  val cfiInfo = ValidIO(new CfiUpdateInfo)
  // commit cnt of branch instr
  val bcommit = Input(UInt(BrTagWidth.W))
}

class Brq extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new BrqIO)

  class BrqEntry extends Bundle {
    val ptrFlag = Bool()
    val exuOut = new ExuOutput
  }

  val s_idle :: s_wb :: Nil = Enum(2)

  // data and state
  val brQueue = Mem(BrqSize, new BrqEntry) //Reg(Vec(BrqSize, new BrqEntry))
  val stateQueue = RegInit(VecInit(Seq.fill(BrqSize)(s_idle)))

  // queue pointers
  val headPtr, tailPtr = RegInit(BrqPtr(false.B, 0.U))
  val writebackPtr = RegInit(BrqPtr(false.B, 0.U))

  val headIdx = headPtr.value
  val writebackIdx = writebackPtr.value

  /**
    * commit (dequeue): after ROB commits branch instructions, move headPtr forward
    */
  headPtr := headPtr + io.bcommit

  /**
    * write back
    */
  val wbValid = stateQueue(writebackIdx) === s_wb
  val wbEntry = brQueue(writebackIdx)
  val wbIsMisPred = wbEntry.exuOut.redirect.target =/= wbEntry.exuOut.brUpdate.pnpc

  io.redirectOut.valid := wbValid && wbIsMisPred
  io.redirectOut.bits := wbEntry.exuOut.redirect
  io.redirectOut.bits.brTag := BrqPtr(wbEntry.ptrFlag, writebackIdx)

  io.out.valid := wbValid
  io.out.bits := wbEntry.exuOut
  when (wbValid) {
    stateQueue(writebackIdx) := s_idle
    writebackPtr := writebackPtr + 1.U
  }

  val brTagRead = RegNext(Mux(io.redirect.bits.flushItself(), io.redirect.bits.brTag - 1.U, io.redirect.bits.brTag))
  io.cfiInfo.valid := RegNext(io.redirect.valid || wbValid)
  io.cfiInfo.bits := brQueue(brTagRead.value).exuOut.brUpdate
  io.cfiInfo.bits.brTag := brTagRead
  io.cfiInfo.bits.isReplay := RegNext(io.redirect.bits.flushItself())
  io.cfiInfo.bits.isMisPred := RegNext(wbIsMisPred)

  XSInfo(io.out.valid,
    p"commit branch to roq, mispred:${io.redirectOut.valid} pc=${Hexadecimal(io.out.bits.uop.cf.pc)}\n"
  )

  /**
    * branch insts enq
    */
  // note that redirect sent to IFU is delayed for one clock cycle
  // thus, brq should not allow enqueue in the next cycle after redirect
  val lastCycleRedirect = RegNext(io.redirect.valid)
  val validEntries = distanceBetween(tailPtr, headPtr)
  for(i <- 0 until DecodeWidth){
    val offset = if (i == 0) 0.U else PopCount(io.enq.needAlloc.take(i))
    val brTag = tailPtr + offset
    val idx = brTag.value
    io.enq.req(i).ready := validEntries <= (BrqSize - (i + 1)).U && !lastCycleRedirect
    io.enq.resp(i) := brTag
    when (io.enq.req(i).fire()) {
      brQueue(idx).ptrFlag := brTag.flag
      brQueue(idx).exuOut.brUpdate.pc := io.enq.req(i).bits.pc
      brQueue(idx).exuOut.brUpdate.pnpc := io.enq.req(i).bits.brUpdate.pnpc
      brQueue(idx).exuOut.brUpdate.fetchIdx := io.enq.req(i).bits.brUpdate.fetchIdx
      brQueue(idx).exuOut.brUpdate.pd := io.enq.req(i).bits.brUpdate.pd
      brQueue(idx).exuOut.brUpdate.bpuMeta := io.enq.req(i).bits.brUpdate.bpuMeta
      stateQueue(idx) := s_idle
    }
  }
  val enqCnt = PopCount(io.enq.req.map(_.fire()))
  tailPtr := tailPtr + enqCnt

  /**
    * exu write back
    */
  for (exuWb <- io.exuRedirectWb) {
    when (exuWb.valid) {
      val wbIdx = exuWb.bits.redirect.brTag.value
      XSInfo(
        p"exu write back: brTag:${exuWb.bits.redirect.brTag}" +
        p" pc=${Hexadecimal(exuWb.bits.uop.cf.pc)} " +
        p"pnpc=${Hexadecimal(brQueue(wbIdx).exuOut.brUpdate.pnpc)} " +
        p"target=${Hexadecimal(exuWb.bits.redirect.target)}\n"
      )
      assert(stateQueue(wbIdx) === s_idle)
      stateQueue(wbIdx) := s_wb
      // only writeback necessary information
      brQueue(wbIdx).exuOut.uop := exuWb.bits.uop
      brQueue(wbIdx).exuOut.data := exuWb.bits.data
      brQueue(wbIdx).exuOut.fflags := exuWb.bits.fflags
      brQueue(wbIdx).exuOut.redirectValid := exuWb.bits.redirectValid
      brQueue(wbIdx).exuOut.redirect := exuWb.bits.redirect
      brQueue(wbIdx).exuOut.debug := exuWb.bits.debug
      brQueue(wbIdx).exuOut.brUpdate.target := exuWb.bits.brUpdate.target
      brQueue(wbIdx).exuOut.brUpdate.brTarget := exuWb.bits.brUpdate.brTarget
      brQueue(wbIdx).exuOut.brUpdate.taken := exuWb.bits.brUpdate.taken
    }
  }

  // when redirect is valid, we need to update the states and pointers
  when (io.redirect.valid) {
    // For unconditional redirect, flush all entries
    when (io.redirect.bits.isUnconditional()) {
      stateQueue.foreach(_ := s_idle)
      headPtr := BrqPtr(false.B, 0.U)
      tailPtr := BrqPtr(false.B, 0.U)
      writebackPtr := BrqPtr(false.B, 0.U)
    }.otherwise {
      // conditional check: branch mis-prediction and memory dependence violation
      stateQueue.zipWithIndex.foreach({ case(s, i) =>
        val ptr = BrqPtr(brQueue(i).ptrFlag, i.U)
        when (ptr.needBrFlush(io.redirect)) {
          s := s_idle
        }
      })
      tailPtr := io.redirect.bits.brTag + Mux(io.redirect.bits.flushItself(), 0.U, 1.U)
      when (io.redirect.bits.flushItself() && writebackPtr.needBrFlush(io.redirect)) {
        writebackPtr := io.redirect.bits.brTag
      }
    }
  }

  // Debug info
  val debug_roq_redirect = io.redirect.valid && io.redirect.bits.isUnconditional()
  val debug_brq_redirect = io.redirectOut.valid
  val debug_normal_mode = !(debug_roq_redirect || debug_brq_redirect)

  for(i <- 0 until DecodeWidth){
    XSDebug(
      debug_normal_mode,
      p"enq v:${io.enq.req(i).valid} rdy:${io.enq.req(i).ready} pc:${Hexadecimal(io.enq.req(i).bits.pc)}" +
        p" brTag:${io.enq.resp(i)}\n"
    )
  }

  XSInfo(debug_roq_redirect, "roq redirect, flush brq\n")
  XSInfo(debug_brq_redirect, p"brq redirect, target:${Hexadecimal(io.redirectOut.bits.target)}\n")
  XSDebug(io.cfiInfo.valid, "inOrderValid: pc=%x\n", io.cfiInfo.bits.pc)

  XSDebug(p"headIdx:$headIdx writebackIdx:$writebackIdx\n")
  XSDebug(p"headPtr:$headPtr tailPtr:$tailPtr\n")
  XSDebug("")
  stateQueue.reverse.map(s =>{
    XSDebug(false, s === s_idle, "-")
    XSDebug(false, s === s_wb, "w")
  })
  XSDebug(false, true.B, "\n")

  val fire = io.out.fire()
  val predRight = fire && !wbIsMisPred
  val predWrong = fire && wbIsMisPred
  // val isBType = wbEntry.exuOut.brUpdate.btbType===BTBtype.B
  val isBType = wbEntry.exuOut.brUpdate.pd.isBr
  // val isJType = wbEntry.exuOut.brUpdate.btbType===BTBtype.J
  val isJType = wbEntry.exuOut.brUpdate.pd.isJal
  // val isIType = wbEntry.exuOut.brUpdate.btbType===BTBtype.I
  val isIType = wbEntry.exuOut.brUpdate.pd.isJalr
  // val isRType = wbEntry.exuOut.brUpdate.btbType===BTBtype.R
  val isRType = wbEntry.exuOut.brUpdate.pd.isRet
  val mbpInstr = fire
  val mbpRight = predRight
  val mbpWrong = predWrong
  val mbpBRight = predRight && isBType
  val mbpBWrong = predWrong && isBType
  val mbpJRight = predRight && isJType
  val mbpJWrong = predWrong && isJType
  val mbpIRight = predRight && isIType
  val mbpIWrong = predWrong && isIType
  val mbpRRight = predRight && isRType
  val mbpRWrong = predWrong && isRType

  if(!env.FPGAPlatform){
    ExcitingUtils.addSource(mbpInstr, "perfCntCondMbpInstr", Perf)
    ExcitingUtils.addSource(mbpRight, "perfCntCondMbpRight", Perf)
    ExcitingUtils.addSource(mbpWrong, "perfCntCondMbpWrong", Perf)
    ExcitingUtils.addSource(mbpBRight, "perfCntCondMbpBRight", Perf)
    ExcitingUtils.addSource(mbpBWrong, "perfCntCondMbpBWrong", Perf)
    ExcitingUtils.addSource(mbpJRight, "perfCntCondMbpJRight", Perf)
    ExcitingUtils.addSource(mbpJWrong, "perfCntCondMbpJWrong", Perf)
    ExcitingUtils.addSource(mbpIRight, "perfCntCondMbpIRight", Perf)
    ExcitingUtils.addSource(mbpIWrong, "perfCntCondMbpIWrong", Perf)
    ExcitingUtils.addSource(mbpRRight, "perfCntCondMbpRRight", Perf)
    ExcitingUtils.addSource(mbpRWrong, "perfCntCondMbpRWrong", Perf)
  }

  val utilization = Mux(headPtr.flag === tailPtr.flag, tailPtr.value - headPtr.value, BrqSize.U + tailPtr.value - headPtr.value)
  XSPerf("utilization", utilization)
  XSPerf("mbpInstr", PopCount(mbpInstr))
  XSPerf("mbpRight", PopCount(mbpRight))
  XSPerf("mbpWrong", PopCount(mbpWrong))
  XSPerf("mbpBRight", PopCount(mbpBRight))
  XSPerf("mbpBWrong", PopCount(mbpBWrong))
  XSPerf("mbpJRight", PopCount(mbpJRight))
  XSPerf("mbpJWrong", PopCount(mbpJWrong))
  XSPerf("mbpIRight", PopCount(mbpIRight))
  XSPerf("mbpIWrong", PopCount(mbpIWrong))
  XSPerf("mbpRRight", PopCount(mbpRRight))
  XSPerf("mbpRWrong", PopCount(mbpRWrong))
}
