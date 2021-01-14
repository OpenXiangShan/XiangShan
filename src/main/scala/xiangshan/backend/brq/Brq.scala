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
  val decodeData = Module(new SyncDataModuleTemplate(new ExuOutput, BrqSize, 2, DecodeWidth))
  val writebackData = Module(new SyncDataModuleTemplate(new ExuOutput, BrqSize, 2, exuParameters.AluCnt + exuParameters.JmpCnt))
  val ptrFlagVec = Reg(Vec(BrqSize, Bool()))
  val stateQueue = RegInit(VecInit(Seq.fill(BrqSize)(s_idle)))

  // queue pointers
  val headPtr, tailPtr = RegInit(BrqPtr(false.B, 0.U))
  val writebackPtr = RegInit(BrqPtr(false.B, 0.U))
  val writebackPtr_next = WireInit(writebackPtr)
  writebackPtr := writebackPtr_next

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
  val wbEntry = Wire(new ExuOutput)
  val wbIsMisPred = wbEntry.redirect.target =/= wbEntry.brUpdate.pnpc

  io.redirectOut.valid := wbValid && wbIsMisPred
  io.redirectOut.bits := wbEntry.redirect
  io.redirectOut.bits.brTag := BrqPtr(ptrFlagVec(writebackIdx), writebackIdx)

  io.out.valid := wbValid
  io.out.bits := wbEntry
  when (wbValid) {
    stateQueue(writebackIdx) := s_idle
    writebackPtr_next := writebackPtr + 1.U
  }

  val brUpdateReadIdx = Mux(io.redirect.bits.flushItself(), io.redirect.bits.brTag - 1.U, io.redirect.bits.brTag)
  val brUpdateReadEntry = Wire(new ExuOutput)
  io.cfiInfo.valid := RegNext(io.redirect.valid || wbValid)
  io.cfiInfo.bits := brUpdateReadEntry.brUpdate
  io.cfiInfo.bits.brTag := RegNext(brUpdateReadIdx)
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
  val enqBrTag = VecInit((0 until DecodeWidth).map(i => tailPtr + PopCount(io.enq.needAlloc.take(i))))

  io.enq.resp := enqBrTag

  for (i <- 0 until DecodeWidth) {
    val idx = enqBrTag(i).value
    io.enq.req(i).ready := validEntries <= (BrqSize - (i + 1)).U && !lastCycleRedirect
    when (io.enq.req(i).fire()) {
      ptrFlagVec(idx) := enqBrTag(i).flag
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
        // p"pnpc=${Hexadecimal(brQueue(wbIdx).exuOut.brUpdate.pnpc)} " +
        p"target=${Hexadecimal(exuWb.bits.redirect.target)}\n"
      )
      assert(stateQueue(wbIdx) === s_idle)

      stateQueue(wbIdx) := s_wb
    }
  }

  // when redirect is valid, we need to update the states and pointers
  when (io.redirect.valid) {
    // For unconditional redirect, flush all entries
    when (io.redirect.bits.isUnconditional()) {
      stateQueue.foreach(_ := s_idle)
      headPtr := BrqPtr(false.B, 0.U)
      tailPtr := BrqPtr(false.B, 0.U)
      writebackPtr_next := BrqPtr(false.B, 0.U)
    }.otherwise {
      // conditional check: branch mis-prediction and memory dependence violation
      stateQueue.zipWithIndex.foreach({ case(s, i) =>
        val ptr = BrqPtr(ptrFlagVec(i), i.U)
        when (ptr.needBrFlush(io.redirect)) {
          s := s_idle
        }
      })
      tailPtr := io.redirect.bits.brTag + Mux(io.redirect.bits.flushItself(), 0.U, 1.U)
      when (io.redirect.bits.flushItself() && writebackPtr.needBrFlush(io.redirect)) {
        writebackPtr_next := io.redirect.bits.brTag
      }
    }
  }

  def mergeDecodeWbData(dec: ExuOutput, wb: ExuOutput) : ExuOutput = {
    val mergeData = Wire(new ExuOutput)
    mergeData := dec
    // only writeback necessary information
    mergeData.uop := wb.uop
    mergeData.data := wb.data
    mergeData.fflags := wb.fflags
    mergeData.redirectValid := wb.redirectValid
    mergeData.redirect := wb.redirect
    mergeData.debug := wb.debug
    mergeData.brUpdate.target := wb.brUpdate.target
    mergeData.brUpdate.brTarget := wb.brUpdate.brTarget
    mergeData.brUpdate.taken := wb.brUpdate.taken
    mergeData
  }

  decodeData.io.raddr(0) := writebackPtr_next.value
  decodeData.io.raddr(1) := brUpdateReadIdx.value
  decodeData.io.wen := VecInit(io.enq.req.map(_.fire()))
  decodeData.io.waddr := VecInit(enqBrTag.map(_.value))
  decodeData.io.wdata.zip(io.enq.req).map{ case (wdata, req) => {
    wdata := DontCare
    wdata.brUpdate := req.bits.brUpdate
    wdata.brUpdate.pc := req.bits.pc
  }}

  writebackData.io.raddr(0) := writebackPtr_next.value
  writebackData.io.raddr(1) := brUpdateReadIdx.value
  writebackData.io.wen := VecInit(io.exuRedirectWb.map(_.valid))
  writebackData.io.waddr := VecInit(io.exuRedirectWb.map(_.bits.redirect.brTag.value))
  writebackData.io.wdata := VecInit(io.exuRedirectWb.map(_.bits))

  wbEntry := mergeDecodeWbData(decodeData.io.rdata(0), writebackData.io.rdata(0))
  brUpdateReadEntry := mergeDecodeWbData(decodeData.io.rdata(1), writebackData.io.rdata(1))


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
  // val isBType = wbEntry.brUpdate.btbType===BTBtype.B
  val isBType = wbEntry.brUpdate.pd.isBr
  // val isJType = wbEntry.brUpdate.btbType===BTBtype.J
  val isJType = wbEntry.brUpdate.pd.isJal
  // val isIType = wbEntry.brUpdate.btbType===BTBtype.I
  val isIType = wbEntry.brUpdate.pd.isJalr
  // val isRType = wbEntry.brUpdate.btbType===BTBtype.R
  val isRType = wbEntry.brUpdate.pd.isRet
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
    ExcitingUtils.addSource(mbpInstr, "perfCntCondBpInstr", Perf)
    ExcitingUtils.addSource(mbpRight, "perfCntCondBpRight", Perf)
    ExcitingUtils.addSource(mbpWrong, "perfCntCondBpWrong", Perf)
    ExcitingUtils.addSource(mbpBRight, "perfCntCondBpBRight", Perf)
    ExcitingUtils.addSource(mbpBWrong, "perfCntCondBpBWrong", Perf)
    ExcitingUtils.addSource(mbpJRight, "perfCntCondBpJRight", Perf)
    ExcitingUtils.addSource(mbpJWrong, "perfCntCondBpJWrong", Perf)
    ExcitingUtils.addSource(mbpIRight, "perfCntCondBpIRight", Perf)
    ExcitingUtils.addSource(mbpIWrong, "perfCntCondBpIWrong", Perf)
    ExcitingUtils.addSource(mbpRRight, "perfCntCondBpRRight", Perf)
    ExcitingUtils.addSource(mbpRWrong, "perfCntCondBpRWrong", Perf)
  }
}
