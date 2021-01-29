package xiangshan.backend.brq

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.ExcitingUtils._
import xiangshan.backend.JumpOpType
import xiangshan.backend.decode.ImmUnion


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

class BrqPcRead extends XSBundle {
  val brqIdx = Input(new BrqPtr)
  val pc = Output(UInt(VAddrBits.W))
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
  // read pc for jump unit
  val pcReadReq = new BrqPcRead
}

class Brq extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new BrqIO)

  class BrqEntry extends Bundle {
    val ptrFlag = Bool()
    val exuOut = new ExuOutput
  }

  val s_idle :: s_wb :: s_auipc_wb :: Nil = Enum(3)

  class DecodeEnqBrqData extends Bundle {
    val cfiUpdateInfo = new CfiUpdateInfo
    // we use this to calculate branch target
    val imm12 = UInt(12.W)
  }

  // data and state
  val decodeData = Module(new SyncDataModuleTemplate(new DecodeEnqBrqData, BrqSize, 3, DecodeWidth))
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
  val wbState = stateQueue(writebackIdx)
  val wbValid = wbState === s_wb
  val wbIsAuipc = wbState === s_auipc_wb
  val wbEntry = Wire(new ExuOutput)
  val wbIsMisPred = wbEntry.redirect.target =/= wbEntry.brUpdate.pnpc

  io.redirectOut.valid := wbValid && wbIsMisPred
  io.redirectOut.bits := wbEntry.redirect
  io.redirectOut.bits.level := RedirectLevel.flushAfter
  io.redirectOut.bits.brTag := BrqPtr(ptrFlagVec(writebackIdx), writebackIdx)

  io.out.valid := wbValid || wbIsAuipc
  io.out.bits := wbEntry
  when (io.out.valid) {
    stateQueue(writebackIdx) := s_idle
    writebackPtr_next := writebackPtr + 1.U
  }

  val brUpdateReadIdx = Mux(io.redirect.bits.flushItself(), io.redirect.bits.brTag - 1.U, io.redirect.bits.brTag)
  val brUpdateReadEntry = Wire(new CfiUpdateInfo)
  io.cfiInfo.valid := RegNext(io.redirect.valid || wbValid)
  io.cfiInfo.bits := brUpdateReadEntry
  io.cfiInfo.bits.target := RegNext(Mux(io.redirect.bits.flushItself(),
    io.redirect.bits.target,
    wbEntry.brUpdate.target
  ))
  io.cfiInfo.bits.brTarget := io.cfiInfo.bits.target
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
  for ((exuWb, i) <- io.exuRedirectWb.zipWithIndex) {
    when (exuWb.valid) {
      val wbIdx = exuWb.bits.redirect.brTag.value
      XSInfo(
        p"exu write back: brTag:${exuWb.bits.redirect.brTag}" +
        p" pc=${Hexadecimal(exuWb.bits.uop.cf.pc)} " +
        // p"pnpc=${Hexadecimal(brQueue(wbIdx).exuOut.brUpdate.pnpc)} " +
        p"target=${Hexadecimal(exuWb.bits.redirect.target)}\n"
      )
      assert(stateQueue(wbIdx) === s_idle)
      if(i == 0){ // jump
        stateQueue(wbIdx) := Mux(JumpOpType.jumpOpisAuipc(exuWb.bits.uop.ctrl.fuOpType),
          s_auipc_wb,
          s_wb
        )
      } else { // alu
        stateQueue(wbIdx) := s_wb
      }
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

  def mergeWbEntry(dec: DecodeEnqBrqData, wb: ExuOutput) : ExuOutput = {
    val mergeData = Wire(new ExuOutput)
    // only writeback necessary information
    mergeData.uop := wb.uop
    mergeData.data := wb.data
    mergeData.fflags := wb.fflags
    mergeData.redirectValid := wb.redirectValid

    // calculate target pc
    val pc = dec.cfiUpdateInfo.pc
    val offset = SignExt(ImmUnion.B.toImm32(dec.imm12), VAddrBits)
    val snpc = pc + Mux(dec.cfiUpdateInfo.pd.isRVC, 2.U, 4.U)
    val bnpc = pc + offset
    val branch_pc = Mux(wb.brUpdate.taken, bnpc, snpc)
    val redirectTarget = Mux(dec.cfiUpdateInfo.pd.isBr, branch_pc, wb.redirect.target)

    mergeData.redirect := wb.redirect
    mergeData.redirect.target := redirectTarget
    mergeData.debug := wb.debug
    mergeData.brUpdate := dec.cfiUpdateInfo
    mergeData.brUpdate.target := redirectTarget
    mergeData.brUpdate.brTarget := redirectTarget
    mergeData.brUpdate.taken := wb.brUpdate.taken
    mergeData.brUpdate.bpuMeta.predictor:= wb.brUpdate.bpuMeta.predictor
    mergeData
  }

  def mergeBrUpdateEntry(dec: DecodeEnqBrqData, wb: ExuOutput): CfiUpdateInfo = {
    val mergeData = WireInit(dec.cfiUpdateInfo)
    mergeData.taken := wb.brUpdate.taken
    mergeData
  }

  decodeData.io.raddr(0) := writebackPtr_next.value
  decodeData.io.raddr(1) := brUpdateReadIdx.value
  decodeData.io.raddr(2) := io.pcReadReq.brqIdx.value
  decodeData.io.wen := VecInit(io.enq.req.map(_.fire()))
  decodeData.io.waddr := VecInit(enqBrTag.map(_.value))
  decodeData.io.wdata.zip(io.enq.req).foreach{ case (wdata, req) =>
    wdata.cfiUpdateInfo := req.bits.brUpdate
    wdata.cfiUpdateInfo.pc := req.bits.pc
    wdata.imm12 := ImmUnion.B.minBitsFromInstr(req.bits.instr)
  }

  writebackData.io.raddr(0) := writebackPtr_next.value
  writebackData.io.raddr(1) := brUpdateReadIdx.value
  writebackData.io.wen := VecInit(io.exuRedirectWb.map(_.valid))
  writebackData.io.waddr := VecInit(io.exuRedirectWb.map(_.bits.redirect.brTag.value))
  writebackData.io.wdata := VecInit(io.exuRedirectWb.map(_.bits))

  wbEntry := mergeWbEntry(decodeData.io.rdata(0), writebackData.io.rdata(0))
  brUpdateReadEntry := mergeBrUpdateEntry(decodeData.io.rdata(1), writebackData.io.rdata(1))

  io.pcReadReq.pc := decodeData.io.rdata(2).cfiUpdateInfo.pc

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

  if(!env.FPGAPlatform && env.EnablePerfDebug) {
    val predictor = io.cfiInfo.bits.bpuMeta.predictor

    val cfiCountValid = io.cfiInfo.valid && !io.cfiInfo.bits.isReplay

    val ubtbAns = io.cfiInfo.bits.bpuMeta.ubtbAns
    val btbAns = io.cfiInfo.bits.bpuMeta.btbAns
    val tageAns = io.cfiInfo.bits.bpuMeta.tageAns
    val rasAns = io.cfiInfo.bits.bpuMeta.rasAns
    val loopAns = io.cfiInfo.bits.bpuMeta.loopAns

    // Pipeline stage counter
    val s1Right =  cfiCountValid && !io.cfiInfo.bits.isMisPred && predictor === 0.U
    val s1Wrong =  cfiCountValid && io.cfiInfo.bits.isMisPred && predictor === 0.U

    val s2Right  =  cfiCountValid && !io.cfiInfo.bits.isMisPred && predictor === 1.U
    val s2Wrong  =  cfiCountValid && io.cfiInfo.bits.isMisPred && predictor === 1.U

    val s3Right =  cfiCountValid && !io.cfiInfo.bits.isMisPred && predictor === 2.U
    val s3Wrong =  cfiCountValid && io.cfiInfo.bits.isMisPred && predictor === 2.U

    // Predictor counter
    // val ubtbRight = cfiCountValid && ubtbAns.hit && io.cfiInfo.bits.target === ubtbAns.target && io.cfiInfo.bits.taken === ubtbAns.taken
    // val ubtbWrong = cfiCountValid && ubtbAns.hit && (io.cfiInfo.bits.target =/= ubtbAns.target || io.cfiInfo.bits.taken =/= ubtbAns.taken)

    val ubtbRight = cfiCountValid && ubtbAns.hit && Mux(ubtbAns.taken, 
      io.cfiInfo.bits.target === ubtbAns.target && io.cfiInfo.bits.taken === ubtbAns.taken, // taken
      io.cfiInfo.bits.taken === ubtbAns.taken) // noTaken
    val ubtbWrong = cfiCountValid && ubtbAns.hit && Mux(ubtbAns.taken, 
      io.cfiInfo.bits.target =/= ubtbAns.target || io.cfiInfo.bits.taken =/= ubtbAns.taken, // taken
      io.cfiInfo.bits.taken =/= ubtbAns.taken) // noTaken

    val takenAndRight = ubtbAns.taken && ubtbRight
    val takenButWrong = ubtbAns.taken && ubtbWrong

    // val btbRight = cfiCountValid && btbAns.hit && io.cfiInfo.bits.target === btbAns.target && io.cfiInfo.bits.taken === btbAns.taken
    // val btbWrong = cfiCountValid && btbAns.hit && (io.cfiInfo.bits.target =/= btbAns.target || io.cfiInfo.bits.taken =/= btbAns.taken)

    val btbRight = cfiCountValid && btbAns.hit && Mux(btbAns.taken, 
      io.cfiInfo.bits.target === btbAns.target && io.cfiInfo.bits.taken === btbAns.taken, // taken
      io.cfiInfo.bits.taken === btbAns.taken) // noTaken
    val btbWrong = cfiCountValid && btbAns.hit && Mux(btbAns.taken, 
      io.cfiInfo.bits.target =/= btbAns.target || io.cfiInfo.bits.taken =/= btbAns.taken, // taken
      io.cfiInfo.bits.taken =/= btbAns.taken) // noTaken

    val tageRight = cfiCountValid && io.cfiInfo.bits.pd.brType =/= "b10".U && io.cfiInfo.bits.taken === tageAns.taken // DontCare jal
    val tageWrong = cfiCountValid && io.cfiInfo.bits.pd.brType =/= "b10".U && io.cfiInfo.bits.taken =/= tageAns.taken // DontCare jal

    val rasRight = cfiCountValid && io.cfiInfo.bits.pd.isRet && rasAns.hit && io.cfiInfo.bits.target === rasAns.target
    val rasWrong = cfiCountValid && io.cfiInfo.bits.pd.isRet && rasAns.hit && io.cfiInfo.bits.target =/= rasAns.target

    val loopRight = cfiCountValid && loopAns.hit && io.cfiInfo.bits.taken === loopAns.taken
    val loopWrong = cfiCountValid && loopAns.hit && io.cfiInfo.bits.taken =/= loopAns.taken

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

    ExcitingUtils.addSource(s1Right, "perfCntS1Right", Perf)
    ExcitingUtils.addSource(s1Wrong, "perfCntS1Wrong", Perf)
    ExcitingUtils.addSource(s2Right, "perfCntS2Right", Perf)
    ExcitingUtils.addSource(s2Wrong, "perfCntS2Wrong", Perf)
    ExcitingUtils.addSource(s3Right, "perfCntS3Right", Perf)
    ExcitingUtils.addSource(s3Wrong, "perfCntS3Wrong", Perf)
    
    ExcitingUtils.addSource(ubtbRight, "perfCntubtbRight", Perf)
    ExcitingUtils.addSource(ubtbWrong, "perfCntubtbWrong", Perf)
    ExcitingUtils.addSource(btbRight, "perfCntbtbRight", Perf)
    ExcitingUtils.addSource(btbWrong, "perfCntbtbWrong", Perf)
    ExcitingUtils.addSource(tageRight, "perfCnttageRight", Perf)
    ExcitingUtils.addSource(tageWrong, "perfCnttageWrong", Perf)
    ExcitingUtils.addSource(rasRight, "perfCntrasRight", Perf)
    ExcitingUtils.addSource(rasWrong, "perfCntrasWrong", Perf)
    ExcitingUtils.addSource(loopRight, "perfCntloopRight", Perf)
    ExcitingUtils.addSource(loopWrong, "perfCntloopWrong", Perf)

    ExcitingUtils.addSource(takenAndRight, "perfCntTakenAndRight", Perf)
    ExcitingUtils.addSource(takenButWrong, "perfCntTakenButWrong", Perf)
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
