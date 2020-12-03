package xiangshan.backend.brq

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.ExcitingUtils._


class BrqPtr extends CircularQueuePtr(BrqPtr.BrqSize) {

  // this.age < that.age
  final def < (that: BrqPtr): Bool = {
    Mux(this.flag === that.flag,
      this.value > that.value,
      this.value < that.value
    )
  }

  def needBrFlush(redirectTag: BrqPtr): Bool = this < redirectTag

  def needFlush(redirect: Valid[Redirect]): Bool = {
    redirect.valid && (redirect.bits.isException || redirect.bits.isFlushPipe || needBrFlush(redirect.bits.brTag)) //TODO: discuss if (isException || isFlushPipe) need here?
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

class BrqIO extends XSBundle{
  // interrupt/exception happen, flush Brq
  val roqRedirect = Input(Valid(new Redirect))
  // mem replay
  val memRedirect = Input(Valid(new Redirect))
  // receive branch/jump calculated target
  val exuRedirect = Vec(exuParameters.AluCnt + exuParameters.JmpCnt, Flipped(ValidIO(new ExuOutput)))
  // from decode, branch insts enq
  val enqReqs = Vec(DecodeWidth, Flipped(DecoupledIO(new CfCtrl)))
  // to decode
  val brTags = Output(Vec(DecodeWidth, new BrqPtr))
  // to roq
  val out = ValidIO(new ExuOutput)
  // misprediction, flush pipeline
  val redirect = Output(Valid(new Redirect))
  val outOfOrderBrInfo = ValidIO(new BranchUpdateInfo)
  // commit cnt of branch instr
  val bcommit = Input(UInt(BrTagWidth.W))
  // in order dequeue to train bpd
  val inOrderBrInfo = ValidIO(new BranchUpdateInfo)
}

class Brq extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new BrqIO)

  class BrqEntry extends Bundle {
    val ptrFlag = Bool()
    val npc = UInt(VAddrBits.W)
    val exuOut = new ExuOutput
  }

  val s_invalid :: s_idle :: s_wb :: s_commited :: Nil  =
    List.tabulate(4)(i => (1 << i).U(4.W).asTypeOf(new StateQueueEntry))

  class StateQueueEntry extends Bundle{
    val isCommit = Bool()
    val isWb = Bool()
    val isIdle = Bool()
    val isInvalid = Bool()
  }

  val brCommitCnt = RegInit(0.U(BrTagWidth.W))
  val brQueue = Mem(BrqSize, new BrqEntry) //Reg(Vec(BrqSize, new BrqEntry))
  val stateQueue = RegInit(VecInit(Seq.fill(BrqSize)(s_invalid)))

  val headPtr, tailPtr = RegInit(BrqPtr(false.B, 0.U))

  // dequeue
  val headIdx = headPtr.value

  val skipMask = Cat(stateQueue.map(_.isCommit).reverse)

  /*
      example: headIdx       = 2
               headIdxOH     = 00000100
               headIdxMaskHI = 11111100
               headIdxMaskLo = 00000011
               skipMask      = 00111101
               commitIdxHi   =  6
               commitIdxLo   =        0
               commitIdx     =  6
   */
  val headIdxOH = UIntToOH(headIdx)
  val headIdxMaskHiVec = Wire(Vec(BrqSize, Bool()))
  for(i <- headIdxMaskHiVec.indices){
    headIdxMaskHiVec(i) := { if(i==0) headIdxOH(i) else headIdxMaskHiVec(i-1) || headIdxOH(i) }
  }
  val headIdxMaskHi = headIdxMaskHiVec.asUInt()
  val headIdxMaskLo = (~headIdxMaskHi).asUInt()

  val commitIdxHi = PriorityEncoder((~skipMask).asUInt() & headIdxMaskHi)
  val (commitIdxLo, findLo) = PriorityEncoderWithFlag((~skipMask).asUInt() & headIdxMaskLo)

  val skipHi = (skipMask | headIdxMaskLo) === Fill(BrqSize, 1.U(1.W))
  val useLo = skipHi && findLo


  val commitIdx = Mux(stateQueue(commitIdxHi).isWb,
    commitIdxHi,
    Mux(useLo && stateQueue(commitIdxLo).isWb,
      commitIdxLo,
      headIdx
    )
  )

  val deqValid = stateQueue(headIdx).isCommit && brCommitCnt=/=0.U
  val commitValid = stateQueue(commitIdx).isWb
  val commitEntry = brQueue(commitIdx)
  val commitIsMisPred = commitEntry.exuOut.redirect.isMisPred

  brCommitCnt := brCommitCnt + io.bcommit - deqValid

  XSDebug(p"brCommitCnt:$brCommitCnt\n")
  assert(brCommitCnt+io.bcommit >= deqValid)
  io.inOrderBrInfo.valid := commitValid
  io.inOrderBrInfo.bits := commitEntry.exuOut.brUpdate
  XSDebug(io.inOrderBrInfo.valid, "inOrderValid: pc=%x\n", io.inOrderBrInfo.bits.pc)

//  XSDebug(
//    p"commitIdxHi:$commitIdxHi ${Binary(headIdxMaskHi)} ${Binary(skipMask)}\n"
//  )
//  XSDebug(
//    p"commitIdxLo:$commitIdxLo ${Binary(headIdxMaskLo)} ${Binary(skipMask)}\n"
//  )
  XSDebug(p"headIdx:$headIdx commitIdx:$commitIdx\n")
  XSDebug(p"headPtr:$headPtr tailPtr:$tailPtr\n")
  XSDebug("")
  stateQueue.reverse.map(s =>{
    XSDebug(false, s.isInvalid, "-")
    XSDebug(false, s.isIdle, "i")
    XSDebug(false, s.isWb, "w")
    XSDebug(false, s.isCommit, "c")
  })
  XSDebug(false, true.B, "\n")

  val headPtrNext = WireInit(headPtr + deqValid)

  when(commitValid){
    stateQueue(commitIdx) := s_commited
  }
  when(deqValid){
    stateQueue(headIdx) := s_invalid
  }
  assert(!(commitIdx===headIdx && commitValid && deqValid), "Error: deq and commit a same entry!")

  headPtr := headPtrNext
  io.redirect.valid := commitValid &&
    commitIsMisPred &&
    !io.roqRedirect.valid &&
    !io.redirect.bits.roqIdx.needFlush(io.memRedirect)

  io.redirect.bits := commitEntry.exuOut.redirect
  io.out.valid := commitValid
  io.out.bits := commitEntry.exuOut
  io.outOfOrderBrInfo.valid := commitValid
  io.outOfOrderBrInfo.bits := commitEntry.exuOut.brUpdate

  when (io.redirect.valid) {
    commitEntry.npc := io.redirect.bits.target
  }

  XSInfo(io.out.valid,
    p"commit branch to roq, mispred:${io.redirect.valid} pc=${Hexadecimal(io.out.bits.uop.cf.pc)}\n"
  )

  // branch insts enq
  for(i <- 0 until DecodeWidth){
    val offset = if(i == 0) 0.U else PopCount(io.enqReqs.take(i).map(_.valid))
    val brTag = tailPtr + offset
    val idx = brTag.value
    io.enqReqs(i).ready := stateQueue(idx).isInvalid
    io.brTags(i) := brTag
    when(io.enqReqs(i).fire()){
      brQueue(idx).npc := io.enqReqs(i).bits.cf.brUpdate.pnpc
      brQueue(idx).ptrFlag := brTag.flag
      stateQueue(idx) := s_idle
    }
  }
  val enqCnt = PopCount(io.enqReqs.map(_.fire()))
  tailPtr := tailPtr + enqCnt

  // exu write back
  for(exuWb <- io.exuRedirect){
    when(exuWb.valid){
      val wbIdx = exuWb.bits.redirect.brTag.value
      XSInfo(
        p"exu write back: brTag:${exuWb.bits.redirect.brTag}" +
          p" pc=${Hexadecimal(exuWb.bits.uop.cf.pc)} pnpc=${Hexadecimal(brQueue(wbIdx).npc)} target=${Hexadecimal(exuWb.bits.redirect.target)}\n"
      )
      stateQueue(wbIdx) := s_wb
      val exuOut = WireInit(exuWb.bits)
      val isMisPred = brQueue(wbIdx).npc =/= exuWb.bits.redirect.target
      exuOut.redirect.isMisPred := isMisPred
      exuOut.brUpdate.isMisPred := isMisPred
      brQueue(wbIdx).exuOut := exuOut
    }
  }

  when(io.roqRedirect.valid){
    // exception
    stateQueue.foreach(_ := s_invalid)
    headPtr := BrqPtr(false.B, 0.U)
    tailPtr := BrqPtr(false.B, 0.U)
    brCommitCnt := 0.U
  }.elsewhen(io.redirect.valid || io.memRedirect.valid){
    // misprediction or replay
    stateQueue.zipWithIndex.foreach({case(s, i) =>
      val ptr = BrqPtr(brQueue(i).ptrFlag, i.U)
      when(io.memRedirect.valid && ptr.needBrFlush(io.memRedirect.bits.brTag)){
        s := s_idle
      }
      when(io.redirect.valid && ptr.needBrFlush(io.redirect.bits.brTag)){
        s := s_invalid
      }
    })
    when(io.redirect.valid || io.memRedirect.valid){
      when (!io.memRedirect.valid || isAfter(io.memRedirect.bits.brTag, io.redirect.bits.brTag)) {
        tailPtr := io.redirect.bits.brTag + 1.U
      }.otherwise {
        tailPtr := io.memRedirect.bits.brTag + 1.U
      }
    }

  }




  // Debug info
  val debug_roq_redirect = io.roqRedirect.valid
  val debug_brq_redirect = io.redirect.valid && !debug_roq_redirect
  val debug_normal_mode = !(debug_roq_redirect || debug_brq_redirect)

  for(i <- 0 until DecodeWidth){
    XSDebug(
      debug_normal_mode,
      p"enq v:${io.enqReqs(i).valid} rdy:${io.enqReqs(i).ready} pc:${Hexadecimal(io.enqReqs(i).bits.cf.pc)}" +
        p" brTag:${io.brTags(i)}\n"
    )
  }

  XSInfo(debug_roq_redirect, "roq redirect, flush brq\n")

  XSInfo(debug_brq_redirect, p"brq redirect, target:${Hexadecimal(io.redirect.bits.target)}\n")

  val fire = io.out.fire()
  val predRight = fire && !commitIsMisPred
  val predWrong = fire && commitIsMisPred
  // val isBType = commitEntry.exuOut.brUpdate.btbType===BTBtype.B
  val isBType = commitEntry.exuOut.brUpdate.pd.isBr
  // val isJType = commitEntry.exuOut.brUpdate.btbType===BTBtype.J
  val isJType = commitEntry.exuOut.brUpdate.pd.isJal
  // val isIType = commitEntry.exuOut.brUpdate.btbType===BTBtype.I
  val isIType = commitEntry.exuOut.brUpdate.pd.isJalr
  // val isRType = commitEntry.exuOut.brUpdate.btbType===BTBtype.R
  val isRType = commitEntry.exuOut.brUpdate.pd.isRet
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
}
