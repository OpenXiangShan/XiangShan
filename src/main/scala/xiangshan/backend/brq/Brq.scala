package xiangshan.backend.brq

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils.XSInfo



class Brq extends XSModule {
  val io = IO(new Bundle() {
    // interrupt/exception happen, flush Brq
    val roqRedirect = Input(Valid(new Redirect))
    // receive branch/jump calculated target
    val exuRedirect = Vec(exuConfig.AluCnt + exuConfig.BruCnt, Flipped(ValidIO(new ExuOutput)))
    // from decode, branch insts enq
    val enqReqs = Vec(DecodeWidth, Flipped(DecoupledIO(new CfCtrl)))
    // to decode
    val brTags = Output(Vec(DecodeWidth, UInt(BrTagWidth.W)))
    val brMasks = Output(Vec(DecodeWidth, UInt(BrqSize.W)))

    // misprediction, flush pipeline
    val redirect = Output(Valid(new Redirect))
  })

  class BrqEntry extends Bundle {
    // val pc = UInt(VAddrBits.W)
    val npc = UInt(VAddrBits.W)
    val redirect = new Redirect
    // val _type = UInt(2.W)
  }

  val brQueue = Reg(Vec(BrqSize, new BrqEntry))
  val brMask = RegInit(0.U(BrqSize.W))
  val wbFlags = RegInit(VecInit(Seq.fill(BrqSize)(false.B)))

  val headPtr, tailPtr = RegInit(0.U((BrTagWidth+1).W))

  def ptrToIndex(ptr: UInt): UInt = ptr.tail(1)
  def isEmpty(ptr1: UInt, ptr2: UInt): Bool = ptr1 === ptr2
  def isFull(ptr1: UInt, ptr2: UInt): Bool = (ptr1.head(1)=/=ptr2.head(1)) && (ptr1.tail(1)===ptr2.tail(1))


  // dequeue
  val headIdx = ptrToIndex(headPtr)
  val deqValid = wbFlags(headIdx)
  val deqEntry = brQueue(headIdx)

  val deqMask = (~Mux(deqValid, UIntToOH(headIdx), 0.U)).asUInt()
  val headPtrNext = WireInit(headPtr + deqValid)
  when(deqValid){
    wbFlags(headIdx) := false.B
  }
  headPtr := headPtrNext
  io.redirect.valid := deqValid && (deqEntry.npc =/= deqEntry.redirect.target)
  io.redirect.bits := deqEntry.redirect

  // branch insts enq
  var full = WireInit(isFull(headPtrNext, tailPtr))
  var tailPtrNext = WireInit(tailPtr)
  var brMaskNext = WireInit(brMask & deqMask)
  for(((enq, brMask), brTag) <- io.enqReqs.zip(io.brMasks).zip(io.brTags)){
    val tailIdx = ptrToIndex(tailPtrNext)
    enq.ready := !full
    brTag := tailIdx
    // TODO: check rvc and use predict npc
    // when(enq.fire()){ brQueue(tailIdx).npc := enq.bits.cf.pc + 4.U }
    when (enq.fire()) {
      brQueue(tailIdx).npc := enq.bits.cf.pnpc
    }
    brMaskNext = brMaskNext | Mux(enq.fire(), UIntToOH(tailIdx), 0.U)
    brMask := brMaskNext
    tailPtrNext = tailPtrNext + enq.fire()
    full = isFull(tailPtrNext, headPtrNext)
  }
  brMask := brMaskNext
  tailPtr := tailPtrNext

  // exu write back
  for(exuWb <- io.exuRedirect){
    when(exuWb.valid && exuWb.bits.redirect.valid){
      wbFlags(exuWb.bits.uop.brTag) := true.B
      brQueue(exuWb.bits.uop.brTag).redirect := exuWb.bits.redirect.bits
    }
  }

  // when redirect, reset all regs
  when(io.roqRedirect.valid || io.redirect.valid){
    brMask := 0.U
    wbFlags.foreach(_ := false.B)
    headPtr := 0.U
    tailPtr := 0.U
  }




  // Debug info
  val debug_roq_redirect = io.roqRedirect.valid
  val debug_brq_redirect = io.redirect.valid && !debug_roq_redirect
  val debug_normal_mode = !(debug_roq_redirect || debug_brq_redirect)

  for(i <- 0 until DecodeWidth){
    XSInfo(
      debug_normal_mode,
      p"enq v:${io.enqReqs(i).valid} rdy:${io.enqReqs(i).ready} pc:${Hexadecimal(io.enqReqs(i).bits.cf.pc)}" +
        p" brMask:${Binary(io.brMasks(i))} brTag:${io.brTags(i)}\n"
    )
  }

  XSInfo(debug_roq_redirect, "roq redirect, flush brq\n")
  XSInfo(debug_brq_redirect, p"brq redirect, target:${Hexadecimal(io.redirect.bits.target)}\n")
}
