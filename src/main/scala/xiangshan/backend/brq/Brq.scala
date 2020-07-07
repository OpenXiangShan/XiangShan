package xiangshan.backend.brq

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils.XSInfo


class BrqPtr extends XSBundle {

  val flag = Bool()
  val value = UInt(BrTagWidth.W)

  final def + (inc: Bool): BrqPtr = {
    Mux(inc && (value === (BrqSize-1).U),
      BrqPtr(!flag, 0.U),
      BrqPtr(flag, value + inc)
    )
  }

  final def === (that: BrqPtr): Bool = {
    (this.value===that.value) && (this.flag===that.flag)
  }

  // this.age < that.age
  final def < (that: BrqPtr): Bool = {
    Mux(this.flag === that.flag,
      this.value > that.value,
      this.value < that.value
    )
  }

  def needBrFlush(redirectTag: BrqPtr): Bool = this < redirectTag

  def needFlush(redirect: Valid[Redirect]): Bool = {
    redirect.valid && (redirect.bits.isException || needBrFlush(redirect.bits.brTag))
  }

  override def toPrintable: Printable = p"f:$flag v:$value"

}

object BrqPtr {
  def apply(f: Bool, v: UInt): BrqPtr = {
    val ptr = Wire(new BrqPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}


class Brq extends XSModule {
  val io = IO(new Bundle() {
    // interrupt/exception happen, flush Brq
    val roqRedirect = Input(Valid(new Redirect))
    // receive branch/jump calculated target
    val exuRedirect = Vec(exuConfig.AluCnt + exuConfig.BruCnt, Flipped(ValidIO(new ExuOutput)))
    // from decode, branch insts enq
    val enqReqs = Vec(DecodeWidth, Flipped(DecoupledIO(new CfCtrl)))
    // to decode
    val brTags = Output(Vec(DecodeWidth, new BrqPtr))
    // to roq
    val out = ValidIO(new ExuOutput)
    // misprediction, flush pipeline
    val redirect = Output(Valid(new Redirect))
  })

  class BrqEntry extends Bundle {
    // val pc = UInt(VAddrBits.W)
    val npc = UInt(VAddrBits.W)
    val exuOut = new ExuOutput
  }

  val brQueue = Reg(Vec(BrqSize, new BrqEntry))
  val wbFlags = RegInit(VecInit(Seq.fill(BrqSize)(false.B)))

  val headPtr, tailPtr = RegInit(BrqPtr(false.B, 0.U))

  def isEmpty(ptr1: BrqPtr, ptr2: BrqPtr): Bool = ptr1 === ptr2
  def isFull(ptr1: BrqPtr, ptr2: BrqPtr): Bool = (ptr1.flag=/=ptr2.flag) && (ptr1.value===ptr2.value)


  // dequeue
  val headIdx = headPtr.value
  val deqValid = wbFlags(headIdx)
  val deqEntry = brQueue(headIdx)

  val headPtrNext = WireInit(headPtr + deqValid)
  when(deqValid){
    wbFlags(headIdx) := false.B
  }
  headPtr := headPtrNext
  io.redirect.valid := deqValid && (deqEntry.npc =/= deqEntry.exuOut.redirect.target)
  io.redirect.bits := deqEntry.exuOut.redirect
  io.out.valid := deqValid
  io.out.bits := deqEntry.exuOut

  // branch insts enq
  var full = WireInit(isFull(headPtrNext, tailPtr))
  var tailPtrNext = WireInit(tailPtr)
  for((enq, brTag) <- io.enqReqs.zip(io.brTags)){
    enq.ready := !full
    brTag := tailPtrNext
    // TODO: check rvc and use predict npc
    when(enq.fire()){ brQueue(tailPtrNext.value).npc := enq.bits.cf.pnpc }
    tailPtrNext = tailPtrNext + enq.fire()
    full = isFull(tailPtrNext, headPtrNext)
  }
  tailPtr := tailPtrNext

  // exu write back
  for(exuWb <- io.exuRedirect){
    when(exuWb.valid){
      wbFlags(exuWb.bits.uop.brTag.value) := true.B
      brQueue(exuWb.bits.uop.brTag.value).exuOut := exuWb.bits
    }
  }

  // when redirect, reset all regs
  when(io.roqRedirect.valid || io.redirect.valid){
    wbFlags.foreach(_ := false.B)
    val resetPtr  = io.redirect.bits.brTag + true.B
    headPtr := resetPtr
    tailPtr := resetPtr
  }




  // Debug info
  val debug_roq_redirect = io.roqRedirect.valid
  val debug_brq_redirect = io.redirect.valid && !debug_roq_redirect
  val debug_normal_mode = !(debug_roq_redirect || debug_brq_redirect)

  for(i <- 0 until DecodeWidth){
    XSInfo(
      debug_normal_mode,
      p"enq v:${io.enqReqs(i).valid} rdy:${io.enqReqs(i).ready} pc:${Hexadecimal(io.enqReqs(i).bits.cf.pc)}" +
        p" brTag:${io.brTags(i)}\n"
    )
  }

  XSInfo(debug_roq_redirect, "roq redirect, flush brq\n")
  XSInfo(debug_brq_redirect, p"brq redirect, target:${Hexadecimal(io.redirect.bits.target)} flptr:${io.redirect.bits.freelistAllocPtr}\n")
}
