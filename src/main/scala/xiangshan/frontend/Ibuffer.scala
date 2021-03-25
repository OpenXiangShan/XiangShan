package xiangshan.frontend

import chisel3._
import chisel3.util._

import xiangshan._
import utils._
import xiangshan.backend.fu.HasExceptionNO
import xiangshan.backend.ftq.FtqPtr
import xiangshan.backend.decode.WaitTableParameters

class IbufPtr extends CircularQueuePtr(IbufPtr.IBufSize) { }

object IbufPtr extends HasXSParameter {
  def apply(f: Bool, v: UInt): IbufPtr = {
    val ptr = Wire(new IbufPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class IBufferIO extends XSBundle {
  val flush = Input(Bool())
  val in = Flipped(DecoupledIO(new FetchPacket))
  val out = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
}

class Ibuffer extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new IBufferIO)

  class IBufEntry extends XSBundle with WaitTableParameters {
    val inst = UInt(32.W)
    val pc = UInt(VAddrBits.W)
    val foldpc = UInt(WaitTableAddrWidth.W)
    val pd = new PreDecodeInfo
    val ipf = Bool()
    val acf = Bool()
    val crossPageIPFFix = Bool()
    val pred_taken = Bool()
    val ftqPtr = new FtqPtr
    val ftqOffset = UInt(log2Ceil(PredictWidth).W)
  }

  // Ignore
  // io.loopBufPar <> DontCare
  // io.loopBufPar.LBredirect.valid := false.B
  // io.loopBufPar.inLoop := false.B


  for(out <- io.out) {
    // out.bits.exceptionVec := DontCare
    out.bits.intrVec := DontCare
    // out.bits.crossPageIPFFix := DontCare
  }

  // Ibuffer define
  // val ibuf = Reg(Vec(IBufSize, new IBufEntry))
  val ibuf = Module(new SyncDataModuleTemplate(new IBufEntry, IBufSize, DecodeWidth, PredictWidth))
  ibuf.io.wdata.map(w => dontTouch(w.ftqOffset))
  val head_vec = RegInit(VecInit((0 until DecodeWidth).map(_.U.asTypeOf(new IbufPtr))))
  val tail_vec = RegInit(VecInit((0 until PredictWidth).map(_.U.asTypeOf(new IbufPtr))))
  val head_ptr = head_vec(0)
  val tail_ptr = tail_vec(0)

  val validEntries = distanceBetween(tail_ptr, head_ptr)
  val allowEnq = RegInit(true.B)

  val numEnq = Mux(io.in.fire, PopCount(io.in.bits.mask), 0.U)
  val numTryDeq = Mux(validEntries >= DecodeWidth.U, DecodeWidth.U, validEntries)
  val numDeq = PopCount(io.out.map(_.fire))

  val numAfterEnq = validEntries +& numEnq
  val nextValidEntries = Mux(io.out(0).ready, numAfterEnq - numTryDeq, numAfterEnq)
  allowEnq := (IBufSize - PredictWidth).U >= nextValidEntries

  // Enque
  io.in.ready := allowEnq
 
  val offset = Wire(Vec(PredictWidth, UInt(log2Up(PredictWidth).W)))
  for(i <- 0 until PredictWidth) {
    if (i == 0) {
      offset(i) := 0.U
    } else {
      offset(i) := PopCount(io.in.bits.pdmask(i-1, 0))
    }
  }

  for (i <- 0 until PredictWidth) {
    val inWire = Wire(new IBufEntry)
    inWire.inst := io.in.bits.instrs(i)
    inWire.pc := io.in.bits.pc(i)
    inWire.pd := io.in.bits.pd(i)
    inWire.ipf := io.in.bits.ipf
    inWire.acf := io.in.bits.acf
    inWire.crossPageIPFFix := io.in.bits.crossPageIPFFix
    inWire.foldpc := io.in.bits.foldpc(i)
    inWire.pred_taken := io.in.bits.pred_taken(i)
    inWire.ftqPtr := io.in.bits.ftqPtr
    inWire.ftqOffset := i.U

    ibuf.io.waddr(i) := tail_vec(offset(i)).value
    ibuf.io.wdata(i) := inWire
    ibuf.io.wen(i)   := io.in.bits.mask(i) && io.in.fire && !io.flush
  }

  when (io.in.fire && !io.flush) {
    tail_vec := VecInit(tail_vec.map(_ + PopCount(io.in.bits.mask)))
  }

  // Dequeue
  val validVec = Mux(validEntries >= DecodeWidth.U, ((1 << DecodeWidth) - 1).U, UIntToMask(validEntries, DecodeWidth))
  for (i <- 0 until DecodeWidth) {
    io.out(i).valid := validVec(i)

    val outWire = ibuf.io.rdata(i)

    io.out(i).bits.instr := outWire.inst
    io.out(i).bits.pc := outWire.pc
    // io.out(i).bits.exceptionVec := Mux(outWire.ipf, UIntToOH(instrPageFault.U), 0.U)
    io.out(i).bits.exceptionVec := 0.U.asTypeOf(Vec(16, Bool()))
    io.out(i).bits.exceptionVec(instrPageFault) := outWire.ipf
    io.out(i).bits.exceptionVec(instrAccessFault) := outWire.acf
    // io.out(i).bits.brUpdate := outWire.brInfo
    io.out(i).bits.pd := outWire.pd
    io.out(i).bits.pred_taken := outWire.pred_taken
    io.out(i).bits.ftqPtr := outWire.ftqPtr
    io.out(i).bits.ftqOffset := outWire.ftqOffset

    io.out(i).bits.crossPageIPFFix := outWire.crossPageIPFFix
    io.out(i).bits.foldpc := outWire.foldpc
    io.out(i).bits.loadWaitBit := DontCare
  }
  val next_head_vec = VecInit(head_vec.map(_ + numDeq))
  ibuf.io.raddr := VecInit(next_head_vec.map(_.value))
  head_vec := next_head_vec

  // Flush
  when (io.flush) {
    allowEnq := true.B
    head_vec := VecInit((0 until DecodeWidth).map(_.U.asTypeOf(new IbufPtr)))
    tail_vec := VecInit((0 until PredictWidth).map(_.U.asTypeOf(new IbufPtr)))
  }

  // Debug info
  XSDebug(io.flush, "IBuffer Flushed\n")

  when(io.in.fire) {
    XSDebug("Enque:\n")
    XSDebug(p"MASK=${Binary(io.in.bits.mask)}\n")
    for(i <- 0 until PredictWidth){
        XSDebug(p"PC=${Hexadecimal(io.in.bits.pc(i))} ${Hexadecimal(io.in.bits.instrs(i))}\n")
    }
  }

  for (i <- 0 until DecodeWidth) {
    XSDebug(io.out(i).fire(), p"deq: ${Hexadecimal(io.out(i).bits.instr)} PC=${Hexadecimal(io.out(i).bits.pc)} v=${io.out(i).valid} r=${io.out(i).ready} " +
      p"excpVec=${Binary(io.out(i).bits.exceptionVec.asUInt)} crossPageIPF=${io.out(i).bits.crossPageIPFFix}\n")
  }

  XSDebug(p"ValidEntries: ${validEntries}\n")
  XSDebug(p"EnqNum: ${numEnq}\n")
  XSDebug(p"DeqNum: ${numDeq}\n")

  // XSDebug(p"last_head_ptr=$head_ptr  last_tail_ptr=$tail_ptr\n")
  // for(i <- 0 until IBufSize/8) {
  //   XSDebug("%x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b\n",
  //     ibuf(i*8+0).inst, ibuf_valid(i*8+0),
  //       ibuf(i*8+1).inst, ibuf_valid(i*8+1),
  //       ibuf(i*8+2).inst, ibuf_valid(i*8+2),
  //       ibuf(i*8+3).inst, ibuf_valid(i*8+3),
  //       ibuf(i*8+4).inst, ibuf_valid(i*8+4),
  //       ibuf(i*8+5).inst, ibuf_valid(i*8+5),
  //       ibuf(i*8+6).inst, ibuf_valid(i*8+6),
  //       ibuf(i*8+7).inst, ibuf_valid(i*8+7)
  //   )
  // }

  // XSDebug(p"validEntries=$validEntries, last_head_ptr=$head_ptr  last_tail_ptr=$tail_ptr\n")
  // for(i <- 0 until IBufSize/8) {
  //   XSDebug("%x | %x | %x | %x | %x | %x | %x | %x\n",
  //     ibuf(i*8+0).inst,
  //       ibuf(i*8+1).inst,
  //       ibuf(i*8+2).inst,
  //       ibuf(i*8+3).inst,
  //       ibuf(i*8+4).inst,
  //       ibuf(i*8+5).inst,
  //       ibuf(i*8+6).inst,
  //       ibuf(i*8+7).inst
  //   )
  // }

  val afterInit = RegInit(false.B)
  val headBubble = RegInit(false.B)
  when (io.in.fire) { afterInit := true.B }
  when (io.flush) {
    headBubble := true.B
  } .elsewhen(validEntries =/= 0.U) {
    headBubble := false.B
  }
  val instrHungry = afterInit && (validEntries === 0.U) && !headBubble

  QueuePerf(IBufSize, validEntries, !allowEnq)
  XSPerf("flush", io.flush)
  XSPerf("hungry", instrHungry)
}
