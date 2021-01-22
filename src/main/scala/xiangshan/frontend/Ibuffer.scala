package xiangshan.frontend

import chisel3._
import chisel3.util._

import xiangshan._
import utils._
import xiangshan.backend.fu.HasExceptionNO


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

  class IBufEntry extends XSBundle {
    val inst = UInt(32.W)
    val pc = UInt(VAddrBits.W)
    val pnpc = UInt(VAddrBits.W)
    val brInfo = new BpuMeta
    val pd = new PreDecodeInfo
    val ipf = Bool()
    val acf = Bool()
    val crossPageIPFFix = Bool()
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
  val head_ptr = RegInit(IbufPtr(false.B, 0.U))
  val next_head_ptr = WireInit(head_ptr)
  val tail_vec = RegInit(VecInit((0 until PredictWidth).map(_.U.asTypeOf(new IbufPtr))))
  val tail_ptr = tail_vec(0)

  // val validEntries = distanceBetween(tail_ptr, head_ptr) // valid entries
  val validEntries = RegInit(0.U(log2Up(IBufSize + 1).W))// valid entries
  val allowEnq = RegInit(true.B)

  // val enqValid = (IBufSize.U - PredictWidth.U) >= validEntries
  val deqValid = validEntries > 0.U

  val numEnq = Mux(io.in.fire, PopCount(io.in.bits.mask), 0.U)
  val numDeq = Mux(deqValid, PopCount(io.out.map(_.fire)), 0.U)

  validEntries := validEntries + numEnq - numDeq
  allowEnq := (IBufSize.U - PredictWidth.U) >= (validEntries + numEnq)

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

  when(io.in.fire && !io.flush) {
    for(i <- 0 until PredictWidth) {
      val inWire = Wire(new IBufEntry)
      inWire := DontCare

      when(io.in.bits.mask(i)) {
        inWire.inst := io.in.bits.instrs(i)
        inWire.pc := io.in.bits.pc(i)
        inWire.pnpc := io.in.bits.pnpc(i)
        inWire.brInfo := io.in.bits.bpuMeta(i)
        inWire.pd := io.in.bits.pd(i)
        inWire.ipf := io.in.bits.ipf
        inWire.acf := io.in.bits.acf
        inWire.crossPageIPFFix := io.in.bits.crossPageIPFFix
        // ibuf(tail_vec(offset(i)).value) := inWire
      }
      ibuf.io.waddr(i) := tail_vec(offset(i)).value
      ibuf.io.wdata(i) := inWire
      ibuf.io.wen(i) := io.in.bits.mask(i)

    }

    tail_vec := VecInit(tail_vec.map(_ + PopCount(io.in.bits.mask)))
  }.otherwise {
    ibuf.io.wen.foreach(_ := false.B)
    ibuf.io.waddr := DontCare
    ibuf.io.wdata := DontCare
  }

  // Deque
  when(deqValid) {
    val validVec = UIntToMask(Mux(validEntries >= DecodeWidth.U, DecodeWidth.U, validEntries), DecodeWidth)

    io.out.zipWithIndex.foreach{case (e, i) => e.valid := validVec(i)}
    next_head_ptr := head_ptr + PopCount(io.out.map(_.fire))

    for(i <- 0 until DecodeWidth) {
      val outWire = ibuf.io.rdata(i)

      io.out(i).bits.instr := outWire.inst
      io.out(i).bits.pc := outWire.pc
      // io.out(i).bits.exceptionVec := Mux(outWire.ipf, UIntToOH(instrPageFault.U), 0.U)
      io.out(i).bits.exceptionVec := 0.U.asTypeOf(Vec(16, Bool()))
      io.out(i).bits.exceptionVec(instrPageFault) := outWire.ipf
      io.out(i).bits.exceptionVec(instrAccessFault) := outWire.acf
      // io.out(i).bits.brUpdate := outWire.brInfo
      io.out(i).bits.brUpdate := DontCare
      io.out(i).bits.brUpdate.pc := outWire.pc
      io.out(i).bits.brUpdate.pnpc := outWire.pnpc
      io.out(i).bits.brUpdate.pd := outWire.pd
      io.out(i).bits.brUpdate.bpuMeta := outWire.brInfo
      io.out(i).bits.crossPageIPFFix := outWire.crossPageIPFFix
      
      val head_wire = next_head_ptr.value + i.U
      ibuf.io.raddr(i) := head_wire
    }
    head_ptr := next_head_ptr
  }.otherwise {
    ibuf.io.raddr := DontCare
    io.out.foreach(_.valid := false.B)
    io.out.foreach(_.bits <> DontCare)
  }

  // Flush
  when(io.flush) {
    validEntries := 0.U
    allowEnq := true.B
    head_ptr.value := 0.U
    head_ptr.flag := false.B
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

  when(deqValid) {
    XSDebug("Deque:\n")
    for(i <- 0 until DecodeWidth){
        XSDebug(p"${Hexadecimal(io.out(i).bits.instr)} PC=${Hexadecimal(io.out(i).bits.pc)} v=${io.out(i).valid} r=${io.out(i).ready} " +
          p"excpVec=${Binary(io.out(i).bits.exceptionVec.asUInt)} crossPageIPF=${io.out(i).bits.crossPageIPFFix}\n")
    }
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

  XSPerf("utilization", validEntries)
}
