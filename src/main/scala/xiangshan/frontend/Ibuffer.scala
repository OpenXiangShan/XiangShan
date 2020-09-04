package xiangshan.frontend

import chisel3._
import chisel3.util._

import xiangshan._
import utils._

class Ibuffer extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = Flipped(DecoupledIO(new FetchPacket))
    val out = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  })

  class IBufEntry extends XSBundle {
    val inst = UInt(32.W)
    val pc = UInt(VAddrBits.W)
    val pnpc = UInt(VAddrBits.W)
    val brInfo = new BranchInfo
    val pd = new PreDecodeInfo
  }

  // Ignore
  for(out <- io.out) {
    out.bits.exceptionVec := DontCare
    out.bits.intrVec := DontCare
    out.bits.crossPageIPFFix := DontCare
  }

  // Ibuffer define
  val ibuf = Mem(IBufSize, new IBufEntry)
  val ibuf_valid = RegInit(VecInit(Seq.fill(IBufSize)(false.B)))
  val head_ptr = RegInit(0.U(log2Up(IBufSize).W))
  val tail_ptr = RegInit(0.U(log2Up(IBufSize).W))

  val enqValid = !io.flush && !ibuf_valid(tail_ptr + PredictWidth.U - 1.U)
  val deqValid = !io.flush && ibuf_valid(head_ptr)

  io.in.ready := enqValid

  // Enque
  when(io.in.fire) {
    var enq_idx = tail_ptr

    for(i <- 0 until PredictWidth) {
      ibuf_valid(enq_idx) := io.in.bits.mask(i)

      ibuf(enq_idx).inst := io.in.bits.instrs(i)
      ibuf(enq_idx).pc := io.in.bits.pc(i)
      ibuf(enq_idx).pnpc := io.in.bits.pnpc(i)
      ibuf(enq_idx).brInfo := io.in.bits.brInfo(i)
      ibuf(enq_idx).pd := io.in.bits.pd(i)

      enq_idx = enq_idx + io.in.bits.mask(i)
    }

    tail_ptr := enq_idx
  }

  // Deque
  when(deqValid) {
    var deq_idx = head_ptr
    for(i <- 0 until DecodeWidth) {
      io.out(i).valid := ibuf_valid(deq_idx)
      // Only when the entry is valid can it be set invalid
      when (ibuf_valid(deq_idx)) { ibuf_valid(deq_idx) := !io.out(i).fire }
      
      io.out(i).bits.instr := ibuf(deq_idx).inst
      io.out(i).bits.pc := ibuf(deq_idx).pc
      // io.out(i).bits.brUpdate := ibuf(deq_idx).brInfo
      io.out(i).bits.brUpdate := DontCare
      io.out(i).bits.brUpdate.pc := ibuf(deq_idx).pc
      io.out(i).bits.brUpdate.pnpc := ibuf(deq_idx).pnpc
      io.out(i).bits.brUpdate.pd := ibuf(deq_idx).pd
      io.out(i).bits.brUpdate.brInfo := ibuf(deq_idx).brInfo

      deq_idx = deq_idx + io.out(i).fire
    }
    head_ptr := deq_idx
  }.otherwise {
    io.out.foreach(_.valid := false.B)
    io.out.foreach(_.bits <> DontCare)
  }

  // Flush
  when(io.flush) {
    ibuf_valid.foreach(_ := false.B)
    head_ptr := 0.U
    tail_ptr := 0.U
    io.out.foreach(_.valid := false.B)
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
        XSDebug(p"${Hexadecimal(io.out(i).bits.instr)}  PC=${Hexadecimal(io.out(i).bits.pc)}  v=${io.out(i).valid}  r=${io.out(i).ready}\n")
    }
  }

  XSDebug(p"last_head_ptr=$head_ptr  last_tail_ptr=$tail_ptr\n")
  for(i <- 0 until IBufSize/8) {
    XSDebug("%x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b\n",
      ibuf(i*8+0).inst, ibuf_valid(i*8+0),
        ibuf(i*8+1).inst, ibuf_valid(i*8+1),
        ibuf(i*8+2).inst, ibuf_valid(i*8+2),
        ibuf(i*8+3).inst, ibuf_valid(i*8+3),
        ibuf(i*8+4).inst, ibuf_valid(i*8+4),
        ibuf(i*8+5).inst, ibuf_valid(i*8+5),
        ibuf(i*8+6).inst, ibuf_valid(i*8+6),
        ibuf(i*8+7).inst, ibuf_valid(i*8+7)
    )
  }

  XSPerf("utilization", PopCount(ibuf_valid))
}