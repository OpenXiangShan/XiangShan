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
    val fetchOffset = UInt((log2Up(FetchWidth * 4)).W)
    val hist = UInt(HistoryLength.W)
    val btbPredCtr = UInt(2.W)
    val btbHit = Bool()
    val tageMeta = new TageMeta
    val rasSp = UInt(log2Up(RasSize).W)
    val rasTopCtr = UInt(8.W)
  }

  // Ignore
  for(out <- io.out) {
    out.bits.exceptionVec := DontCare
    out.bits.intrVec := DontCare
    out.bits.isBr := DontCare
    out.bits.crossPageIPFFix := DontCare
  }

  // Ibuffer define
  val ibuf = Mem(IBufSize, new IBufEntry)
  val ibuf_valid = RegInit(VecInit(Seq.fill(IBufSize)(false.B)))
  val head_ptr = RegInit(0.U(log2Up(IBufSize).W))
  val tail_ptr = RegInit(0.U(log2Up(IBufSize).W))

  val enqValid = !io.flush && !ibuf_valid(tail_ptr + FetchWidth.U - 1.U)
  val deqValid = !io.flush && ibuf_valid(head_ptr)

  io.in.ready := enqValid

  // Enque
  when(io.in.fire) {
    var enq_idx = tail_ptr
    for(i <- 0 until FetchWidth) {
      ibuf_valid(enq_idx) := io.in.bits.mask(i<<1)

      ibuf(enq_idx).inst := io.in.bits.instrs(i)
      ibuf(enq_idx).pc := io.in.bits.pc + ((enq_idx - tail_ptr)<<2).asUInt
      ibuf(enq_idx).pnpc := io.in.bits.pnpc(i<<1)
      ibuf(enq_idx).fetchOffset := ((enq_idx - tail_ptr)<<2).asUInt
      ibuf(enq_idx).hist := io.in.bits.hist(i<<1)
      ibuf(enq_idx).btbPredCtr := io.in.bits.predCtr(i<<1)
      ibuf(enq_idx).btbHit := io.in.bits.btbHit(i<<1)
      ibuf(enq_idx).tageMeta := io.in.bits.tageMeta(i<<1)
      ibuf(enq_idx).rasSp := io.in.bits.rasSp
      ibuf(enq_idx).rasTopCtr := io.in.bits.rasTopCtr

      enq_idx = enq_idx + io.in.bits.mask(i<<1)
    }

    tail_ptr := enq_idx
  }

  // Deque
  when(deqValid) {
    var deq_idx = head_ptr
    for(i <- 0 until DecodeWidth) {
      io.out(i).valid := ibuf_valid(deq_idx)
      ibuf_valid(deq_idx) := !io.out(i).fire

      io.out(i).bits.instr := ibuf(deq_idx).inst
      io.out(i).bits.pc := ibuf(deq_idx).pc
      io.out(i).bits.fetchOffset := ibuf(deq_idx).fetchOffset
      io.out(i).bits.pnpc := ibuf(deq_idx).pnpc
      io.out(i).bits.hist := ibuf(deq_idx).hist
      io.out(i).bits.btbPredCtr := ibuf(deq_idx).btbPredCtr
      io.out(i).bits.btbHit := ibuf(deq_idx).btbHit
      io.out(i).bits.tageMeta := ibuf(deq_idx).tageMeta
      io.out(i).bits.rasSp := ibuf(deq_idx).rasSp
      io.out(i).bits.rasTopCtr := ibuf(deq_idx).rasTopCtr
      io.out(i).bits.isRVC := false.B // FIXME: This is not ibuffer's work now

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
    XSDebug(p"PC=${Hexadecimal(io.in.bits.pc)} MASK=${Binary(io.in.bits.mask)}\n")
    for(i <- 0 until FetchWidth){
        XSDebug(p"${Hexadecimal(io.in.bits.instrs(i))}  v=${io.in.valid}  r=${io.in.ready}\n")
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
}