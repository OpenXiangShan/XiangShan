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

  // ignore
  for(i <- 0 until DecodeWidth) {
    io.out(i).bits.exceptionVec := DontCare
    io.out(i).bits.intrVec := DontCare
    io.out(i).bits.isBr := DontCare
    io.out(i).bits.crossPageIPFFix := DontCare
  }

  //mask initial
  //  val mask = Wire(Vec(FetchWidth*2, false.B))
  //  (0 until 16).map(i => mask(i.U) := (io.in.bits.pc(4,1) <= i.U))

  // ibuf define
  val ibuf = Reg(Vec(IBufSize*2, UInt(16.W)))
  val ibuf_pc = Reg(Vec(IBufSize*2, UInt(VAddrBits.W)))
  val ibuf_pnpc = Reg(Vec(IBufSize*2, UInt(VAddrBits.W)))
  val ibuf_fetchOffset = Reg(Vec(IBufSize*2, UInt(log2Up(FetchWidth*4).W)))
  val ibuf_hist = Reg(Vec(IBufSize*2, UInt(HistoryLength.W)))
  // val ibuf_btbVictimWay = Reg(Vec(IBufSize*2, UInt(log2Up(BtbWays).W)))
  val ibuf_btbPredCtr = Reg(Vec(IBufSize*2, UInt(2.W)))
  val ibuf_btbHit = Reg(Vec(IBufSize*2, Bool()))
  val ibuf_tageMeta = Reg(Vec(IBufSize*2, (new TageMeta)))
  val ibuf_rasSp = Reg(Vec(IBufSize*2, UInt(log2Up(RasSize).W)))
  val ibuf_rasTopCtr = Reg(Vec(IBufSize*2, UInt(8.W)))
  val ibuf_valid = RegInit(VecInit(Seq.fill(IBufSize*2)(false.B)))
  val head_ptr = RegInit(0.U(log2Up(IBufSize*2).W))
  val tail_ptr = RegInit(0.U(log2Up(IBufSize*2).W))

  // true: Last operation is enqueue
  // false: Last operation is deq_ueue
  val last_enq = RegInit(false.B)
  val full = head_ptr === tail_ptr && last_enq
  val empty = head_ptr === tail_ptr && !last_enq
  val enqValid = !io.flush && io.in.valid && !full && !ibuf_valid(tail_ptr + (FetchWidth*2).U)
  val deqValid = !io.flush && !empty //&& io.out.map(_.ready).reduce(_||_)

  io.in.ready := !full && !ibuf_valid(tail_ptr + (FetchWidth*2).U)

  // enque
  when(enqValid) {
    var enq_idx = 0.U(log2Up(FetchWidth*2+1).W)
    for(i <- 0 until FetchWidth*2) {
      when(io.in.bits.mask(i)) {
        ibuf(tail_ptr + enq_idx) := Mux(i.U(0), io.in.bits.instrs(i>>1)(31,16), io.in.bits.instrs(i>>1)(15,0))
        ibuf_pc(tail_ptr + enq_idx) := io.in.bits.pc + (enq_idx<<1).asUInt
        ibuf_pnpc(tail_ptr + enq_idx) := io.in.bits.pnpc(i)
        ibuf_fetchOffset(tail_ptr + enq_idx) := (enq_idx << 1).asUInt
        ibuf_hist(tail_ptr + enq_idx) := io.in.bits.hist(i)
        // ibuf_btbVictimWay(tail_ptr + enq_idx) := io.in.bits.btbVictimWay
        ibuf_btbPredCtr(tail_ptr + enq_idx) := io.in.bits.predCtr(i)
        ibuf_btbHit(tail_ptr + enq_idx) := io.in.bits.btbHit(i)
        ibuf_tageMeta(tail_ptr + enq_idx) := io.in.bits.tageMeta(i)
        ibuf_rasSp(tail_ptr + enq_idx) := io.in.bits.rasSp
        ibuf_rasTopCtr(tail_ptr + enq_idx) := io.in.bits.rasTopCtr
        ibuf_valid(tail_ptr + enq_idx) := true.B
      }

      XSDebug(!(i.U)(0), "Enq: i:%d Idx:%d mask:%b instr:%x pc:%x pnpc:%x\n",
        (i/2).U, tail_ptr+enq_idx, io.in.bits.mask(i), io.in.bits.instrs(i/2), io.in.bits.pc + (2*i).U, io.in.bits.pnpc(i/2))
      enq_idx = enq_idx + io.in.bits.mask(i)
    }

    tail_ptr := tail_ptr + enq_idx
    last_enq := true.B
  }

  // deque
  when(deqValid) {
    var deq_idx = 0.U(log2Up(DecodeWidth*2+1).W)
    for(i <- 0 until DecodeWidth) {
      io.out(i).valid := ibuf_valid(head_ptr + deq_idx) // FIXME: need fix me when support RVC
      when(ibuf_valid(head_ptr + deq_idx)) {
        when(ibuf(head_ptr + deq_idx)(1,0) =/= "b11".U) {
          // is RVC
          io.out(i).bits.instr := Cat(0.U(16.W), ibuf(head_ptr + deq_idx))
          io.out(i).bits.pc := ibuf_pc(head_ptr + deq_idx)
          io.out(i).bits.pnpc := ibuf_pnpc(head_ptr + deq_idx)
          io.out(i).bits.fetchOffset := ibuf_fetchOffset(head_ptr + deq_idx)
          io.out(i).bits.hist := ibuf_hist(head_ptr + deq_idx)
          // io.out(i).bits.btbVictimWay := ibuf_btbVictimWay(head_ptr + deq_idx)
          io.out(i).bits.btbPredCtr := ibuf_btbPredCtr(head_ptr + deq_idx)
          io.out(i).bits.btbHit := ibuf_btbHit(head_ptr + deq_idx)
          io.out(i).bits.tageMeta := ibuf_tageMeta(head_ptr + deq_idx)
          io.out(i).bits.rasSp := ibuf_rasSp(head_ptr + deq_idx)
          io.out(i).bits.rasTopCtr := ibuf_rasTopCtr(head_ptr + deq_idx)
          io.out(i).bits.isRVC := true.B
          ibuf_valid(head_ptr + deq_idx) := !io.out(i).fire
        }.elsewhen(ibuf_valid(head_ptr + deq_idx + 1.U)) {
          // isn't RVC
          io.out(i).bits.instr := Cat(ibuf(head_ptr + deq_idx+1.U), ibuf(head_ptr + deq_idx))
          io.out(i).bits.pc := ibuf_pc(head_ptr + deq_idx)
          io.out(i).bits.pnpc := ibuf_pnpc(head_ptr + deq_idx)
          io.out(i).bits.fetchOffset := ibuf_fetchOffset(head_ptr + deq_idx)
          io.out(i).bits.hist := ibuf_hist(head_ptr + deq_idx)
          // io.out(i).bits.btbVictimWay := ibuf_btbVictimWay(head_ptr + deq_idx)
          io.out(i).bits.btbPredCtr := ibuf_btbPredCtr(head_ptr + deq_idx)
          io.out(i).bits.btbHit := ibuf_btbHit(head_ptr + deq_idx)
          io.out(i).bits.tageMeta := ibuf_tageMeta(head_ptr + deq_idx)
          io.out(i).bits.rasSp := ibuf_rasSp(head_ptr + deq_idx)
          io.out(i).bits.rasTopCtr := ibuf_rasTopCtr(head_ptr + deq_idx)
          io.out(i).bits.isRVC := false.B
          ibuf_valid(head_ptr + deq_idx) := !io.out(i).fire
          ibuf_valid(head_ptr + deq_idx+1.U) := !io.out(i).fire
        }.otherwise {
          // half inst keep in buffer
          io.out(i).bits.instr := 0.U(32.W)
          io.out(i).bits.pc := 0.U(VAddrBits.W)
          io.out(i).bits.pnpc := 0.U(VAddrBits.W)
          io.out(i).bits.fetchOffset := 0.U(log2Up(FetchWidth*4).W)
          io.out(i).bits.hist := 0.U(HistoryLength.W)
          // io.out(i).bits.btbVictimWay := 0.U(log2Up(BtbWays).W)
          io.out(i).bits.btbPredCtr := 0.U(2.W)
          io.out(i).bits.btbHit := false.B
          io.out(i).bits.tageMeta := 0.U.asTypeOf(new TageMeta)
          io.out(i).bits.rasSp := 0.U(log2Up(RasSize))
          io.out(i).bits.rasTopCtr := 0.U(8.W)
          io.out(i).bits.isRVC := false.B
          io.out(i).valid := false.B
        }
      }.otherwise {
        io.out(i).bits.instr := Cat(ibuf(head_ptr + (i<<1).U + 1.U), ibuf(head_ptr + (i<<1).U))
        io.out(i).bits.pc := ibuf_pc(head_ptr + (i<<1).U)
        io.out(i).bits.pnpc := ibuf_pnpc(head_ptr + (i<<1).U)
        io.out(i).bits.fetchOffset := ibuf_fetchOffset(head_ptr + (i<<1).U)
        io.out(i).bits.hist := ibuf_hist(head_ptr + (i<<1).U)
        // io.out(i).bits.btbVictimWay := ibuf_btbVictimWay(head_ptr + (i<<1).U)
        io.out(i).bits.btbPredCtr := ibuf_btbPredCtr(head_ptr + (i<<1).U)
        io.out(i).bits.btbHit := ibuf_btbHit(head_ptr + (i<<1).U)
        io.out(i).bits.tageMeta := ibuf_tageMeta(head_ptr + (i<<1).U)
        io.out(i).bits.rasSp := ibuf_rasSp(head_ptr + (i<<1).U)
        io.out(i).bits.rasTopCtr := ibuf_rasTopCtr(head_ptr + (i<<1).U)
        io.out(i).bits.isRVC := false.B
      }

      // When can't deque, deq_idx+0
      // when RVC deque, deq_idx+1
      // when not RVC deque, deq_idx+2
      // when only have half inst, keep it in buffer
      deq_idx = deq_idx + PriorityMux(Seq(
        !(io.out(i).ready && ibuf_valid(head_ptr + deq_idx)) -> 0.U,
        (ibuf(head_ptr + deq_idx)(1,0) =/= "b11".U) -> 1.U,
        ibuf_valid(head_ptr + deq_idx + 1.U) -> 2.U
      ))

      XSDebug(deqValid, p"Deq: i:${i.U} valid:${ibuf_valid(head_ptr + deq_idx)} idx=${Decimal(head_ptr + deq_idx)} instr:${Hexadecimal(io.out(i).bits.instr)} PC=${Hexadecimal(io.out(i).bits.pc)}  ${Decimal(head_ptr + deq_idx + 1.U)} v=${io.out(i).valid}  r=${io.out(i).ready}\n")
    }
    head_ptr := head_ptr + deq_idx

    last_enq := false.B
  }.otherwise {
    for(i <- 0 until DecodeWidth) {
      io.out(i).bits.instr := 0.U
      io.out(i).bits.pc := 0.U
      io.out(i).bits.pnpc := 0.U
      io.out(i).bits.fetchOffset := 0.U
      io.out(i).bits.hist := 0.U(HistoryLength.W)
      // io.out(i).bits.btbVictimWay := 0.U(log2Up(BtbWays).W)
      io.out(i).bits.btbPredCtr := 0.U(2.W)
      io.out(i).bits.btbHit := false.B
      io.out(i).bits.tageMeta := 0.U.asTypeOf(new TageMeta)
      io.out(i).bits.rasSp := 0.U(log2Up(RasSize))
      io.out(i).bits.rasTopCtr := 0.U(8.W)
      io.out(i).bits.isRVC := false.B
      io.out(i).valid := false.B
    }
  }

  // flush
  when(io.flush) {
    for(i <- 0 until IBufSize*2) {
      ibuf_valid(i) := false.B
    }
    head_ptr := 0.U
    tail_ptr := 0.U

    for(i <- 0 until DecodeWidth) {
      io.out(i).valid := false.B
    }
  }

  //Debug Info
  // XSDebug(enqValid, "Enque:\n")
  // for(i <- 0 until FetchWidth) {
  //   XSDebug(enqValid, p"${Binary(io.in.bits.instrs(i))}\n")
  // }

  XSInfo(io.flush, "Flush signal received, clear buffer\n")
  // XSDebug(deqValid, "Deque:\n")
  // for(i <- 0 until DecodeWidth) {
  //   XSDebug(deqValid, p"${Binary(io.out(i).bits.instr)}  PC=${Hexadecimal(io.out(i).bits.pc)}  v=${io.out(i).valid}  r=${io.out(i).ready}\n")
  // }
  XSDebug(enqValid, p"last_head_ptr=$head_ptr  last_tail_ptr=$tail_ptr\n")
//  XSInfo(full, "Queue is full\n")
}
