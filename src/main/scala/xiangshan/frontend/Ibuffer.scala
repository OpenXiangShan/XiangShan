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
    val inst = UInt(16.W)
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

  // ignore
  for(i <- 0 until DecodeWidth) {
    io.out(i).bits.exceptionVec := DontCare
    io.out(i).bits.intrVec := DontCare
    io.out(i).bits.brUpdate := DontCare
    io.out(i).bits.crossPageIPFFix := DontCare
  }

  //mask initial
  //  val mask = Wire(Vec(FetchWidth*2, false.B))
  //  (0 until 16).map(i => mask(i.U) := (io.in.bits.pc(4,1) <= i.U))

  // ibuf define
  val ibuf = Mem(IBufSize*2, new IBufEntry)
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
    var enq_idx = WireInit(tail_ptr)
    for(i <- 0 until FetchWidth*2) {
      when(io.in.bits.mask(i)) {
        ibuf(enq_idx).inst := Mux(i.U(0), io.in.bits.instrs(i>>1)(31,16), io.in.bits.instrs(i>>1)(15,0))
        ibuf(enq_idx).pc := io.in.bits.pc + ((enq_idx - tail_ptr)<<1).asUInt
        ibuf(enq_idx).pnpc := io.in.bits.pnpc(i)
        ibuf(enq_idx).fetchOffset := ((enq_idx - tail_ptr) << 1).asUInt
        ibuf(enq_idx).hist := io.in.bits.hist(i)
        // ibuf(enq_idx).btbVictimWay := io.in.bits.btbVictimWay
        ibuf(enq_idx).btbPredCtr := io.in.bits.predCtr(i)
        ibuf(enq_idx).btbHit := io.in.bits.btbHit(i)
        ibuf(enq_idx).tageMeta := io.in.bits.tageMeta(i)
        ibuf(enq_idx).rasSp := io.in.bits.rasSp
        ibuf(enq_idx).rasTopCtr := io.in.bits.rasTopCtr
        ibuf_valid(enq_idx) := true.B
        XSDebug("Enq: i:%d idx:%d mask:%b instr:%x pc:%x fetchOffset=%d\n",
          i.U, enq_idx, io.in.bits.mask(i), Mux(i.U(0), io.in.bits.instrs(i>>1)(31,16), io.in.bits.instrs(i>>1)(15,0)), io.in.bits.pc + ((enq_idx - tail_ptr)<<1).asUInt, ((enq_idx - tail_ptr) << 1).asUInt)
      }

      // XSDebug(!(i.U)(0), "Enq: i:%d Idx:%d mask:%b instr:%x pc:%x pnpc:%x\n",
      //   (i/2).U, enq_idx, io.in.bits.mask(i), io.in.bits.instrs(i/2), io.in.bits.pc + ((enq_idx - tail_ptr)<<1).asUInt, io.in.bits.pnpc(i/2))
      enq_idx = enq_idx + io.in.bits.mask(i)
    }

    tail_ptr := enq_idx
    last_enq := true.B
  }

  // deque
  when(deqValid) {
    var deq_idx = WireInit(head_ptr)
    for(i <- 0 until DecodeWidth) {
      io.out(i).valid := ibuf_valid(deq_idx) // FIXME: need fix me when support RVC
      when(ibuf_valid(deq_idx)) {
        when(ibuf(deq_idx).inst(1,0) =/= "b11".U) {
          // is RVC
          io.out(i).bits.instr := Cat(0.U(16.W), ibuf(deq_idx).inst)
          io.out(i).bits.pc := ibuf(deq_idx).pc
          io.out(i).bits.brUpdate.pnpc := ibuf(deq_idx).pnpc
          io.out(i).bits.brUpdate.fetchOffset := ibuf(deq_idx).fetchOffset
          io.out(i).bits.brUpdate.hist := ibuf(deq_idx).hist
          // io.out(i).bits.btbVictimWay := ibuf(deq_idx).btbVictimWay
          io.out(i).bits.brUpdate.btbPredCtr := ibuf(deq_idx).btbPredCtr
          io.out(i).bits.brUpdate.btbHit := ibuf(deq_idx).btbHit
          io.out(i).bits.brUpdate.tageMeta := ibuf(deq_idx).tageMeta
          io.out(i).bits.brUpdate.rasSp := ibuf(deq_idx).rasSp
          io.out(i).bits.brUpdate.rasTopCtr := ibuf(deq_idx).rasTopCtr
          io.out(i).bits.brUpdate.isRVC := true.B
          ibuf_valid(deq_idx) := !io.out(i).fire
        }.elsewhen(ibuf_valid(deq_idx + 1.U)) {
          // isn't RVC
          io.out(i).bits.instr := Cat(ibuf(deq_idx+1.U).inst, ibuf(deq_idx).inst)
          io.out(i).bits.pc := ibuf(deq_idx).pc
          io.out(i).bits.brUpdate.pnpc := ibuf(deq_idx).pnpc
          io.out(i).bits.brUpdate.fetchOffset := ibuf(deq_idx).fetchOffset
          io.out(i).bits.brUpdate.hist := ibuf(deq_idx).hist
          // io.out(i).bits.btbVictimWay := ibuf(deq_idx).btbVictimWay
          io.out(i).bits.brUpdate.btbPredCtr := ibuf(deq_idx).btbPredCtr
          io.out(i).bits.brUpdate.btbHit := ibuf(deq_idx).btbHit
          io.out(i).bits.brUpdate.tageMeta := ibuf(deq_idx).tageMeta
          io.out(i).bits.brUpdate.rasSp := ibuf(deq_idx).rasSp
          io.out(i).bits.brUpdate.rasTopCtr := ibuf(deq_idx).rasTopCtr
          io.out(i).bits.brUpdate.isRVC := false.B
          ibuf_valid(deq_idx) := !io.out(i).fire
          ibuf_valid(deq_idx+1.U) := !io.out(i).fire
        }.otherwise {
          // half inst keep in buffer
          io.out(i).bits.instr := 0.U(32.W)
          io.out(i).bits.pc := 0.U(VAddrBits.W)
          io.out(i).bits.brUpdate.pnpc := 0.U(VAddrBits.W)
          io.out(i).bits.brUpdate.fetchOffset := 0.U(log2Up(FetchWidth*4).W)
          io.out(i).bits.brUpdate.hist := 0.U(HistoryLength.W)
          // io.out(i).bits.btbVictimWay := 0.U(log2Up(BtbWays).W)
          io.out(i).bits.brUpdate.btbPredCtr := 0.U(2.W)
          io.out(i).bits.brUpdate.btbHit := false.B
          io.out(i).bits.brUpdate.tageMeta := 0.U.asTypeOf(new TageMeta)
          io.out(i).bits.brUpdate.rasSp := 0.U(log2Up(RasSize))
          io.out(i).bits.brUpdate.rasTopCtr := 0.U(8.W)
          io.out(i).bits.brUpdate.isRVC := false.B
          io.out(i).valid := false.B
        }
      }.otherwise {
        io.out(i).bits.instr := Cat(ibuf(head_ptr + (i<<1).U + 1.U).inst, ibuf(head_ptr + (i<<1).U).inst)
        io.out(i).bits.pc := ibuf(head_ptr + (i<<1).U).pc
        io.out(i).bits.brUpdate.pnpc := ibuf(head_ptr + (i<<1).U).pnpc
        io.out(i).bits.brUpdate.fetchOffset := ibuf(head_ptr + (i<<1).U).fetchOffset
        io.out(i).bits.brUpdate.hist := ibuf(head_ptr + (i<<1).U).hist
        // io.out(i).bits.btbVictimWay := ibuf(head_ptr + (i<<1).U).btbVictimWay
        io.out(i).bits.brUpdate.btbPredCtr := ibuf(head_ptr + (i<<1).U).btbPredCtr
        io.out(i).bits.brUpdate.btbHit := ibuf(head_ptr + (i<<1).U).btbHit
        io.out(i).bits.brUpdate.tageMeta := ibuf(head_ptr + (i<<1).U).tageMeta
        io.out(i).bits.brUpdate.rasSp := ibuf(head_ptr + (i<<1).U).rasSp
        io.out(i).bits.brUpdate.rasTopCtr := ibuf(head_ptr + (i<<1).U).rasTopCtr
        io.out(i).bits.brUpdate.isRVC := false.B
      }
      XSDebug(deqValid, p"Deq: i:${i.U} valid:${ibuf_valid(deq_idx)} idx=${Decimal(deq_idx)} ${Decimal(deq_idx + 1.U)} instr:${Hexadecimal(io.out(i).bits.instr)} PC=${Hexadecimal(io.out(i).bits.pc)} v=${io.out(i).valid}  r=${io.out(i).ready}\n")

      // When can't deque, deq_idx+0
      // when RVC deque, deq_idx+1
      // when not RVC deque, deq_idx+2
      // when only have half inst, keep it in buffer
      deq_idx = deq_idx + PriorityMux(Seq(
        !(io.out(i).ready && ibuf_valid(deq_idx)) -> 0.U,
        (ibuf(deq_idx).inst(1,0) =/= "b11".U) -> 1.U,
        ibuf_valid(deq_idx + 1.U) -> 2.U
      ))
    }
    head_ptr := deq_idx

    last_enq := false.B
  }.otherwise {
    for(i <- 0 until DecodeWidth) {
      io.out(i).bits.instr := 0.U
      io.out(i).bits.pc := 0.U
      io.out(i).bits.brUpdate.pnpc := 0.U
      io.out(i).bits.brUpdate.fetchOffset := 0.U
      io.out(i).bits.brUpdate.hist := 0.U(HistoryLength.W)
      // io.out(i).bits.btbVictimWay := 0.U(log2Up(BtbWays).W)
      io.out(i).bits.brUpdate.btbPredCtr := 0.U(2.W)
      io.out(i).bits.brUpdate.btbHit := false.B
      io.out(i).bits.brUpdate.tageMeta := 0.U.asTypeOf(new TageMeta)
      io.out(i).bits.brUpdate.rasSp := 0.U(log2Up(RasSize))
      io.out(i).bits.brUpdate.rasTopCtr := 0.U(8.W)
      io.out(i).bits.brUpdate.isRVC := false.B
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
  XSDebug(p"head_ptr=$head_ptr  tail_ptr=$tail_ptr\n")
//  XSInfo(full, "Queue is full\n")
}
