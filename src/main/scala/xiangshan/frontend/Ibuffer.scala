package xiangshan.frontend

import chisel3._
import chisel3.util._

import xiangshan._
import xiangshan.utils._

class Ibuffer extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = Flipped(DecoupledIO(new FetchPacket))
    val out = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  })

  val debug = false
  if(debug){
//    printf("\n------New Cycle------\n")
    when(io.in.valid) {
      printf("cache data:\n")
      for (i <- 0 until FetchWidth) {
        printf("%b\n", io.in.bits.instrs(i))
      }
      printf("\n")
    }
  }

  // ignore
  for(i <- 0 until DecodeWidth) {
    io.out(i).bits.exceptionVec := DontCare
    io.out(i).bits.intrVec := DontCare
    io.out(i).bits.isBr := DontCare
  }

  // ibuf define
  val ibuf = RegInit(VecInit(Seq.fill(IBufSize*2)(0.U(16.W))))
  val ibuf_pc = RegInit(VecInit(Seq.fill(IBufSize*2)(0.U(VAddrBits.W))))
  val ibuf_valid = RegInit(VecInit(Seq.fill(IBufSize*2)(false.B)))
  val head_ptr = RegInit(0.U(log2Up(IBufSize*2).W))
  val tail_ptr = RegInit(0.U(log2Up(IBufSize*2).W))

  // true: Last operation is enqueue
  // false: Last operation is deq_ueue
  val last_enq = RegInit(false.B)
  val full = head_ptr === tail_ptr && last_enq
  val empty = head_ptr === tail_ptr && !last_enq
  val enqValid = !io.flush && io.in.valid && !full && !ibuf_valid(tail_ptr + (FetchWidth*2).U)
  val deqValid = !io.flush && !empty && io.out.map(x => x.ready).reduce(_||_)

  io.in.ready := enqValid

  // enque
  when(enqValid) {
    if(debug) {
      printf("\n*****Enque start*****\n")
    }

    var enq_idx = 0.U(log2Up(FetchWidth*2+1).W)
    for(i <- 0 until FetchWidth) {
      when(io.in.bits.mask(i)) {
        ibuf(tail_ptr + enq_idx) := io.in.bits.instrs(i)(15,0)
        ibuf_pc(tail_ptr + enq_idx) := io.in.bits.pc + enq_idx + enq_idx
        ibuf_valid(tail_ptr + enq_idx) := true.B

        ibuf(tail_ptr + enq_idx+1.U) := io.in.bits.instrs(i)(31,16)
        ibuf_pc(tail_ptr + enq_idx+1.U) := io.in.bits.pc + enq_idx + enq_idx + 2.U
        ibuf_valid(tail_ptr + enq_idx+1.U) := true.B

        if(debug) {
          printf("%b\n", io.in.bits.instrs(i)(15,0))
          printf("%b\n", io.in.bits.instrs(i)(31,16))
        }
      }
      enq_idx = enq_idx + io.in.bits.mask(i) + io.in.bits.mask(i)
    }

    tail_ptr := tail_ptr + enq_idx
    last_enq := true.B

    if(debug) {
      printf("tail_ptr: %d\n", tail_ptr + enq_idx)
    }
  }

  // deque
  when(deqValid) {
    if(debug) {
      printf("\n*****Deque start*****\n")
      printf("Last head_ptr: %d, tail_ptr: %d\n", head_ptr, tail_ptr)
    }

    var deq_idx = 0.U(log2Up(DecodeWidth*2+1).W)
    for(i <- 0 until DecodeWidth) {
      when(io.out(i).ready && ibuf_valid(head_ptr + deq_idx)) {
        when(ibuf(head_ptr + deq_idx)(1,0) =/= "b11".U) {
          // is RVC
          io.out(i).bits.instr := Cat(0.U(16.W), ibuf(head_ptr + deq_idx))
          io.out(i).bits.pc := ibuf_pc(head_ptr + deq_idx)
          if(debug) {
            printf("RCV: %b    ", Cat(0.U(16.W), ibuf(head_ptr + deq_idx)))
            printf("PC: %d\n", ibuf_pc(head_ptr + deq_idx))
          }
          io.out(i).bits.isRVC := true.B
          io.out(i).valid := true.B
          ibuf_valid(head_ptr + deq_idx) := false.B
        }.elsewhen(ibuf_valid(head_ptr + deq_idx + 1.U)) {
          // isn't RVC
          io.out(i).bits.instr := Cat(ibuf(head_ptr + deq_idx+1.U), ibuf(head_ptr + deq_idx))
          io.out(i).bits.pc := ibuf_pc(head_ptr + deq_idx)
          if(debug) {
            printf("NOT RVC: %b    ", Cat(ibuf(head_ptr + deq_idx+1.U), ibuf(head_ptr + deq_idx)))
            printf("PC: %d\n", ibuf_pc(head_ptr + deq_idx))
          }
          io.out(i).bits.isRVC := false.B
          io.out(i).valid := true.B
          ibuf_valid(head_ptr + deq_idx) := false.B
          ibuf_valid(head_ptr + deq_idx+1.U) := false.B
        }.otherwise {
          // half inst keep in buffer
          io.out(i).bits.instr := 0.U(32.W)
          if(debug) {
            printf("This is half inst\n")
          }
          io.out(i).bits.pc := 0.U(VAddrBits.W)
          io.out(i).bits.isRVC := false.B
          io.out(i).valid := false.B
        }
      }.otherwise {
        if(debug) {
          printf("This output is not ready, or buffer is empty\n")
        }

        io.out(i).bits.instr := 0.U
        io.out(i).bits.pc := 0.U
        io.out(i).bits.isRVC := false.B
        io.out(i).valid := false.B
      }

      if(debug) {
        printf("deq_idx: %d\n", deq_idx)
      }

      // When can't deque, deq_idx+0
      // when RVC deque, deq_idx+1
      // when not RVC deque, deq_idx+2
      // when only have half inst, keep it in buffer
      deq_idx = deq_idx +
                (io.out(i).ready && ibuf_valid(head_ptr + deq_idx) && ibuf(head_ptr + deq_idx)(1,0) =/= "b11".U) +
                (io.out(i).ready && ibuf_valid(head_ptr + deq_idx) && !(ibuf(head_ptr + deq_idx)(1,0) =/= "b11".U) && ibuf_valid(head_ptr + deq_idx + 1.U)) +
                (io.out(i).ready && ibuf_valid(head_ptr + deq_idx) && !(ibuf(head_ptr + deq_idx)(1,0) =/= "b11".U) && ibuf_valid(head_ptr + deq_idx + 1.U))
    }
    head_ptr := head_ptr + deq_idx
    if(debug) {
      printf("head_ptr: %d\n", head_ptr + deq_idx)
    }
    last_enq := false.B
  }.otherwise {
    for(i <- 0 until DecodeWidth) {
      io.out(i).bits.instr := 0.U
      io.out(i).bits.pc := 0.U
      io.out(i).bits.isRVC := false.B
      io.out(i).valid := false.B
    }
  }

  // flush
  when(io.flush) {
    for(i <- 0 until IBufSize) {
      ibuf_valid(i) := false.B
      head_ptr := 0.U
      tail_ptr := 0.U
    }

    for(i <- 0 until DecodeWidth) {
      io.out(i).valid := false.B
    }
  }
}