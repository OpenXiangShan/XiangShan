package xiangshan.frontend

import chisel3._
import chisel3.util._

import xiangshan._
import xiangshan.utils._

class LbufEntry extends XSBundle {
  val inst = UInt(32.W)
  val pc = UInt(VAddrBits.W)
  val valid = Bool()
  val isTaken = Bool()
}

class LoopBuffer extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val redirect = Input(new BranchPrediction)
    val in = Flipped(DecoupledIO(new FetchPacket))
    val out = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  })

  // ignore
  for(i <- 0 until DecodeWidth) {
    io.out(i).bits.exceptionVec := DontCare
    io.out(i).bits.intrVec := DontCare
    io.out(i).bits.isBr := DontCare
    io.out(i).bits.isRVC := DontCare
  }

  // Check Short Backward Branch
  def isSBB(inst: UInt): Bool = {
    // is jal
    val isJal = inst === BitPat("b1111_???????_111111111_?????_1101111") && inst =/= BitPat("b????????????_?????_000_?????_1100111")
    val isConj = inst === BitPat("b111111?_?????_?????_???_????1_1100011")
//    Mux(isJal, (true.B, inst(25, 21)), Mux(isConj, (true.B, Cat(inst(25), inst(11,8))), (false.B, 0.U(5.W))))
    Mux(isJal, true.B, Mux(isConj, true.B, false.B))

  }

  // Get sbb target
  def SBBOffset(inst: UInt): UInt = {
    val isJal = inst === BitPat("b1111_???????_111111111_?????_1101111") && inst =/= BitPat("b????????????_?????_000_?????_1100111")
    val isConj = inst === BitPat("b111111?_?????_?????_???_????1_1100011")
    Mux(isJal, inst(27, 21), Mux(isConj, Cat(inst(25), inst(11,8)), 0.U(5.W)))
  }

  def isJal(inst: UInt): Bool = {
    inst === BitPat("b????????????????????_?????_1101111") || inst === BitPat("b???????_?????_?????_???_?????_1100011")
  }

  // Loop Buffer define
  val lbuf = Reg(Vec(IBufSize, new LbufEntry))
  val head_ptr = RegInit(0.U(log2Up(IBufSize).W))
  val tail_ptr = RegInit(0.U(log2Up(IBufSize).W))

  val loop_str = RegInit(0.U(log2Up(IBufSize).W))
  val loop_end = RegInit(0.U(log2Up(IBufSize).W))
  val loop_ptr = RegInit(0.U(log2Up(IBufSize).W))

  //Count Register
  val countReg = Reg(UInt((log2Up(IBufSize)+1).W))
  val tsbbPC = Reg(UInt(VAddrBits.W))

//  def isFull(ptr1: UInt, ptr2: UInt): Bool = (ptr1.head(1)=/=ptr2.head(1)) && (ptr1.tail(1)===ptr2.tail(1))
//  def isEmpty(ptr1: UInt, ptr2: UInt): Bool = ptr1 === ptr2
//  def isOverflow(ptr1: UInt, ptr2: UInt): Bool = (ptr1.head(1)===ptr2.head(1) && ptr1.tail(1) > ptr2.tail(1)) || (ptr1.head(1)=/=ptr2.head(1) && ptr1.tail(1) < ptr2.tail(1))
  def isFull(ptr1: UInt, ptr2: UInt): Bool = ptr1 === ptr2 && lbuf(ptr2).valid
  def isEmpty(ptr1: UInt, ptr2: UInt): Bool = ptr1 === ptr2 && !lbuf(ptr1).valid
  def isOverflow(ptr: UInt): Bool = lbuf(ptr).valid

  val s_idle :: s_fill :: s_active :: Nil = Enum(3)
  val LBstate = RegInit(s_idle)

  // dequeue
  var deq_idx = 0.U(log2Up(DecodeWidth+1).W)

  when(LBstate =/= s_active) {
    for(i <- 0 until DecodeWidth) {
      io.out(i).valid := !isEmpty(head_ptr + deq_idx, tail_ptr)
      io.out(i).bits.instr := lbuf(head_ptr + deq_idx).inst
      io.out(i).bits.pc := lbuf(head_ptr + deq_idx).pc
      lbuf(head_ptr + deq_idx).valid := lbuf(head_ptr + deq_idx).valid && LBstate === s_fill

      deq_idx = deq_idx + !isEmpty(head_ptr + deq_idx, tail_ptr)
    }

    head_ptr := head_ptr + deq_idx
  }.otherwise {
    for(i <- 0 until DecodeWidth) {
      io.out(i).valid := Mux(deq_idx === DecodeWidth.U, false.B, lbuf(loop_ptr + deq_idx).valid)
      io.out(i).bits.instr := lbuf(loop_ptr + deq_idx).inst
      io.out(i).bits.pc := lbuf(loop_ptr + deq_idx).pc
      io.out(i).bits.isRVC := DontCare

      deq_idx = Mux(loop_ptr + deq_idx === loop_end, DecodeWidth.U, deq_idx + 1.U)
    }

    loop_ptr := Mux(deq_idx === DecodeWidth.U, loop_str, loop_ptr + deq_idx)
  }

  val countRegWire = countReg + (PopCount((0 until DecodeWidth).map(io.out(_).fire())) << 2).asUInt
  countReg := countRegWire

  // enqueue
  var enq_idx = 0.U(log2Up(FetchWidth+1).W)

  io.in.ready := !isOverflow(tail_ptr + FetchWidth.U) && LBstate =/= s_active

  when(io.in.fire() && LBstate =/= s_active){
    for(i <- 0 until FetchWidth) {
      lbuf(tail_ptr + enq_idx).inst := io.in.bits.instrs(i)
      lbuf(tail_ptr + enq_idx).pc := io.in.bits.pc + (i<<2).U
      lbuf(tail_ptr + enq_idx).valid := io.in.bits.mask(i<<1) && io.redirect.instrValid(i)
      lbuf(tail_ptr + enq_idx).isTaken := io.redirect.redirect

      enq_idx = enq_idx + (io.in.bits.mask(i<<1) && io.redirect.instrValid(i))
    }
    tail_ptr := tail_ptr + enq_idx
  }

  //Loop Buffer FSM
  switch(LBstate) {
    is(s_idle) {
      // To FILL
      // 检测到sbb且跳转，sbb成为triggrting sbb
      for(i <- 0 until DecodeWidth) {
        val issbb = isSBB(io.out(i).bits.instr)
        val sbbOffset = SBBOffset(io.out(i).bits.instr)
        when(issbb && lbuf(head_ptr + i.U).isTaken) {
          LBstate := s_fill
          XSDebug("State change: FILL\n")
          countReg := Cat("b1".U, sbbOffset)
          tsbbPC := io.out(i).bits.pc
          loop_str := head_ptr + deq_idx
        }
      }
    }
    is(s_fill) {
      when(countRegWire.head(1) === 0.U){
        when((0 until DecodeWidth).map(i => io.out(i).bits.pc === tsbbPC).reduce(_||_)) {
          val tsbbIdx = DecodeWidth.U - OHToUInt((0 until DecodeWidth).map(i => io.out(i).bits.pc === tsbbPC)) - 1.U
          when(lbuf(head_ptr + tsbbIdx).isTaken) {
            // To ACTIVE
            // triggering sbb造成cof
            LBstate := s_active
            XSDebug("State change: ACTIVE\n")
            loop_end := head_ptr + tsbbIdx
            XSDebug("loop_end=%d\n", head_ptr + tsbbIdx)
            loop_ptr := loop_str
          }.otherwise {
            // triggering sbb不跳转
            // To IDLE
            LBstate := s_idle
            XSDebug("State change: IDLE\n")
          }
        }
      }

      // 非triggering sbb造成的cof
      when((0 until DecodeWidth).map(i => !isSBB(io.out(i).bits.instr) && isJal(io.out(i).bits.instr) && lbuf(tail_ptr + i.U).isTaken).reduce(_||_)) {
        // To IDLE
        LBstate := s_idle
        XSDebug("State change: IDLE\n")
      }

      // To FILL
      // 非cof
    }
    is(s_active) {
      // To IDLE
      // triggering sbb不跳转 或 非triggering sbb造成的cof
      when((0 until DecodeWidth).map(i => io.out(i).bits.pc === tsbbPC && !lbuf(tail_ptr + i.U).isTaken).reduce(_||_)) {
        // To IDLE
        LBstate := s_idle
        XSDebug("State change: IDLE\n")
      }

      when((0 until DecodeWidth).map(i => isJal(io.out(i).bits.instr) && lbuf(tail_ptr + i.U).isTaken).reduce(_||_)) {
        // To IDLE
        LBstate := s_idle
        XSDebug("State change: IDLE\n")
      }

      // To ACTIVE
      // 非cof 或 triggering sbb造成的cof
    }
  }

  // flush
  when(io.flush) {
    for(i <- 0 until IBufSize) {
      lbuf(i).inst := 0.U
      lbuf(i).pc := 0.U
      lbuf(i).valid := false.B
    }
    head_ptr := 0.U
    tail_ptr := 0.U
  }

  // Debug Info
  // XSDebug(io.in.fire(), p"PC= ${Hexadecimal(io.in.bits.pc)}\n")
  XSDebug(io.flush, "Loop Buffer Flushed\n")

//  when(io.in.valid) {
//    XSDebug(p"PC=${Hexadecimal(io.in.bits.pc)}\n")
//    for(i <- 0 until FetchWidth){
//      XSDebug(p"${Hexadecimal(io.in.bits.instrs(i))}  v=${io.in.valid}  r=${io.in.ready}\n")
//    }
//  }

  when((0 until DecodeWidth).map(i => io.out(i).ready).reduce(_||_)){
    for(i <- 0 until DecodeWidth){
      XSDebug(p"${Hexadecimal(io.out(i).bits.instr)}  PC=${Hexadecimal(io.out(i).bits.pc)}  v=${io.out(i).valid}  r=${io.out(i).ready}\n")
    }
  }

  XSDebug(p"last_head_ptr=$head_ptr  last_tail_ptr=$tail_ptr\n")

  // Print loop buffer
  for(i <- 0 until IBufSize/8) {
    XSDebug("%x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b\n",
      lbuf(i*8+0).inst, lbuf(i*8+0).valid,
      lbuf(i*8+1).inst, lbuf(i*8+1).valid,
      lbuf(i*8+2).inst, lbuf(i*8+2).valid,
      lbuf(i*8+3).inst, lbuf(i*8+3).valid,
      lbuf(i*8+4).inst, lbuf(i*8+4).valid,
      lbuf(i*8+5).inst, lbuf(i*8+5).valid,
      lbuf(i*8+6).inst, lbuf(i*8+6).valid,
      lbuf(i*8+7).inst, lbuf(i*8+7).valid
    )
  }
}
