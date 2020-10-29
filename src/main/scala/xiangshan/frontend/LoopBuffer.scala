package xiangshan.frontend

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.ExcitingUtils._
import utils._
import xiangshan._
import xiangshan.cache._

class LoopBufferParameters extends XSBundle {
  val LBredirect = ValidIO(UInt(VAddrBits.W))
  val tgtpc = Input(UInt(VAddrBits.W))
  val inLoop = Output(Bool())
  val LBReq = Input(UInt(VAddrBits.W))
  val LBResp  = Output(new ICacheResp)
}

class LoopBufferIO extends XSBundle {
  val flush = Input(Bool())
  val in = Flipped(DecoupledIO(new FetchPacket))
  val out = Vec(DecodeWidth, DecoupledIO(new CtrlFlow))
  val loopBufPar = new LoopBufferParameters
}

class LoopBuffer extends XSModule {
  val io = IO(new LoopBufferIO)

  class IBufEntry extends XSBundle {
    val inst = UInt(32.W)
    val pc = UInt(VAddrBits.W)
    val pnpc = UInt(VAddrBits.W)
    val brInfo = new BranchInfo
    val pd = new PreDecodeInfo
    val ipf = Bool()
    val crossPageIPFFix = Bool()
  }

  class LBufEntry extends XSBundle {
    val inst = UInt(16.W)
  }

  // ignore
  for(i <- 0 until DecodeWidth) {
    // io.out(i).bits.exceptionVec := DontCare
    io.out(i).bits.intrVec := DontCare
    // io.out(i).bits.crossPageIPFFix := DontCare
  }

  def sbbOffest(inst: UInt): UInt = {
    val isJal = inst === BitPat("b1111_???????_111111111_?????_1101111")
    val isCon = inst === BitPat("b1111???_?????_?????_???_????1_1100011")
    val isRVCJal = inst === BitPat("b????????????????_001_1?111??????_01")
    val isRVCCon = inst === BitPat("b????????????????_11?_1??_???_?????_01")

    PriorityMux(Seq(
      isJal    -> inst(27, 21),
      isCon    -> Cat(inst(27,25), inst(11,8)),
      isRVCJal -> Cat(inst(6), inst(7), inst(2), inst(11), inst(5,3)),
      isRVCCon -> Cat(inst(6), inst(5), inst(2), inst(11,10), inst(4,3)),
      true.B   -> 0.U(7.W)
    ))
  }

  def isSBB(inst: UInt): Bool = {
    sbbOffest(inst) > 0.U // TODO < 56.U
  }

  // predTaken to OH
  val predTakenVec = Mux(io.in.bits.predTaken, Reverse(PriorityEncoderOH(Reverse(io.in.bits.mask))), 0.U(PredictWidth.W))

  // Loop detect register
  val offsetCounter = Reg(UInt((log2Up(IBufSize)+2).W))
  val tsbbPC = RegInit(0.U(VAddrBits.W))

  val brTaken = Cat((0 until PredictWidth).map(i => io.in.fire && io.in.bits.mask(i) && predTakenVec(i))).orR()
  val brIdx = OHToUInt(predTakenVec.asUInt)
  val sbbTaken = brTaken && isSBB(io.in.bits.instrs(brIdx))

  val tsbbVec = Cat((0 until PredictWidth).map(i => io.in.fire && io.in.bits.mask(i) && io.in.bits.pc(i) === tsbbPC))
  val hasTsbb = tsbbVec.orR()
  val tsbbIdx = OHToUInt(Reverse(tsbbVec))
  val tsbbTaken = brTaken && io.in.bits.pc(brIdx) === tsbbPC

  // IBuffer define
  val ibuf = Mem(IBufSize, new IBufEntry)
  val ibufValid = RegInit(VecInit(Seq.fill(IBufSize)(false.B)))
  val headPtr = RegInit(0.U(log2Up(IBufSize).W))
  val tailPtr = RegInit(0.U(log2Up(IBufSize).W))

  // val enqValid = !io.flush && !ibufValid(tailPtr + PopCount(io.in.bits.mask) - 1.U)
  val enqValid = !io.flush && (io.in.bits.mask === 0.U || !ibufValid(tailPtr + PopCount(io.in.bits.mask) - 1.U))
  val deqValid = !io.flush && ibufValid(headPtr)

  // LoopBuffer define
  val lbuf = Mem(IBufSize*2, new LBufEntry)
  val lbufValid = RegInit(VecInit(Seq.fill(IBufSize*2)(false.B)))

  // FSM state define
  val s_idle :: s_fill :: s_active :: Nil = Enum(3)
  val LBstate = RegInit(s_idle)

  io.loopBufPar.inLoop := LBstate === s_active

  def flushLB() = {
    for(i <- 0 until IBufSize*2) {
      lbuf(i).inst := 0.U // TODO: This is to make the debugging information clearer, this can be deleted
      lbufValid(i) := false.B
    }
  }

  def flushIB() = {
    for(i <- 0 until IBufSize) {
      ibuf(i).inst := 0.U // TODO: This is to make the debugging information clearer, this can be deleted
      ibuf(i).pc := 0.U // TODO: This is to make the debugging information clearer, this can be deleted
      lbuf(i).inst := 0.U // TODO: This is to make the debugging information clearer, this can be deleted
      ibufValid(i) := false.B
    }
    headPtr := 0.U
    tailPtr := 0.U
  }

  def flush() = {
    XSDebug("Loop Buffer Flushed.\n")
    LBstate := s_idle
    flushLB
    flushIB
  }

  io.loopBufPar.LBredirect.valid := false.B
  io.loopBufPar.LBredirect.bits := DontCare

  /*---------------*/
  /*    Dequeue    */
  /*---------------*/
  var deq_idx = WireInit(headPtr)

  when(deqValid) {
    for(i <- 0 until DecodeWidth) {
      var outWire = WireInit(ibuf(deq_idx))

      io.out(i).valid := ibufValid(deq_idx)
      when(ibufValid(deq_idx)) { ibufValid(deq_idx) := !io.out(i).fire }
      io.out(i).bits.instr := outWire.inst

      io.out(i).bits.pc := outWire.pc
      io.out(i).bits.exceptionVec := 0.U.asTypeOf(Vec(16, Bool()))
      io.out(i).bits.exceptionVec(instrPageFault) := outWire.ipf

      io.out(i).bits.brUpdate := DontCare
      io.out(i).bits.brUpdate.pc := outWire.pc
      io.out(i).bits.brUpdate.pnpc := outWire.pnpc
      io.out(i).bits.brUpdate.pd := outWire.pd
      io.out(i).bits.brUpdate.brInfo := outWire.brInfo
      io.out(i).bits.crossPageIPFFix := outWire.crossPageIPFFix

      deq_idx = deq_idx + io.out(i).fire
    }
    headPtr := deq_idx
  }.otherwise {
    io.out.foreach(_.valid := false.B)
    io.out.foreach(_.bits <> DontCare)
  }

  /*---------------*/
  /*    Enqueue    */
  /*---------------*/
  io.in.ready := enqValid

  var enq_idx = WireInit(tailPtr)
  // ExcitingUtils.addSource(io.in.fire && LBstate =/= s_active, "CntFetchFromICache", Perf)
  // ExcitingUtils.addSource(io.in.fire && LBstate === s_active, "CntFetchFromLoopBuffer", Perf)
  when(io.in.fire) {
    for(i <- 0 until PredictWidth) {
      var inWire = Wire(new IBufEntry)
      inWire := DontCare

      when(io.in.bits.mask(i)) {
        inWire.inst := io.in.bits.instrs(i)
        when(LBstate === s_fill/* || (sbbTaken && i.U > brIdx)*/) {
          lbuf(io.in.bits.pc(i)(7,1)).inst := io.in.bits.instrs(i)(15, 0)
          // lbuf(io.in.bits.pc(i)(7,1)).pd := io.in.bits.pd(i)
          lbufValid(io.in.bits.pc(i)(7,1)) := true.B
          when(!io.in.bits.pd(i).isRVC) {
            lbuf(io.in.bits.pc(i)(7,1) + 1.U).inst := io.in.bits.instrs(i)(31, 16)
            lbufValid(io.in.bits.pc(i)(7,1) + 1.U) := true.B
          }
        }
        inWire.pc := io.in.bits.pc(i)
        inWire.pnpc := io.in.bits.pnpc(i)
        inWire.brInfo := io.in.bits.brInfo(i)
        inWire.pd := io.in.bits.pd(i)
        inWire.ipf := io.in.bits.ipf
        inWire.crossPageIPFFix := io.in.bits.crossPageIPFFix

        // ibufValid(enq_idx) := Mux(LBstate =/= s_active, true.B, !(hasTsbb && !tsbbTaken && i.U > tsbbIdx))
        ibufValid(enq_idx) := true.B
        ibuf(enq_idx) := inWire
      }

      enq_idx = enq_idx + io.in.bits.mask(i)
    }

    tailPtr := enq_idx
  }

  // This is ugly
  val pcStep = (0 until PredictWidth).map(i => Mux(!io.in.fire || !io.in.bits.mask(i), 0.U, Mux(io.in.bits.pd(i).isRVC, 1.U, 2.U))).fold(0.U(log2Up(16+1).W))(_+_)
  val offsetCounterWire = WireInit(offsetCounter + pcStep)
  offsetCounter := offsetCounterWire

  // IFU fetch from LB
  io.loopBufPar.LBResp.pc := io.loopBufPar.LBReq
  io.loopBufPar.LBResp.data := Cat((31 to 0 by -1).map(i => lbuf(io.loopBufPar.LBReq(7,1) + i.U).inst))
  io.loopBufPar.LBResp.mask := Cat((31 to 0 by -1).map(i => lbufValid(io.loopBufPar.LBReq(7,1) + i.U)))
  io.loopBufPar.LBResp.ipf := false.B

  /*-----------------------*/
  /*    Loop Buffer FSM    */
  /*-----------------------*/
  when(io.in.fire) {
    switch(LBstate) {
      is(s_idle) {
        // To FILL
        // 检测到sbb且跳转，sbb成为triggering sbb
        when(sbbTaken) {
          LBstate := s_fill
          XSDebug("State change: FILL\n")
          // This is ugly
          // offsetCounter := Cat("b1".U, sbbOffest(io.in.bits.instrs(brIdx))) + 
          //   (0 until PredictWidth).map(i => Mux(!io.in.bits.mask(i) || i.U < brIdx, 0.U, Mux(io.in.bits.pd(i).isRVC, 1.U, 2.U))).fold(0.U(log2Up(16+1).W))(_+_)
          offsetCounter := Cat("b1".U, sbbOffest(io.in.bits.instrs(brIdx)))
          tsbbPC := io.in.bits.pc(brIdx)
        }
      }
      is(s_fill) {
        // To AVTIVE
        // triggering sbb 造成cof
        when(offsetCounterWire((log2Up(IBufSize)+2)-1) === 0.U){
          when(hasTsbb && tsbbTaken) {
            LBstate := s_active
            XSDebug("State change: ACTIVE\n")
          }.otherwise {
            LBstate := s_idle
            XSDebug("State change: IDLE\n")
            flushLB()
          }
        }

        when(brTaken && !tsbbTaken) {
          // To IDLE
          LBstate := s_idle
          XSDebug("State change: IDLE\n")
          flushLB()
        }
      }
      is(s_active) {
        // To IDLE
        // triggering sbb不跳转 退出循环
        val redirect_pc = io.in.bits.pnpc(PredictWidth.U - PriorityEncoder(Reverse(io.in.bits.mask)) - 1.U)
        when(hasTsbb && !tsbbTaken) {
          XSDebug("tsbb not taken, State change: IDLE\n")
          LBstate := s_idle
          io.loopBufPar.LBredirect.valid := true.B
          // io.loopBufPar.LBredirect.bits := tsbbPC + Mux(io.in.bits.pd(tsbbIdx).isRVC, 2.U, 4.U)
          io.loopBufPar.LBredirect.bits := redirect_pc
          // ExcitingUtils.addSource(true.B, "CntLBRedirect1", Perf)
          XSDebug(p"redirect pc=${Hexadecimal(redirect_pc)}\n")
          flushLB()
        }

        when(brTaken && !tsbbTaken) {
          XSDebug("cof by other inst, State change: IDLE\n")
          LBstate := s_idle
          io.loopBufPar.LBredirect.valid := true.B
          io.loopBufPar.LBredirect.bits := redirect_pc
          // io.loopBufPar.LBredirect.bits := Mux(brIdx > tsbbIdx, tsbbPC + 4.U, io.loopBufPar.LBReq)
          // ExcitingUtils.addSource(true.B, "CntLBRedirect2", Perf)
          XSDebug(p"redirect pc=${Hexadecimal(redirect_pc)}\n")
          flushLB()
        }

        when(hasTsbb && brTaken && !tsbbTaken) {
          XSDebug("tsbb and cof, State change: IDLE\n")
          LBstate := s_idle
          io.loopBufPar.LBredirect.valid := true.B
          io.loopBufPar.LBredirect.bits := redirect_pc
          // io.loopBufPar.LBredirect.bits := Mux(brIdx > tsbbIdx, tsbbPC + 4.U, io.loopBufPar.LBReq)
          // ExcitingUtils.addSource(true.B, "CntLBRedirect3", Perf)
          XSDebug(p"redirect pc=${Hexadecimal(redirect_pc)}\n")
          flushLB()
        }
      }
    }
  }

  when(io.flush){
    flush()
  }

  // Debug Info
  XSDebug(io.flush, "LoopBuffer Flushed\n")

  XSDebug(LBstate === s_idle, "Current state: IDLE\n")
  XSDebug(LBstate === s_fill, "Current state: FILL\n")
  XSDebug(LBstate === s_active, "Current state: ACTIVE\n")

  XSDebug(p"offsetCounter = ${Binary(offsetCounterWire)}\n")
  XSDebug(p"tsbbIdx = ${tsbbIdx}\n")
  when(io.in.fire) {
    XSDebug("Enque:\n")
    XSDebug(brTaken, p"Detected jump, idx=${brIdx}\n")
    XSDebug(p"predTaken=${io.in.bits.predTaken}, predTakenVec=${Binary(predTakenVec)}\n")
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

  XSDebug(p"last_headPtr=$headPtr  last_tailPtr=$tailPtr\n")
  XSDebug("IBuffer:\n")
  for(i <- 0 until IBufSize/8) {
    XSDebug("%x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b\n",
      ibuf(i*8+0).inst, ibufValid(i*8+0),
        ibuf(i*8+1).inst, ibufValid(i*8+1),
        ibuf(i*8+2).inst, ibufValid(i*8+2),
        ibuf(i*8+3).inst, ibufValid(i*8+3),
        ibuf(i*8+4).inst, ibufValid(i*8+4),
        ibuf(i*8+5).inst, ibufValid(i*8+5),
        ibuf(i*8+6).inst, ibufValid(i*8+6),
        ibuf(i*8+7).inst, ibufValid(i*8+7)
    )
  }

  XSDebug("LoopBuffer:\n")
  for(i <- 0 until IBufSize*2/8) {
    XSDebug("%x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b\n",
      lbuf(i*8+0).inst, lbufValid(i*8+0),
        lbuf(i*8+1).inst, lbufValid(i*8+1),
        lbuf(i*8+2).inst, lbufValid(i*8+2),
        lbuf(i*8+3).inst, lbufValid(i*8+3),
        lbuf(i*8+4).inst, lbufValid(i*8+4),
        lbuf(i*8+5).inst, lbufValid(i*8+5),
        lbuf(i*8+6).inst, lbufValid(i*8+6),
        lbuf(i*8+7).inst, lbufValid(i*8+7)
    )
  }
}