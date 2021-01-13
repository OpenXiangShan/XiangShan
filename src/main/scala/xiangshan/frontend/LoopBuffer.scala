// package xiangshan.frontend

// import chisel3._
// import chisel3.util._
// import utils._
// import xiangshan._
// import xiangshan.cache._

// class LoopBufferParameters extends XSBundle {
//   val LBredirect = ValidIO(UInt(VAddrBits.W))
//   val fetchReq = Input(UInt(VAddrBits.W))
//   val noTakenMask = Input(UInt(PredictWidth.W))
// }

// class LoopBufferIO extends XSBundle {
//   val flush = Input(Bool())
//   val in = Flipped(DecoupledIO(new FetchPacket))
//   val out = ValidIO(new ICacheResp)
//   val loopBufPar = new LoopBufferParameters
// }

// class FakeLoopBuffer extends XSModule {
//   val io = IO(new LoopBufferIO)

//   io.out <> DontCare
//   io.out.valid := false.B
//   io.in.ready := false.B
//   io.loopBufPar <> DontCare
//   io.loopBufPar.LBredirect.valid := false.B
// }

// class LoopBuffer extends XSModule with HasIFUConst{
//   val io = IO(new LoopBufferIO)

//   // FSM state define
//   val s_idle :: s_fill :: s_active :: Nil = Enum(3)
//   val LBstate = RegInit(s_idle)

//   io.out <> DontCare
//   io.out.valid := LBstate === s_active
//   io.in.ready := true.B

//   class LBufEntry extends XSBundle {
//     val inst = UInt(16.W)
//     // val tag = UInt(tagBits.W)
//   }

//   def sbboffset(inst: UInt) = {
//     val isJal = inst === BitPat("b1111_???????_111111111_?????_1101111")
//     val isCon = inst === BitPat("b1111???_?????_?????_???_????1_1100011")
//     val isRVCJal = inst === BitPat("b????????????????_001_1?111??????_01")
//     val isRVCCon = inst === BitPat("b????????????????_11?_1??_???_?????_01")

//     val rst = PriorityMux(Seq(
//       isJal    -> inst(27, 21),
//       isCon    -> Cat(inst(27,25), inst(11,8)),
//       isRVCJal -> Cat(inst(6), inst(7), inst(2), inst(11), inst(5,3)),
//       isRVCCon -> Cat(inst(6), inst(5), inst(2), inst(11,10), inst(4,3)),
//       true.B   -> 0.U(7.W)
//     ))

//     ((~rst).asUInt + 1.U, rst)
//   }

//   def isSBB(inst: UInt): Bool = {
//     val sbboffsetWire = WireInit(sbboffset(inst)._1)
//     sbboffsetWire > 0.U && sbboffsetWire <= 112.U // TODO < 56.U
//   }

//   // predTaken to OH
//   val predTakenVec = Mux(io.in.bits.predTaken, Reverse(PriorityEncoderOH(Reverse(io.in.bits.mask))), 0.U(PredictWidth.W))

//   // Loop detect register
//   val offsetCounter = Reg(UInt((log2Up(IBufSize)+2).W))
//   val tsbbPC = RegInit(0.U(VAddrBits.W))

//   val brTaken = Cat((0 until PredictWidth).map(i => io.in.fire && io.in.bits.mask(i) && predTakenVec(i))).orR()
//   val brIdx = OHToUInt(predTakenVec.asUInt)
//   val sbbTaken = brTaken && isSBB(io.in.bits.instrs(brIdx))

//   val tsbbVec = Cat((0 until PredictWidth).map(i => io.in.fire && io.in.bits.mask(i) && io.in.bits.pc(i) === tsbbPC))
//   val hasTsbb = tsbbVec.orR()
//   val tsbbIdx = OHToUInt(Reverse(tsbbVec))
//   val tsbbTaken = brTaken && io.in.bits.pc(brIdx) === tsbbPC

//   val buffer = Mem(IBufSize*2, new LBufEntry)
//   val bufferValid = RegInit(VecInit(Seq.fill(IBufSize*2)(false.B)))

//   val redirect_pc = io.in.bits.pnpc(PredictWidth.U - PriorityEncoder(Reverse(io.in.bits.mask)) - 1.U)

//   def flush() = {
//     XSDebug("Loop Buffer Flushed.\n")
//     LBstate := s_idle
//     for(i <- 0 until IBufSize*2) {
//       // buffer(i).inst := 0.U // TODO: This is to make the debugging information clearer, this can be deleted
//       bufferValid(i) := false.B
//     }
//   }

//   // Enque loop body
//   when(io.in.fire && LBstate === s_fill) {
//     io.loopBufPar.noTakenMask.asBools().zipWithIndex.map {case(m, i) =>
//       when(m) {
//         buffer(io.in.bits.pc(i)(7,1)).inst := io.in.bits.instrs(i)(15, 0)
//         bufferValid(io.in.bits.pc(i)(7,1)) := true.B
//         when(!io.in.bits.pd(i).isRVC) {
//           buffer(io.in.bits.pc(i)(7,1) + 1.U).inst := io.in.bits.instrs(i)(31, 16)
//           bufferValid(io.in.bits.pc(i)(7,1) + 1.U) := true.B // May need to be considered already valid
//         }
//       }
//     }
//   }

//   // This is ugly
//   val pcStep = (0 until PredictWidth).map(i => Mux(!io.in.fire || !io.in.bits.mask(i), 0.U, Mux(io.in.bits.pd(i).isRVC, 1.U, 2.U))).fold(0.U(log2Up(16+1).W))(_+_)
//   val offsetCounterWire = WireInit(offsetCounter + pcStep)
//   offsetCounter := offsetCounterWire

//   // Provide ICacheResp to IFU
//   when(LBstate === s_active) {
//     val offsetInBankWire = offsetInBank(io.loopBufPar.fetchReq)
//     io.out.bits.pc   := io.loopBufPar.fetchReq
//     io.out.bits.data := Cat((15 to 0 by -1).map(i => buffer(io.loopBufPar.fetchReq(7,1) + i.U).inst)) >> Cat(offsetInBankWire, 0.U(4.W))
//     io.out.bits.mask := Cat((15 to 0 by -1).map(i => bufferValid(io.loopBufPar.fetchReq(7,1) + i.U))) >> offsetInBankWire
//     io.out.bits.ipf  := false.B
//   }

//   io.loopBufPar.LBredirect.valid := false.B
//   io.loopBufPar.LBredirect.bits := DontCare

//   /*-----------------------*/
//   /*    Loop Buffer FSM    */
//   /*-----------------------*/
//   when(io.in.fire) {
//     switch(LBstate) {
//       is(s_idle) {
//         // To FILL
//         // 检测到sbb且跳转，sbb成为triggering sbb
//         when(sbbTaken) {
//           LBstate := s_fill
//           XSDebug("State change: FILL\n")
//           // This is ugly
//           // offsetCounter := Cat("b1".U, sbboffset(io.in.bits.instrs(brIdx))) + 
//           //   (0 until PredictWidth).map(i => Mux(!io.in.bits.mask(i) || i.U < brIdx, 0.U, Mux(io.in.bits.pd(i).isRVC, 1.U, 2.U))).fold(0.U(log2Up(16+1).W))(_+_)
//           offsetCounter := Cat("b1".U, sbboffset(io.in.bits.instrs(brIdx))._2)
//           tsbbPC := io.in.bits.pc(brIdx)
//         }
//       }
//       is(s_fill) {
//         // To AVTIVE
//         // triggering sbb 造成cof
//         when(offsetCounterWire((log2Up(IBufSize)+2)-1) === 0.U){
//           when(hasTsbb && tsbbTaken) {
//             LBstate := s_active
//             XSDebug("State change: ACTIVE\n")
//           }.otherwise {
//             LBstate := s_idle
//             XSDebug("State change: IDLE\n")
//             flush()
//           }
//         }

//         when(brTaken && !tsbbTaken) {
//           // To IDLE
//           LBstate := s_idle
//           XSDebug("State change: IDLE\n")
//           flush()
//         }
//       }
//       is(s_active) {
//         // To IDLE
//         // triggering sbb不跳转 退出循环
//         when(hasTsbb && !tsbbTaken) {
//           XSDebug("tsbb not taken, State change: IDLE\n")
//           LBstate := s_idle
//           io.loopBufPar.LBredirect.valid := true.B
//           io.loopBufPar.LBredirect.bits := redirect_pc
//           XSDebug(p"redirect pc=${Hexadecimal(redirect_pc)}\n")
//           flush()
//         }

//         when(brTaken && !tsbbTaken) {
//           XSDebug("cof by other inst, State change: IDLE\n")
//           LBstate := s_idle
//           io.loopBufPar.LBredirect.valid := true.B
//           io.loopBufPar.LBredirect.bits := redirect_pc
//           XSDebug(p"redirect pc=${Hexadecimal(redirect_pc)}\n")
//           flush()
//         }

//         when(hasTsbb && brTaken && !tsbbTaken) {
//           XSDebug("tsbb and cof, State change: IDLE\n")
//           LBstate := s_idle
//           io.loopBufPar.LBredirect.valid := true.B
//           io.loopBufPar.LBredirect.bits := redirect_pc
//           XSDebug(p"redirect pc=${Hexadecimal(redirect_pc)}\n")
//           flush()
//         }
//       }
//     }
//   }

//   when(io.flush){
//     flush()
//   }

//   // XSDebug(io.flush, "LoopBuffer Flushed\n")
//   // if (!env.FPGAPlatform ) {
//   //   ExcitingUtils.addSource(LBstate === s_active && hasTsbb && !tsbbTaken, "CntExitLoop1", Perf)
//   //   ExcitingUtils.addSource(LBstate === s_active && brTaken && !tsbbTaken, "CntExitLoop2", Perf)
//   //   ExcitingUtils.addSource(LBstate === s_active && hasTsbb && brTaken && !tsbbTaken, "CntExitLoop3", Perf)
//   // }

//   XSDebug(LBstate === s_idle, "Current state: IDLE\n")
//   XSDebug(LBstate === s_fill, "Current state: FILL\n")
//   XSDebug(LBstate === s_active, "Current state: ACTIVE\n")

//   XSDebug(p"offsetCounter = ${Binary(offsetCounterWire)}\n")
//   XSDebug(p"tsbbIdx = ${tsbbIdx}\n")
//   when(io.in.fire) {
//     XSDebug("Enque:\n")
//     XSDebug(brTaken, p"Detected jump, idx=${brIdx}\n")
//     XSDebug(p"predTaken=${io.in.bits.predTaken}, predTakenVec=${Binary(predTakenVec)}\n")
//     XSDebug(p"MASK=${Binary(io.in.bits.mask)}\n")
//     for(i <- 0 until PredictWidth){
//         XSDebug(p"PC=${Hexadecimal(io.in.bits.pc(i))} ${Hexadecimal(io.in.bits.instrs(i))}\n")
//     }
//   }

//   XSDebug("LoopBuffer:\n")
//   for(i <- 0 until IBufSize*2/8) {
//     XSDebug("%x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b | %x v:%b\n",
//       buffer(i*8+0).inst, bufferValid(i*8+0),
//         buffer(i*8+1).inst, bufferValid(i*8+1),
//         buffer(i*8+2).inst, bufferValid(i*8+2),
//         buffer(i*8+3).inst, bufferValid(i*8+3),
//         buffer(i*8+4).inst, bufferValid(i*8+4),
//         buffer(i*8+5).inst, bufferValid(i*8+5),
//         buffer(i*8+6).inst, bufferValid(i*8+6),
//         buffer(i*8+7).inst, bufferValid(i*8+7)
//     )
//   }

//   XSDebug(io.out.valid, p"fetch pc: ${Hexadecimal(io.loopBufPar.fetchReq)}\n")
//   XSDebug(io.out.valid, p"fetchIdx: ${io.loopBufPar.fetchReq(7,1)}\n")
//   XSDebug(io.out.valid, p"out data: ${Hexadecimal(io.out.bits.data)}\n")
//   XSDebug(io.out.valid, p"out mask: ${Binary(io.out.bits.mask)}\n")
//   XSDebug(io.out.valid, p"out pc  : ${Hexadecimal(io.out.bits.pc)}\n")
// }