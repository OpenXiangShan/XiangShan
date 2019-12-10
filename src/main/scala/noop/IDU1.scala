package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

class IDU1 extends NOOPModule with HasInstrType with HasExceptionNO {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CtrlFlowIO))
    val out = Decoupled(new CtrlFlowIO)
    val flush = Input(Bool())
    val redirect = new RedirectIO
  })

  val instr = Wire(UInt(32.W))
  val isRVC = instr(1,0) =/= "b11".U

  //RVC support FSM
  //only ensure pnpc given by this FSM is right. May need flush after 6 offset 32 bit inst
  val s_idle :: s_extra :: s_waitnext :: s_waitnext_thenj :: Nil = Enum(4) 
  val state = RegInit(UInt(2.W), s_idle)
  val pcOffsetR = RegInit(UInt(3.W), 0.U)
  val pcOffset = Mux(state === s_idle, io.in.bits.pc(2,0), pcOffsetR)
  val instIn = Cat(0.U(16.W), io.in.bits.instr)
  // val nextState = WireInit(0.U(2.W))
  val canGo = WireInit(false.B)
  val canIn = WireInit(false.B)
  val brIdx = io.in.bits.brIdx
  // val brIdx = 0.U
  val rvcFinish = pcOffset === 0.U && (!isRVC || brIdx(0)) || pcOffset === 4.U && (!isRVC || brIdx(0)) || pcOffset === 2.U && (isRVC || brIdx(1)) || pcOffset === 6.U && isRVC  
  // if brIdx(0) (branch taken at inst with offest 0), ignore the rest part of this instline
  // just get next pc and instline from IFU
  val rvcNext = pcOffset === 0.U && (isRVC && !brIdx(0)) || pcOffset === 4.U && (isRVC && !brIdx(0)) || pcOffset === 2.U && !isRVC && !brIdx(1)
  val rvcSpecial = pcOffset === 6.U && !isRVC && !brIdx(2)
  val rvcSpecialJump = pcOffset === 6.U && !isRVC && brIdx(2)
  val pnpcIsSeq = brIdx(3)
  // val pnpcIsSeqRight = io.in.bits.pnpc === (Cat(io.in.bits.pc(VAddrBits-1,2), 0.U(2.W)) + 4.U) // TODO: add a new user bit bpRight to do this 
  // assert(pnpcIsSeq === pnpcIsSeqRight)
  val flushIFU = (state === s_idle || state === s_extra) && rvcSpecial && io.in.valid && !pnpcIsSeq
  when(flushIFU){printf("flushIFU at pc %x offset %x timer:%d\n", io.in.bits.pc, pcOffset, GTimer())}
  assert(!flushIFU)
  val loadNextInstline = (state === s_idle || state === s_extra) && (rvcSpecial || rvcSpecialJump) && io.in.valid && pnpcIsSeq
  // val loadNextInstline =false.B
  val pcOut = WireInit(0.U(VAddrBits.W))
  val pnpcOut = WireInit(0.U(VAddrBits.W))
  val specialPCR = Reg(UInt(VAddrBits.W)) // reg for full inst that cross 2 inst line
  val specialNPCR = Reg(UInt(VAddrBits.W)) // reg for pnc for full inst jump that cross 2 inst line
  val specialInstR = Reg(UInt(16.W))
  val specialIPFR = RegInit(Bool(), false.B)
  val redirectPC = Cat(io.in.bits.pc(VAddrBits-1,3), 0.U(3.W))+"b1010".U // IDU can got get full inst from a single inst line  
  val rvcForceLoadNext = (pcOffset === 2.U && !isRVC && io.in.bits.pnpc(2,0) === 4.U && !brIdx(1))
  //------------------------------------------------------
  // rvcForceLoadNext is used to deal with: 
  // case 1:
  // 8010004a:	406007b7          	lui	a5,0x40600
  // 8010004e:	470d                	li	a4,3
  // 80100050:	00e78623          	sb	a4,12(a5) # 4060000c <_start-0x3faffff4>
  // For icache req inst in seq, if there is no rvcForceLoadNext, 
  // after 8010004e there will be 8010004c instead of 80100050
  //------------------------------------------------------
  // case 2:
  // 80100046:	406007b7          	lui	a5,0x40600
  // 8010004a:	470d              	li	a4,3
  // force load next instline into ID stage, if bp wrong, it will be flushed by flushIFU
  //------------------------------------------------------
  // if there is a j inst in current inst line, a redirect req will be sent by ALU before invalid inst exception being committed
  // when brIdx(1), next instline will just be branch target, eatline is no longer needed 

  // only for test, add this to pipeline when do real implementation
  // val predictBranch = io.in.valid && Mux(io.in.bits.pc(1), io.in.bits.pc + 2.U === io.in.bits.pnpc, io.in.bits.pc + 4.U === io.in.bits.pnpc)
  // val flush = rvcSpecial
  instr := Mux((state === s_waitnext || state === s_waitnext_thenj), Cat(instIn(15,0), specialInstR), LookupTree(pcOffset, List(
    "b000".U -> instIn(31,0),
    "b010".U -> instIn(31+16,16),
    "b100".U -> instIn(63,32),
    "b110".U -> instIn(63+16,32+16)
  )))

  io.redirect.target := redirectPC
  io.redirect.valid := flushIFU

  when(!io.flush){
    switch(state){
      is(s_idle){//decode current pc in pipeline
        canGo := rvcFinish || rvcNext
        canIn := rvcFinish || rvcForceLoadNext
        pcOut := io.in.bits.pc
        pnpcOut := Mux(rvcFinish, io.in.bits.pnpc, Mux(isRVC, io.in.bits.pc+2.U, io.in.bits.pc+4.U))
        when(io.out.fire() && rvcFinish){state := s_idle}
        when(io.out.fire() && rvcNext){
          state := s_extra
          pcOffsetR := pcOffset + Mux(isRVC, 2.U, 4.U)
        }
        when(rvcSpecial && io.in.valid){
          state := s_waitnext
          specialPCR := pcOut
          specialInstR := io.in.bits.instr(63,63-16+1) 
          specialIPFR := io.in.bits.exceptionVec(instrPageFault)
        }
        when(rvcSpecialJump && io.in.valid){
          state := s_waitnext_thenj
          specialPCR := pcOut
          specialNPCR := io.in.bits.pnpc
          specialInstR := io.in.bits.instr(63,63-16+1) 
          specialIPFR := io.in.bits.exceptionVec(instrPageFault)
        }
      }
      is(s_extra){//get 16 aligned inst, pc controled by this FSM
        canGo := rvcFinish || rvcNext
        canIn := rvcFinish || rvcForceLoadNext
        pcOut := Cat(io.in.bits.pc(VAddrBits-1,3), pcOffsetR(2,0)) 
        pnpcOut := Mux(rvcFinish, io.in.bits.pnpc, Mux(isRVC, pcOut+2.U, pcOut+4.U))
        when(io.out.fire() && rvcFinish){state := s_idle}
        when(io.out.fire() && rvcNext){
          state := s_extra
          pcOffsetR := pcOffset + Mux(isRVC, 2.U, 4.U)
        }
        when(rvcSpecial && io.in.valid){
          state := s_waitnext
          specialPCR := pcOut
          specialInstR := io.in.bits.instr(63,63-16+1) 
        }
        when(rvcSpecialJump && io.in.valid){
          state := s_waitnext_thenj
          specialPCR := pcOut
          specialNPCR := io.in.bits.pnpc
          specialInstR := io.in.bits.instr(63,63-16+1) 
        }
      }
      is(s_waitnext){//require next 64bits, for this inst has size 32 and offset 6
        //ignore bp result, use pc+4 instead
        pcOut := specialPCR
        pnpcOut := specialPCR + 4.U
        // pnpcOut := Mux(rvcFinish, io.in.bits.pnpc, Mux(isRVC, pcOut+2.U, pcOut+4.U))
        canGo := io.in.valid
        canIn := false.B
        when(io.out.fire()){
          state := s_extra
          pcOffsetR := "b010".U
        }
      }
      is(s_waitnext_thenj){//require next 64bits, for this inst has size 32 and offset 6
        //use bp result
        pcOut := specialPCR
        pnpcOut := specialNPCR
        // pnpcOut := Mux(rvcFinish, io.in.bits.pnpc, Mux(isRVC, pcOut+2.U, pcOut+4.U))
        canGo := io.in.valid
        canIn := true.B
        when(io.out.fire()){
          state := s_idle
        }
      }
      // is(s_readnext){//npc right, get next 64 inst bits, flush pipeline is not needed 
      //   //ignore bp result, use pc+4 instead
      //   pcOut := specialPCR
      //   pnpcOut := specialPCR + 4.U
      //   // pnpcOut := Mux(rvcFinish, io.in.bits.pnpc, Mux(isRVC, pcOut+2.U, pcOut+4.U))
      //   canGo := io.in.valid
      //   canIn := false.B
      //   when(io.out.fire()){
      //     state := s_extra
      //     pcOffsetR := "b010".U
      //   }
      // }
    }
  }.otherwise{
    state := s_idle
    canGo := DontCare
    canIn := DontCare
    pcOut := DontCare
    pnpcOut := DontCare
  }

  //output signals
  io.out.bits := DontCare
  io.out.bits.redirect.valid := false.B
  io.out.bits.pc := pcOut
  io.out.bits.pnpc := pnpcOut
  io.out.bits.instr := instr
  io.out.bits.brIdx := io.in.bits.brIdx

  io.out.valid := io.in.valid && canGo
  io.in.ready := (!io.in.valid || (io.out.fire() && canIn) || loadNextInstline)

  io.out.bits.exceptionVec := io.in.bits.exceptionVec/*.map(_ := false.B)*/ //Fix by zhangzifei from false.B
  io.out.bits.exceptionVec(instrPageFault) := io.in.bits.exceptionVec(instrPageFault) || specialIPFR && (state === s_waitnext_thenj || state === s_waitnext)
  io.out.bits.crossPageIPFFix := io.in.bits.exceptionVec(instrPageFault) && (state === s_waitnext_thenj || state === s_waitnext) && !specialIPFR
}
