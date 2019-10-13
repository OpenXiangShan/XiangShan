package noop

import chisel3._
import chisel3.util._

import utils._

class IDU extends NOOPModule with HasInstrType {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new IRIDCtrlFlowIO))
    val out = Decoupled(new DecodeIO)
    val flush = Input(Bool())
    val redirect = new RedirectIO
  })

  val instr = Wire(UInt(32.W))
  val instrType :: fuType :: fuOpType :: Nil =
    ListLookup(instr, Instructions.DecodeDefault, Instructions.DecodeTable)
  val isRVC = instr(1,0) =/= "b11".U
  val rvcImmType :: rvcSrc1Type :: rvcSrc2Type :: rvcDestType :: Nil =
    ListLookup(instr, CInstructions.DecodeDefault, CInstructions.CExtraDecodeTable) 


  io.out.bits := DontCare

  io.out.bits.ctrl.fuType := fuType
  io.out.bits.ctrl.fuOpType := fuOpType

  val SrcTypeTable = List(
    InstrI -> (SrcType.reg, SrcType.imm),
    InstrR -> (SrcType.reg, SrcType.reg),
    InstrS -> (SrcType.reg, SrcType.reg),
    InstrB -> (SrcType.reg, SrcType.reg),
    InstrU -> (SrcType.pc , SrcType.imm),
    InstrJ -> (SrcType.pc , SrcType.imm),
    InstrN -> (SrcType.pc , SrcType.imm)
  )
  val src1Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._1)))
  val src2Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._2)))

  val (rs, rt, rd) = (instr(19, 15), instr(24, 20), instr(11, 7))
  // see riscv-spec vol1, Table 16.1: Compressed 16-bit RVC instruction formats.
  val rs1       = instr(11,7)
  val rs2       = instr(6,2)
  val rs1p      = LookupTree(instr(9,7), RVCInstr.RVCRegNumTable.map(p => (p._1, p._2)))
  val rs2p      = LookupTree(instr(4,2), RVCInstr.RVCRegNumTable.map(p => (p._1, p._2)))
  val rvc_shamt = Cat(instr(12),instr(6,2)) 
  // val rdp_rs1p = LookupTree(instr(9,7), RVCRegNumTable)
  // val rdp      = LookupTree(instr(4,2), RVCRegNumTable)

  val RegLookUpTable = List(
    RVCInstr.DtCare   -> 0.U,
    RVCInstr.REGrs    -> rs,
    RVCInstr.REGrt    -> rt,
    RVCInstr.REGrd    -> rd,
    RVCInstr.REGrs1   -> rs1,
    RVCInstr.REGrs2   -> rs2,
    RVCInstr.REGrs1p  -> rs1p,
    RVCInstr.REGrs2p  -> rs2p,
    RVCInstr.REGx1    -> 1.U,
    RVCInstr.REGx2    -> 2.U
  )

  val rvc_src1 = LookupTree(rvcSrc1Type, RegLookUpTable.map(p => (p._1, p._2)))
  val rvc_src2 = LookupTree(rvcSrc2Type, RegLookUpTable.map(p => (p._1, p._2)))
  val rvc_dest =  LookupTree(rvcDestType, RegLookUpTable.map(p => (p._1, p._2)))

  val rfSrc1 = Mux(isRVC, rvc_src1, rs)
  val rfSrc2 = Mux(isRVC, rvc_src2, rt)
  val rfDest = Mux(isRVC, rvc_dest, rd)
  // TODO: refactor decode logic
  // make non-register addressing to zero, since isu.sb.isBusy(0) === false.B
  io.out.bits.ctrl.rfSrc1 := Mux(src1Type === SrcType.pc, 0.U, rfSrc1)
  io.out.bits.ctrl.rfSrc2 := Mux(src2Type === SrcType.reg, rfSrc2, 0.U)
  io.out.bits.ctrl.rfWen  := isrfWen(instrType)
  io.out.bits.ctrl.rfDest := Mux(isrfWen(instrType), rfDest, 0.U)

  io.out.bits.data := DontCare
  val imm = LookupTree(instrType, List(
    InstrI  -> SignExt(instr(31, 20), XLEN),
    InstrS  -> SignExt(Cat(instr(31, 25), instr(11, 7)), XLEN),
    InstrB  -> SignExt(Cat(instr(31), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W)), XLEN),
    InstrU  -> SignExt(Cat(instr(31, 12), 0.U(12.W)), XLEN),//fixed
    InstrJ  -> SignExt(Cat(instr(31), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W)), XLEN)
  ))
  val immrvc = LookupTree(rvcImmType, List(
    // InstrIW -> Cat(Fill(20+32, instr(31)), instr(31, 20)),//fixed
    RVCInstr.ImmNone  -> 0.U(XLEN.W),
    RVCInstr.ImmLWSP  -> ZeroExt(Cat(instr(3,2), instr(12), instr(6,4), 0.U(2.W)), XLEN),
    RVCInstr.ImmLDSP  -> ZeroExt(Cat(instr(4,2), instr(12), instr(6,5), 0.U(3.W)), XLEN),
    RVCInstr.ImmSWSP  -> ZeroExt(Cat(instr(8,7), instr(12,9), 0.U(2.W)), XLEN),
    RVCInstr.ImmSDSP  -> ZeroExt(Cat(instr(9,7), instr(12,10), 0.U(3.W)), XLEN),
    RVCInstr.ImmSW    -> ZeroExt(Cat(instr(5), instr(12,10), instr(6), 0.U(2.W)), XLEN),
    RVCInstr.ImmSD    -> ZeroExt(Cat(instr(6,5), instr(12,10), 0.U(3.W)), XLEN),
    RVCInstr.ImmLW    -> ZeroExt(Cat(instr(5), instr(12,10), instr(6), 0.U(2.W)), XLEN),
    RVCInstr.ImmLD    -> ZeroExt(Cat(instr(6,5), instr(12,10), 0.U(3.W)), XLEN),
    RVCInstr.ImmJ     -> SignExt(Cat(instr(12), instr(8), instr(10,9), instr(6), instr(7), instr(2), instr(11), instr(5,3), 0.U(1.W)), XLEN),
    RVCInstr.ImmB     -> SignExt(Cat(instr(12), instr(6,5), instr(2), instr(11,10), instr(4,3), 0.U(1.W)), XLEN),
    RVCInstr.ImmLI    -> SignExt(Cat(instr(12), instr(6,2)), XLEN),
    RVCInstr.ImmLUI   -> SignExt(Cat(instr(12), instr(6,2), 0.U(12.W)), XLEN),
    RVCInstr.ImmADDI  -> SignExt(Cat(instr(12), instr(6,2)), XLEN),
    RVCInstr.ImmADDI16SP-> SignExt(Cat(instr(12), instr(4,3), instr(5), instr(2), instr(6), 0.U(4.W)), XLEN),
    RVCInstr.ImmADD4SPN-> ZeroExt(Cat(instr(10,7), instr(12,11), instr(5), instr(6), 0.U(2.W)), XLEN)
    // ImmFLWSP  -> 
    // ImmFLDSP  -> 
  ))
  io.out.bits.data.imm  := Mux(isRVC, immrvc, imm)

  when (fuType === FuType.alu) {
    when (rfDest === 1.U && fuOpType === ALUOpType.jal) { io.out.bits.ctrl.fuOpType := ALUOpType.call }
    when (rfSrc1 === 1.U && fuOpType === ALUOpType.jalr) { io.out.bits.ctrl.fuOpType := ALUOpType.ret }
  }
  // fix LUI
  io.out.bits.ctrl.src1Type := Mux(instr(6,0) === "b0110111".U, SrcType.reg, src1Type)
  io.out.bits.ctrl.src2Type := src2Type

  io.out.bits.ctrl.isInvOpcode := (instrType === InstrN) && io.in.valid
  io.out.bits.ctrl.isNoopTrap := (instr(31,0) === NOOPTrap.TRAP) && io.in.valid

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
  val pnpcIsSeq = io.in.bits.pnpc === (Cat(io.in.bits.pc(AddrBits-1,2), 0.U(2.W)) + 4.U) // TODO: add a new user bit bpRight to do this 
  val flushIFU = (state === s_idle || state === s_extra) && rvcSpecial && io.in.valid && !pnpcIsSeq
  val loadNextInstline = (state === s_idle || state === s_extra) && (rvcSpecial || rvcSpecialJump) && io.in.valid && pnpcIsSeq
  // val loadNextInstline =false.B
  val pcOut = WireInit(0.U(AddrBits.W))
  val pnpcOut = WireInit(0.U(AddrBits.W))
  val specialPCR = Reg(UInt(AddrBits.W)) // reg for full inst that cross 2 inst line
  val specialNPCR = Reg(UInt(AddrBits.W)) // reg for pnc for full inst jump that cross 2 inst line
  val specialInstR = Reg(UInt(16.W))
  val redirectPC = Cat(io.in.bits.pc(31,3), 0.U(3.W))+"b1010".U // IDU can got get full inst from a single inst line
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
  io.redirect.brIdx := DontCare

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
        }
        when(rvcSpecialJump && io.in.valid){
          state := s_waitnext_thenj
          specialPCR := pcOut
          specialNPCR := io.in.bits.pnpc
          specialInstR := io.in.bits.instr(63,63-16+1) 
        }
      }
      is(s_extra){//get 16 aligned inst, pc controled by this FSM
        canGo := rvcFinish || rvcNext
        canIn := rvcFinish || rvcForceLoadNext
        pcOut := Cat(io.in.bits.pc(31,3), pcOffsetR(2,0))
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
  io.out.bits.cf.pc := pcOut
  io.out.bits.cf.pnpc := pnpcOut
  io.out.bits.cf.instr := instr

  io.out.valid := io.in.valid && canGo
  io.in.ready := !io.in.valid || (io.out.fire() && canIn) || loadNextInstline

  // NOTE:
  // we did not do special opt for cross-line jump inst, hopefully there will not be too much such inst 
  // for perf counter:
  // val crossLineJump = state === s_waitnext && fuType === FuType.alu && fuOpType.isBru()

  Debug(){
    // when(io.out.fire()){
      printf("[IDU] pc %x pcin: %x instr %x instrin %x state %x instrType: %x fuType: %x fuOpType: %x brIdx: %x npcin: %x npcout: %x valid: %x\n", pcOut, io.in.bits.pc, instr, io.in.bits.instr, state, instrType, fuType, fuOpType, brIdx, io.in.bits.pnpc, pnpcOut, io.out.fire())
    // }
  }
}

// Note  
// C.LWSP is only valid when rd̸=x0; the code points with rd=x0 are reserved
// C.LDSP is only valid when rd̸=x0; the code points with rd=x0 are reserved.
