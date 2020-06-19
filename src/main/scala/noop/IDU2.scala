package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import noop.isa.{RVDInstr, RVFInstr, RVF_LSUInstr, RVD_LSUInstr}
import utils._

class IDU2(implicit val p: NOOPConfig) extends NOOPModule with HasInstrType {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CtrlFlowIO))
    val out = Decoupled(new DecodeIO)
    val flush = Input(Bool())
  })

  val hasIntr = Wire(Bool())
  val hasIntrOrExceptino = hasIntr || io.in.bits.exceptionVec(instrPageFault)
  val instr = io.in.bits.instr(31, 0)
  val decodeList = ListLookup(instr, Instructions.DecodeDefault, Instructions.DecodeTable)
  val commonInstrType :: commonFuType :: commonFuOpType :: Nil = decodeList

  val intrInstrType :: intrFuType :: intrFuOpType :: Nil = Instructions.DecodeDefault

  //(isFp, src1Type, src2Type, src3Type, rfWen, fpWen, fuOpType, inputFunc, outputFunc)
  val fpExtraDecodeTable = RVFInstr.extraTable ++ RVDInstr.extraTable
  val isFp :: fpSrc1Type :: fpSrc2Type :: fpSrc3Type :: fpRfWen :: fpWen :: fpFuOpType :: fpInputFunc :: fpOutputFunc :: Nil =
    if(HasFPU) ListLookup(instr, RVFInstr.extraTableDefault, fpExtraDecodeTable) else RVFInstr.extraTableDefault

  val floatLdStInstrs = List(
    RVF_LSUInstr.FLW,
    RVF_LSUInstr.FSW,
    RVD_LSUInstr.FLD,
    RVCInstr.C_FLD,
    RVCInstr.C_FLDSP,
    RVD_LSUInstr.FSD,
    RVCInstr.C_FSD,
    RVCInstr.C_FSDSP
  )

  def treeCmp(key: UInt, cmpList: List[BitPat]): Bool = {
    cmpList.size match {
      case 1 =>
        key === cmpList.head
      case n =>
        treeCmp(key, cmpList take n/2) || treeCmp(key, cmpList drop n/2)
    }
  }

  val isFloatLdSd = if(HasFPU) treeCmp(instr, floatLdStInstrs) else false.B

  val isRVFD = isFp.asBool()
  val instrType = Mux(hasIntrOrExceptino,
    intrInstrType,
    commonInstrType
  )
  val fuType = Mux(hasIntrOrExceptino,
    intrFuType,
    Mux(isRVFD && !isFloatLdSd,
      FuType.fpu,
      commonFuType
    )
  )
  val fuOpType = Mux(hasIntrOrExceptino,
    intrFuOpType,
    Mux(isRVFD, fpFuOpType, commonFuOpType)
  )


  val isRVC = instr(1,0) =/= "b11".U
  val rvcImmType :: rvcSrc1Type :: rvcSrc2Type :: rvcDestType :: Nil =
    ListLookup(instr, CInstructions.DecodeDefault, CInstructions.CExtraDecodeTable) 

  io.out.bits := DontCare

  io.out.bits.ctrl.fuType := fuType
  io.out.bits.ctrl.fuOpType := fuOpType
  io.out.bits.ctrl.fpInputFunc := fpInputFunc
  io.out.bits.ctrl.fpOutputFunc := fpOutputFunc

  val SrcTypeTable = List(
    InstrI -> (SrcType.reg, SrcType.imm),
    InstrR -> (SrcType.reg, SrcType.reg),
    InstrS -> (SrcType.reg, SrcType.reg),
    InstrSA-> (SrcType.reg, SrcType.reg),
    InstrB -> (SrcType.reg, SrcType.reg),
    InstrU -> (SrcType.pc , SrcType.imm),
    InstrJ -> (SrcType.pc , SrcType.imm),
    InstrN -> (SrcType.pc , SrcType.imm)
  )
  val src1Type = Mux(isRVFD,
    fpSrc1Type,
    LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._1)))
  )
  val src2Type = Mux(isRVFD,
    fpSrc2Type,
    LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._2)))
  )

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

  val rfWen = !hasIntrOrExceptino && Mux(isRVFD, fpRfWen.asBool(), isrfWen(instrType))

  // TODO: refactor decode logic
  // make non-register addressing to zero, since isu.sb.isBusy(0) === false.B
  io.out.bits.ctrl.rfSrc1 := Mux(src1Type === SrcType.pc, 0.U, rfSrc1)
  io.out.bits.ctrl.rfSrc2 := Mux(src2Type === SrcType.imm, 0.U, rfSrc2)
  io.out.bits.ctrl.rfWen  := rfWen
  io.out.bits.ctrl.fpWen := fpWen.asBool()
  io.out.bits.ctrl.rfDest := Mux(fpWen.asBool() || rfWen, rfDest, 0.U)

  io.out.bits.data := DontCare
  val imm = LookupTree(instrType, List(
    InstrI  -> SignExt(instr(31, 20), XLEN),
    InstrS  -> SignExt(Cat(instr(31, 25), instr(11, 7)), XLEN),
    InstrSA -> SignExt(Cat(instr(31, 25), instr(11, 7)), XLEN),
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
    def isLink(reg: UInt) = (reg === 1.U || reg === 5.U)
    when (isLink(rfDest) && fuOpType === ALUOpType.jal) { io.out.bits.ctrl.fuOpType := ALUOpType.call }
    when (fuOpType === ALUOpType.jalr) {
      when (isLink(rfSrc1)) { io.out.bits.ctrl.fuOpType := ALUOpType.ret }
      when (isLink(rfDest)) { io.out.bits.ctrl.fuOpType := ALUOpType.call }
    }
  }
  // fix LUI
  io.out.bits.ctrl.src1Type := Mux(instr(6,0) === "b0110111".U, SrcType.reg, src1Type)
  io.out.bits.ctrl.src2Type := src2Type
  io.out.bits.ctrl.src3Type := fpSrc3Type

  // io.out.bits.ctrl.isInvOpcode := (instrType === InstrN) && io.in.valid
  io.out.bits.ctrl.isNoopTrap := (instr(31,0) === NOOPTrap.TRAP) && io.in.valid

  //output signals

  io.out.valid := io.in.valid
  io.in.ready := !io.in.valid || io.out.fire() && !hasIntr
  io.out.bits.cf <> io.in.bits

  Debug(){
    when(io.out.fire()){printf("[IDU] issue: pc %x npc %x instr %x\n", io.out.bits.cf.pc, io.out.bits.cf.pnpc, io.out.bits.cf.instr)}
  }

  val intrVec = WireInit(0.U(12.W))
  BoringUtils.addSink(intrVec, "intrVecIDU")
  io.out.bits.cf.intrVec.zip(intrVec.asBools).map{ case(x, y) => x := y }
  hasIntr := intrVec.orR

  io.out.bits.cf.exceptionVec.map(_ := false.B)
  io.out.bits.cf.exceptionVec(illegalInstr) := (!isRVFD && instrType === InstrN && !hasIntr) && io.in.valid
  io.out.bits.cf.exceptionVec(instrPageFault) := io.in.bits.exceptionVec(instrPageFault)

  io.out.bits.ctrl.isNoopTrap := (instr === NOOPTrap.TRAP) && io.in.valid

  if (!p.FPGAPlatform) {
    val isWFI = (instr === Priviledged.WFI) && io.in.valid
    BoringUtils.addSource(isWFI, "isWFI")
  }
}

// Note  
// C.LWSP is only valid when rd̸=x0; the code points with rd=x0 are reserved
// C.LDSP is only valid when rd̸=x0; the code points with rd=x0 are reserved.
