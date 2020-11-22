package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend._
import xiangshan.backend.decode.isa.{RVCInstr, RV32I_ALUInstr, RVFInstr, RVDInstr}
import xiangshan.{CfCtrl, CtrlFlow}


class Decoder extends XSModule with HasInstrType {
  val io = IO(new Bundle() {
    val in = Input(new CtrlFlow)
    val out = Output(new CfCtrl)
  })

  io.out := DontCare // FIXME: remove me!!!
  io.out.cf := io.in

  val instr: UInt = io.in.instr
  val decodeList = ListLookup(instr, Instructions.DecodeDefault, Instructions.DecodeTable)
  val instrType :: fuType :: fuOpType :: Nil = decodeList

  // todo: remove this when fetch stage can decide if an instr is br/jmp
  // io.out.cf.brUpdate.isBr := (instrType === InstrB ||
  //                   (fuOpType === JumpOpType.jal && instrType === InstrJ && fuType === FuType.jmp) ||
  //                   (fuOpType === JumpOpType.jalr && instrType === InstrI && fuType === FuType.jmp) ||
  //                   (fuOpType === CSROpType.jmp && instrType === InstrI && fuType === FuType.csr))
  val isRVC = instr(1, 0) =/= "b11".U
  val rvcImmType :: rvcSrc1Type :: rvcSrc2Type :: rvcDestType :: Nil =
    ListLookup(instr, CInstructions.DecodeDefault, CInstructions.CExtraDecodeTable)

  io.out.ctrl.fuOpType := fuOpType
  io.out.ctrl.fuType := fuType

  val SrcTypeTable = List(
    InstrI    -> (SrcType.reg, SrcType.imm),
    InstrFI   -> (SrcType.reg, SrcType.imm),
    InstrR    -> (SrcType.reg, SrcType.reg),
    InstrFR   -> (SrcType.fp,  SrcType.fp ),
    InstrS    -> (SrcType.reg, SrcType.reg),
    InstrFS   -> (SrcType.reg, SrcType.fp ),
    InstrSA   -> (SrcType.reg, SrcType.reg),
    InstrB    -> (SrcType.reg, SrcType.reg),
    InstrU    -> (SrcType.pc , SrcType.imm),
    InstrJ    -> (SrcType.pc , SrcType.imm),
    InstrN    -> (SrcType.pc , SrcType.imm),
    InstrGtoF -> (SrcType.reg, SrcType.imm),
    InstrFtoG -> (SrcType.fp , SrcType.fp)
  )
  val src1Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._1)))
  val src2Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._2)))

  val (rs, rt, rd) = (instr(19, 15), instr(24, 20), instr(11, 7))

  val rs1       = instr(11,7)
  val rs2       = instr(6,2)
  val rs1p      = LookupTree(instr(9,7), RVCInstr.RVCRegNumTable.map(p => (p._1, p._2)))
  val rs2p      = LookupTree(instr(4,2), RVCInstr.RVCRegNumTable.map(p => (p._1, p._2)))
  val rvc_shamt = Cat(instr(12),instr(6,2))

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
  val rvc_dest = LookupTree(rvcDestType, RegLookUpTable.map(p => (p._1, p._2)))

  val rfSrc1 = Mux(isRVC, rvc_src1, rs)
  val rfSrc2 = Mux(isRVC, rvc_src2, rt)
  val rfSrc3 = instr(31, 27)
  val rfDest = Mux(isRVC, rvc_dest, rd)

  // TODO: refactor decode logic
  // make non-register addressing to zero, since isu.sb.isBusy(0) === false.B
  val rfWen = isrfWen(instrType) && fuType=/=FuType.fence // NOTE: fence instr use instrU but do not wb
  val fpWen = isfpWen(instrType)
  io.out.ctrl.lsrc1 := Mux(src1Type === SrcType.pc, 0.U, rfSrc1)
  io.out.ctrl.lsrc2 := Mux(src2Type === SrcType.imm, 0.U, rfSrc2)
  io.out.ctrl.lsrc3 := rfSrc3
  io.out.ctrl.rfWen := rfWen
  io.out.ctrl.fpWen := fpWen
  io.out.ctrl.ldest := Mux(fpWen || rfWen, rfDest, 0.U)

  val imm = LookupTree(instrType, List(
    InstrI  -> SignExt(instr(31, 20), XLEN),
    InstrFI -> SignExt(instr(31, 20), XLEN),
    InstrS  -> SignExt(Cat(instr(31, 25), instr(11, 7)), XLEN),
    InstrFS -> SignExt(Cat(instr(31, 25), instr(11, 7)), XLEN),
    InstrSA -> SignExt(Cat(instr(31, 25), instr(11, 7)), XLEN),
    InstrB  -> SignExt(Cat(instr(31), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W)), XLEN),
    InstrU  -> SignExt(Cat(instr(31, 12), 0.U(12.W)), XLEN),//fixed
    InstrJ  -> SignExt(Cat(instr(31), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W)), XLEN)
  ))
  val immrvc = LookupTree(rvcImmType, List(
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
  ))
  io.out.ctrl.imm := Mux(isRVC, immrvc, imm)

 when (fuType === FuType.jmp) {
   def isLink(reg: UInt) = (reg === 1.U || reg === 5.U)
   when (isLink(rfDest) && fuOpType === JumpOpType.jal) { io.out.ctrl.fuOpType := JumpOpType.call }
   when (fuOpType === JumpOpType.jalr) {
     when (isLink(rfSrc1)) { io.out.ctrl.fuOpType := JumpOpType.ret }
     when (isLink(rfDest)) { io.out.ctrl.fuOpType := JumpOpType.call }
   }
 }




  def bitPatLookup(key: UInt, default: UInt, mapping: Seq[(BitPat, UInt)]) = {
    mapping.foldLeft(default){case (d, (k, v)) => Mux(k === key, v, d)}
  }

  io.out.ctrl.src1Type := bitPatLookup(instr, src1Type, Seq(
    RV32I_ALUInstr.LUI -> SrcType.reg // FIX LUI
  ))
  io.out.ctrl.src2Type := bitPatLookup(instr, src2Type, Seq(
    RVFInstr.FSQRT_S -> SrcType.imm,
    RVFInstr.FCLASS_S -> SrcType.imm,
    RVFInstr.FMV_X_W -> SrcType.imm,
    RVFInstr.FCVT_W_S -> SrcType.imm,
    RVFInstr.FCVT_WU_S -> SrcType.imm,
    RVFInstr.FCVT_L_S -> SrcType.imm,
    RVFInstr.FCVT_LU_S -> SrcType.imm,

    RVDInstr.FSQRT_D -> SrcType.imm,
    RVDInstr.FCVT_S_D -> SrcType.imm,
    RVDInstr.FCVT_D_S -> SrcType.imm,
    RVDInstr.FCLASS_D -> SrcType.imm,
    RVDInstr.FMV_X_D -> SrcType.imm,
    RVDInstr.FCVT_W_D -> SrcType.imm,
    RVDInstr.FCVT_WU_D -> SrcType.imm,
    RVDInstr.FCVT_L_D -> SrcType.imm,
    RVDInstr.FCVT_LU_D -> SrcType.imm
  ))
  io.out.ctrl.src3Type := bitPatLookup(instr, SrcType.imm, Seq(
    RVFInstr.FMADD_S -> SrcType.fp,
    RVFInstr.FNMADD_S -> SrcType.fp,
    RVFInstr.FMSUB_S -> SrcType.fp,
    RVFInstr.FNMSUB_S -> SrcType.fp,

    RVDInstr.FMADD_D -> SrcType.fp,
    RVDInstr.FNMADD_D -> SrcType.fp,
    RVDInstr.FMSUB_D -> SrcType.fp,
    RVDInstr.FNMSUB_D -> SrcType.fp,
  ))

  io.out.cf.exceptionVec.map(_ := false.B)
  io.out.cf.exceptionVec(illegalInstr) := instrType === InstrN
  io.out.cf.exceptionVec(instrPageFault) := io.in.exceptionVec(instrPageFault)
  // io.out.cf.exceptionVec(instrAccessFault) := io.in.pc(VAddrBits - 1, PAddrBits).orR && !vmEnable // NOTE: PAddrBits is larger than VAddrBits, so comment it

  io.out.ctrl.isXSTrap := (instr === XSTrap.TRAP)
  when(io.out.ctrl.isXSTrap){
    io.out.ctrl.lsrc1 := 10.U // a0
  }

  /*noSpecExec make it sent to alu0,for roq is empty*/
  io.out.ctrl.noSpecExec := io.out.ctrl.isXSTrap ||
    io.out.ctrl.fuType===FuType.csr ||
    io.out.ctrl.fuType===FuType.mou ||
    io.out.ctrl.fuType===FuType.fence

  //                           fflags    zero csrrs rd    csr
  val isFrflags = BitPat("b000000000001_00000_010_?????_1110011") === io.in.instr

  io.out.ctrl.blockBackward := io.out.ctrl.isXSTrap ||
    (io.out.ctrl.fuType===FuType.csr && !isFrflags) ||
    io.out.ctrl.fuType===FuType.mou ||
    io.out.ctrl.fuType===FuType.fence

  io.out.ctrl.flushPipe := io.out.ctrl.fuType===FuType.fence

  io.out.ctrl.isRVF := instr(26, 25) === 0.U


  XSDebug("in:  instr=%x pc=%x excepVec=%b intrVec=%b crossPageIPFFix=%d\n",
    io.in.instr, io.in.pc, io.in.exceptionVec.asUInt, io.in.intrVec.asUInt, io.in.crossPageIPFFix)
  XSDebug("out: src1Type=%b src2Type=%b src3Type=%b lsrc1=%d lsrc2=%d lsrc3=%d ldest=%d fuType=%b fuOpType=%b\n",
    io.out.ctrl.src1Type, io.out.ctrl.src2Type, io.out.ctrl.src3Type, io.out.ctrl.lsrc1, io.out.ctrl.lsrc2, io.out.ctrl.lsrc3, io.out.ctrl.ldest, io.out.ctrl.fuType, io.out.ctrl.fuOpType)
  XSDebug("out: rfWen=%d fpWen=%d isXSTrap=%d noSpecExec=%d isBlocked=%d flushPipe=%d isRVF=%d imm=%x\n",
    io.out.ctrl.rfWen, io.out.ctrl.fpWen, io.out.ctrl.isXSTrap, io.out.ctrl.noSpecExec, io.out.ctrl.blockBackward, io.out.ctrl.flushPipe, io.out.ctrl.isRVF, io.out.ctrl.imm)
}
