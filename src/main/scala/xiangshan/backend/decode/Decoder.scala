package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xiangshan._
import utils.{LookupTree, SignExt, ZeroExt}
import xiangshan.backend._
import xiangshan.backend.decode.isa.RVCInstr
import xiangshan.{CfCtrl, CtrlFlow}


class Decoder extends XSModule with HasInstrType {
  val io = IO(new Bundle() {
    val in = Input(new CtrlFlow)
    val out = Output(new CfCtrl)
  })


  io.out := DontCare // FIXME: remove me!!!
  io.out.cf := io.in

  val hasIntr = Wire(Bool())
  val instr: UInt = io.in.instr
  val decodeList = ListLookup(instr, Instructions.DecodeDefault, Instructions.DecodeTable)
  val instrType :: fuType :: fuOpType :: Nil = decodeList

  // todo: remove this when fetch stage can decide if an instr is br/jmp
  io.out.cf.isBr := (instrType === InstrB ||
                    (fuOpType === BRUOpType.jal && instrType === InstrJ && fuType === FuType.bru) ||
                    (fuOpType === BRUOpType.jalr && instrType === InstrI && fuType === FuType.bru))

//  val isRVC = instr(1, 0) =/= "b11".U
//  val rvcImmType :: rvcSrc1Type :: rvcSrc2Type :: rvcDestType :: Nil =
//    ListLookup(instr, CInstructions.DecodeDefault, CInstructions.CExtraDecodeTable)

  io.out.ctrl.fuOpType := fuOpType
  io.out.ctrl.fuType := fuType

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
  val src1Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._1)))
  val src2Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._2)))

  val (rs, rt, rd) = (instr(19, 15), instr(24, 20), instr(11, 7))
  // see riscv-spec vol1, Table 16.1: Compressed 16-bit RVC instruction formats.
  val rs1       = instr(11,7)
  val rs2       = instr(6,2)
//  val rs1p      = LookupTree(instr(9,7), RVCInstr.RVCRegNumTable.map(p => (p._1, p._2)))
//  val rs2p      = LookupTree(instr(4,2), RVCInstr.RVCRegNumTable.map(p => (p._1, p._2)))
//  val rvc_shamt = Cat(instr(12),instr(6,2))
  // val rdp_rs1p = LookupTree(instr(9,7), RVCRegNumTable)
  // val rdp      = LookupTree(instr(4,2), RVCRegNumTable)

//  val RegLookUpTable = List(
//    RVCInstr.DtCare   -> 0.U,
//    RVCInstr.REGrs    -> rs,
//    RVCInstr.REGrt    -> rt,
//    RVCInstr.REGrd    -> rd,
//    RVCInstr.REGrs1   -> rs1,
//    RVCInstr.REGrs2   -> rs2,
//    RVCInstr.REGrs1p  -> rs1p,
//    RVCInstr.REGrs2p  -> rs2p,
//    RVCInstr.REGx1    -> 1.U,
//    RVCInstr.REGx2    -> 2.U
//  )

//  val rvc_src1 = LookupTree(rvcSrc1Type, RegLookUpTable.map(p => (p._1, p._2)))
//  val rvc_src2 = LookupTree(rvcSrc2Type, RegLookUpTable.map(p => (p._1, p._2)))
//  val rvc_dest =  LookupTree(rvcDestType, RegLookUpTable.map(p => (p._1, p._2)))

//  val rfSrc1 = Mux(isRVC, rvc_src1, rs)
//  val rfSrc2 = Mux(isRVC, rvc_src2, rt)
//  val rfDest = Mux(isRVC, rvc_dest, rd)
  // TODO: refactor decode logic
  // make non-register addressing to zero, since isu.sb.isBusy(0) === false.B
  io.out.ctrl.lsrc1 := Mux(src1Type === SrcType.pc, 0.U, rs)
  io.out.ctrl.lsrc2 := Mux(src2Type === SrcType.reg, rt, 0.U)
  io.out.ctrl.rfWen  := isrfWen(instrType)
  io.out.ctrl.ldest := Mux(isrfWen(instrType), rd, 0.U)

//  io.out.bits.data := DontCare
  val imm = LookupTree(instrType, List(
    InstrI  -> SignExt(instr(31, 20), XLEN),
    InstrS  -> SignExt(Cat(instr(31, 25), instr(11, 7)), XLEN),
    InstrSA -> SignExt(Cat(instr(31, 25), instr(11, 7)), XLEN),
    InstrB  -> SignExt(Cat(instr(31), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W)), XLEN),
    InstrU  -> SignExt(Cat(instr(31, 12), 0.U(12.W)), XLEN),//fixed
    InstrJ  -> SignExt(Cat(instr(31), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W)), XLEN)
  ))
//  val immrvc = LookupTree(rvcImmType, List(
//    // InstrIW -> Cat(Fill(20+32, instr(31)), instr(31, 20)),//fixed
//    RVCInstr.ImmNone  -> 0.U(XLEN.W),
//    RVCInstr.ImmLWSP  -> ZeroExt(Cat(instr(3,2), instr(12), instr(6,4), 0.U(2.W)), XLEN),
//    RVCInstr.ImmLDSP  -> ZeroExt(Cat(instr(4,2), instr(12), instr(6,5), 0.U(3.W)), XLEN),
//    RVCInstr.ImmSWSP  -> ZeroExt(Cat(instr(8,7), instr(12,9), 0.U(2.W)), XLEN),
//    RVCInstr.ImmSDSP  -> ZeroExt(Cat(instr(9,7), instr(12,10), 0.U(3.W)), XLEN),
//    RVCInstr.ImmSW    -> ZeroExt(Cat(instr(5), instr(12,10), instr(6), 0.U(2.W)), XLEN),
//    RVCInstr.ImmSD    -> ZeroExt(Cat(instr(6,5), instr(12,10), 0.U(3.W)), XLEN),
//    RVCInstr.ImmLW    -> ZeroExt(Cat(instr(5), instr(12,10), instr(6), 0.U(2.W)), XLEN),
//    RVCInstr.ImmLD    -> ZeroExt(Cat(instr(6,5), instr(12,10), 0.U(3.W)), XLEN),
//    RVCInstr.ImmJ     -> SignExt(Cat(instr(12), instr(8), instr(10,9), instr(6), instr(7), instr(2), instr(11), instr(5,3), 0.U(1.W)), XLEN),
//    RVCInstr.ImmB     -> SignExt(Cat(instr(12), instr(6,5), instr(2), instr(11,10), instr(4,3), 0.U(1.W)), XLEN),
//    RVCInstr.ImmLI    -> SignExt(Cat(instr(12), instr(6,2)), XLEN),
//    RVCInstr.ImmLUI   -> SignExt(Cat(instr(12), instr(6,2), 0.U(12.W)), XLEN),
//    RVCInstr.ImmADDI  -> SignExt(Cat(instr(12), instr(6,2)), XLEN),
//    RVCInstr.ImmADDI16SP-> SignExt(Cat(instr(12), instr(4,3), instr(5), instr(2), instr(6), 0.U(4.W)), XLEN),
//    RVCInstr.ImmADD4SPN-> ZeroExt(Cat(instr(10,7), instr(12,11), instr(5), instr(6), 0.U(2.W)), XLEN)
//    // ImmFLWSP  ->
//    // ImmFLDSP  ->
//  ))
//  io.out.ctrl.imm  := Mux(isRVC, immrvc, imm)
  io.out.ctrl.imm := imm

//  when (fuType === FuType.bru) {
//    def isLink(reg: UInt) = (reg === 1.U || reg === 5.U)
////    when (isLink(rd) && fuOpType === BRUOpType.jal) { io.out.ctrl.fuOpType := ALUOpType.call }
//    when (fuOpType === BRUOpType.jalr) {
////      when (isLink(rs)) { io.out.ctrl.fuOpType := ALUOpType.ret }
////      when (isLink(rt)) { io.out.ctrl.fuOpType := ALUOpType.call }
//    }
//  }
  // fix LUI
  io.out.ctrl.src1Type := Mux(instr(6,0) === "b0110111".U, SrcType.reg, src1Type)
  io.out.ctrl.src2Type := src2Type

  val NoSpecList = Seq(
//    FuType.csr,
//    FuType.mou
  )

  val BlockList = Seq(
  )

  io.out.ctrl.noSpecExec := NoSpecList.map(j => io.out.ctrl.fuType === j).foldRight(false.B)((sum, i) => sum | i)
  io.out.ctrl.isBlocked := DontCare
//    (
//      io.out.ctrl.fuType === (FuType.ldu | FuType.stu) && LSUOpType.isAtom(io.out.ctrl.fuOpType) ||
//        BlockList.map(j => io.out.ctrl.fuType === j).foldRight(false.B)((sum, i) => sum | i)
//      )

  //output signals

//  Debug(){
//    when(io.out.fire()){printf("[IDU] issue: pc %x npc %x instr %x\n", io.out.bits.cf.pc, io.out.bits.cf.pnpc, io.out.bits.cf.instr)}
//  }

  //FIXME: move it to ROB
  val intrVec = WireInit(0.U(12.W))
  BoringUtils.addSink(intrVec, "intrVecIDU")
  io.out.cf.intrVec.zip(intrVec.asBools).map{ case(x, y) => x := y }
  hasIntr := intrVec.orR

  val vmEnable = WireInit(false.B)
  BoringUtils.addSink(vmEnable, "DTLBENABLE")

  io.out.cf.exceptionVec.map(_ := false.B)
  io.out.cf.exceptionVec(illegalInstr) := (instrType === InstrN && !hasIntr)
  io.out.cf.exceptionVec(instrPageFault) := io.in.exceptionVec(instrPageFault)
  io.out.cf.exceptionVec(instrAccessFault) := io.in.pc(VAddrBits - 1, PAddrBits).orR && !vmEnable

  io.out.ctrl.isXSTrap := (instr === XSTrap.TRAP)
  when(io.out.ctrl.isXSTrap){
    io.out.ctrl.lsrc1 := 10.U // a0
  }
//  io.isWFI := (instr === Priviledged.WFI) && io.in.valid

}
