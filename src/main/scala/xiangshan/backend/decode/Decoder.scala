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

  val instr: UInt = io.in.instr
  val decodeList = ListLookup(instr, Instructions.DecodeDefault, Instructions.DecodeTable)
  val instrType :: fuType :: fuOpType :: Nil = decodeList

  // todo: remove this when fetch stage can decide if an instr is br/jmp
  io.out.cf.isBr := (instrType === InstrB ||
                    (fuOpType === JumpOpType.jal && instrType === InstrJ && fuType === FuType.jmp) ||
                    (fuOpType === JumpOpType.jalr && instrType === InstrI && fuType === FuType.jmp) ||
                    (fuOpType === CSROpType.jmp && instrType === InstrI && fuType === FuType.csr))
//  val isRVC = instr(1, 0) =/= "b11".U
//  val rvcImmType :: rvcSrc1Type :: rvcSrc2Type :: rvcDestType :: Nil =
//    ListLookup(instr, CInstructions.DecodeDefault, CInstructions.CExtraDecodeTable)

  io.out.ctrl.fuOpType := fuOpType
  io.out.ctrl.fuType := fuType

  val SrcTypeTable = List(
    InstrI  ->   (SrcType.reg, SrcType.imm),
    InstrFI ->   (SrcType.reg, SrcType.imm),
    InstrR  ->   (SrcType.reg, SrcType.reg),
    InstrS  ->   (SrcType.reg, SrcType.reg),
    InstrFS ->   (SrcType.reg, SrcType.fp ),
    InstrSA ->   (SrcType.reg, SrcType.reg),
    InstrB  ->   (SrcType.reg, SrcType.reg),
    InstrU  ->   (SrcType.pc , SrcType.imm),
    InstrJ  ->   (SrcType.pc , SrcType.imm),
    InstrN  ->   (SrcType.pc , SrcType.imm)
  )
  val src1Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._1)))
  val src2Type = LookupTree(instrType, SrcTypeTable.map(p => (p._1, p._2._2)))

  val (rs, rt, rd) = (instr(19, 15), instr(24, 20), instr(11, 7))

  // TODO: refactor decode logic
  // make non-register addressing to zero, since isu.sb.isBusy(0) === false.B
  val rfWen = isrfWen(instrType)
  val fpWen = isfpWen(instrType)
  io.out.ctrl.lsrc1 := Mux(src1Type === SrcType.pc, 0.U, rs)
  io.out.ctrl.lsrc2 := Mux(src2Type === SrcType.imm, 0.U, rt)
  io.out.ctrl.rfWen := rfWen
  io.out.ctrl.fpWen := fpWen
  io.out.ctrl.ldest := Mux(fpWen || rfWen, rd, 0.U)

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
  io.out.ctrl.imm := imm

 when (fuType === FuType.jmp) {
   def isLink(reg: UInt) = (reg === 1.U || reg === 5.U)
   when (isLink(rd) && fuOpType === JumpOpType.jal) { io.out.ctrl.fuOpType := JumpOpType.call }
   when (fuOpType === JumpOpType.jalr) {
     when (isLink(rs)) { io.out.ctrl.fuOpType := JumpOpType.ret }
     when (isLink(rt)) { io.out.ctrl.fuOpType := JumpOpType.call }
   }
 }
  // fix LUI
  io.out.ctrl.src1Type := Mux(instr(6,0) === "b0110111".U, SrcType.reg, src1Type)
  io.out.ctrl.src2Type := src2Type

  // val vmEnable = WireInit(false.B)
  // BoringUtils.addSink(vmEnable, "DTLBENABLE")

  io.out.cf.exceptionVec.map(_ := false.B)
  io.out.cf.exceptionVec(illegalInstr) := instrType === InstrN
  io.out.cf.exceptionVec(instrPageFault) := io.in.exceptionVec(instrPageFault)
  // io.out.cf.exceptionVec(instrAccessFault) := io.in.pc(VAddrBits - 1, PAddrBits).orR && !vmEnable // NOTE: PAddrBits is larger than VAddrBits, so comment it

  io.out.ctrl.isXSTrap := (instr === XSTrap.TRAP)
  when(io.out.ctrl.isXSTrap){
    io.out.ctrl.lsrc1 := 10.U // a0
  }
  io.out.ctrl.noSpecExec := io.out.ctrl.isXSTrap || io.out.ctrl.fuType===FuType.csr
}
