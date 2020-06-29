package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.FuType._
import xiangshan.utils._
import xiangshan.backend.regfile.RfWritePort

object ALUOpType {
  def add  = "b000000".U
  def sll  = "b000001".U
  def slt  = "b000010".U
  def sltu = "b000011".U
  def xor  = "b000100".U
  def srl  = "b000101".U
  def or   = "b000110".U
  def and  = "b000111".U
  def sub  = "b001000".U
  def sra  = "b001101".U

  def addw = "b100000".U
  def subw = "b101000".U
  def sllw = "b100001".U
  def srlw = "b100101".U
  def sraw = "b101101".U

  def isWordOp(func: UInt) = func(5)

  // TODO: move jal/jalr/call/ret from ALU to BRU&CSR
  def jal  = "b011000".U
  def jalr = "b011010".U
  // def cjalr= "b111010".U // pc + 2 instead of 4
  def beq  = "b010000".U
  def bne  = "b010001".U
  def blt  = "b010100".U
  def bge  = "b010101".U
  def bltu = "b010110".U
  def bgeu = "b010111".U

  // for RAS
  def call = "b011100".U
  def ret  = "b011110".U

  // def pcPlus2(func: UInt) = func(5)//[important]
  def isBranch(func: UInt) = func(4,3)===2.U
  def isBru(func: UInt) = func(4)
  def isJump(func: UInt) = func(4,3)===3.U//isBru(func) && !isBranch(func)
  def getBranchType(func: UInt) = func(2, 1)
  def isBranchInvert(func: UInt) = func(0)
}

class Alu extends Exu(alu.litValue()) {
  override def toString: String = "Alu"

  val (iovalid, src1, src2, offset, func, pc, uop) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, 
    io.in.bits.uop.ctrl.imm, io.in.bits.uop.ctrl.fuOpType, SignExt(io.in.bits.uop.cf.pc, AddrBits), io.in.bits.uop)

  val redirectHit = (io.redirect.valid && 
    ((UIntToOH(io.redirect.bits.brTag) & uop.brMask).orR || io.redirect.bits.isException))
  val valid = iovalid && !redirectHit

  val isAdderSub = (func =/= ALUOpType.add) && (func =/= ALUOpType.addw) && !ALUOpType.isJump(func)
  val adderRes = (src1 +& (src2 ^ Fill(XLEN, isAdderSub))) + isAdderSub
  val xorRes = src1 ^ src2
  val sltu = !adderRes(XLEN)
  val slt = xorRes(XLEN-1) ^ sltu

  val shsrc1 = LookupTreeDefault(func, src1, List(
    ALUOpType.srlw -> ZeroExt(src1(31,0), 64),
    ALUOpType.sraw -> SignExt(src1(31,0), 64)
  ))
  val shamt = Mux(ALUOpType.isWordOp(func), src2(4, 0), src2(5, 0))
  val res = LookupTreeDefault(func(3, 0), adderRes, List(
    ALUOpType.sll  -> ((shsrc1  << shamt)(XLEN-1, 0)),
    ALUOpType.slt  -> ZeroExt(slt, XLEN),
    ALUOpType.sltu -> ZeroExt(sltu, XLEN),
    ALUOpType.xor  -> xorRes,
    ALUOpType.srl  -> (shsrc1  >> shamt),
    ALUOpType.or   -> (src1  |  src2),
    ALUOpType.and  -> (src1  &  src2),
    ALUOpType.sra  -> ((shsrc1.asSInt >> shamt).asUInt)
  ))
  val aluRes = Mux(ALUOpType.isWordOp(func), SignExt(res(31,0), 64), res)

  val branchOpTable = List(
    ALUOpType.getBranchType(ALUOpType.beq)  -> !xorRes.orR,
    ALUOpType.getBranchType(ALUOpType.blt)  -> slt,
    ALUOpType.getBranchType(ALUOpType.bltu) -> sltu
  )

  val isBru = ALUOpType.isBru(func)
  // val isBranch =  io.in.bits.uop.cf.isBr// ALUOpType.isBranch(func)
  val isBranch =  ALUOpType.isBranch(func)
  val isJump = ALUOpType.isJump(func)
  val taken = LookupTree(ALUOpType.getBranchType(func), branchOpTable) ^ ALUOpType.isBranchInvert(func)
  val target = Mux(isBranch, pc + offset, adderRes)(VAddrBits-1,0)
  val isRVC = uop.cf.isRVC//(io.in.bits.cf.instr(1,0) =/= "b11".U)

  io.in.ready := io.out.ready
  val pcLatchSlot = Mux(isRVC, pc + 2.U, pc + 4.U)
  io.out.bits.redirect.valid := io.out.valid && isBru//isBranch
  io.out.bits.redirect.bits.target := Mux(!taken && isBranch, pcLatchSlot, target)
  io.out.bits.redirect.bits.brTag := uop.brTag
  io.out.bits.redirect.bits.isException := DontCare // false.B
  io.out.bits.redirect.bits.roqIdx := uop.roqIdx
  io.out.bits.redirect.bits.freelistAllocPtr := uop.freelistAllocPtr

  io.out.valid := valid
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := Mux(isJump, pcLatchSlot, aluRes)

  io.dmem <> DontCare
  io.out.bits.debug.isMMIO := DontCare // FIXME: dont know how to do with it
  
  XSDebug(io.in.valid, "In(%d %d) Out(%d %d) Redirect:(%d %d %d) brTag:%x, brMask:%x\n",
    io.in.valid, io.in.ready, io.out.valid, io.out.ready, io.redirect.valid, io.redirect.bits.isException, redirectHit, io.redirect.bits.brTag, uop.brMask)
  XSDebug(io.in.valid, "src1:%x src2:%x offset:%x func:%b pc:%x\n",
    src1, src2, offset, func, pc)
  XSDebug(io.in.valid, "res:%x aluRes:%x isRVC:%d isBru:%d isBranch:%d isJump:%d target:%x taken:%d\n",
     io.out.bits.data, aluRes, isRVC, isBru, isBranch, isJump, target, taken)
}