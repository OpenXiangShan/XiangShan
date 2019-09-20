package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

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

  def jal  = "b011000".U
  def jalr = "b011010".U
  def beq  = "b010000".U
  def bne  = "b010001".U
  def blt  = "b010100".U
  def bge  = "b010101".U
  def bltu = "b010110".U
  def bgeu = "b010111".U

  // for RAS
  def call = "b011100".U
  def ret  = "b011110".U

  def sllw = "b100000".U  
  def srlw  = "b100001".U
  def sraw  = "b100010".U
  def addw  = "b100011".U
  def subw  = "b100100".U

  def isBru(func: UInt) = func(4)//[important]
  def isBranch(func: UInt) = !func(3)
  def isJump(func: UInt) = isBru(func) && !isBranch(func)
  def getBranchType(func: UInt) = func(2, 1)
  def isBranchInvert(func: UInt) = func(0)
}

class ALUIO extends FunctionUnitIO {
  val cfIn = Flipped(new CtrlFlowIO)
  val redirect = new RedirectIO
  val offset = Input(UInt(XLEN.W))
}

class ALU extends NOOPModule {
  val io = IO(new ALUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val src132 = src1(31,0)
  val src232 = src2(31,0)

  val isAdderSub = (func =/= ALUOpType.add) && (func =/= ALUOpType.addw) && !ALUOpType.isJump(func)
  val adderRes = (src1 +& (src2 ^ Fill(XLEN, isAdderSub))) + isAdderSub
  val adderWRes = (src132 +& (src232 ^ Fill(32, isAdderSub))) + isAdderSub
  val xorRes = src1 ^ src2
  val sltu = !adderRes(XLEN)
  val slt = xorRes(XLEN-1) ^ sltu

  val shamt = src2(4, 0)
  val shamt64 = src2(5, 0)//TODO
  val aluRes = LookupTreeDefault(func, adderRes, List(
    ALUOpType.sll  -> ((src1  << shamt64)(XLEN-1, 0)),
    ALUOpType.slt  -> ZeroExt(slt, XLEN),
    ALUOpType.sltu -> ZeroExt(sltu, XLEN),
    ALUOpType.sllw -> SignExt((src132  << shamt)(31, 0), XLEN),
    ALUOpType.xor  -> xorRes,
    ALUOpType.srl  -> (src1  >> shamt64),
    ALUOpType.srlw -> SignExt((src132  >> shamt)(31,0), XLEN),
    ALUOpType.or   -> (src1  |  src2),
    ALUOpType.and  -> (src1  &  src2),
    ALUOpType.sra  -> ((src1.asSInt >> shamt64).asUInt),
    ALUOpType.sraw -> SignExt((src132.asSInt >> shamt).asUInt, XLEN),
    ALUOpType.addw -> SignExt(adderWRes(31,0), XLEN),
    ALUOpType.subw -> SignExt(adderWRes(31,0), XLEN)
  ))

  val branchOpTable = List(
    ALUOpType.getBranchType(ALUOpType.beq)  -> !xorRes.orR,
    ALUOpType.getBranchType(ALUOpType.blt)  -> slt,
    ALUOpType.getBranchType(ALUOpType.bltu) -> sltu
  )

  val isBranch = ALUOpType.isBranch(func)
  val isBru = ALUOpType.isBru(func)
  val taken = LookupTree(ALUOpType.getBranchType(func), branchOpTable) ^ ALUOpType.isBranchInvert(func)
  val target = Mux(isBranch, io.cfIn.pc + io.offset, adderRes)
  val predictWrong = (io.redirect.target =/= io.cfIn.pnpc)
  io.redirect.target := Mux(!taken && isBranch, io.cfIn.pc + 4.U, target)
  // with branch predictor, this is actually to fix the wrong prediction
  io.redirect.valid := valid && isBru && predictWrong
  // may be can move to ISU to calculate pc + 4
  // this is actually for jal and jalr to write pc + 4 to rd
  io.out.bits := Mux(isBru, io.cfIn.pc + 4.U, aluRes)

  io.in.ready := true.B
  io.out.valid := valid

  Debug(){
    when(io.out.valid){
      printf("[ALU] func: %b 1: %x, 2: %x, out: %x\n", func, src1, src2, io.out.bits)
    }
  }

  val bpuUpdateReq = WireInit(0.U.asTypeOf(new BPUUpdateReq))
  bpuUpdateReq.valid := valid && isBru
  bpuUpdateReq.pc := io.cfIn.pc
  bpuUpdateReq.isMissPredict := predictWrong
  bpuUpdateReq.actualTarget := target
  bpuUpdateReq.actualTaken := taken
  bpuUpdateReq.fuOpType := func
  bpuUpdateReq.btbType := LookupTree(func, RV32I_BRUInstr.bruFuncTobtbTypeTable)

  BoringUtils.addSource(RegNext(bpuUpdateReq), "bpuUpdateReq")

  val right = valid && isBru && !predictWrong
  val wrong = valid && isBru && predictWrong
  BoringUtils.addSource(right && isBranch, "MbpBRight")
  BoringUtils.addSource(wrong && isBranch, "MbpBWrong")
  BoringUtils.addSource(right && (func === ALUOpType.jal || func === ALUOpType.call), "MbpJRight")
  BoringUtils.addSource(wrong && (func === ALUOpType.jal || func === ALUOpType.call), "MbpJWrong")
  BoringUtils.addSource(right && func === ALUOpType.jalr, "MbpIRight")
  BoringUtils.addSource(wrong && func === ALUOpType.jalr, "MbpIWrong")
  BoringUtils.addSource(right && func === ALUOpType.ret, "MbpRRight")
  BoringUtils.addSource(wrong && func === ALUOpType.ret, "MbpRWrong")
}
