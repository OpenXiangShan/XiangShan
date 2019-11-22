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

  def addw = "b100000".U
  def subw = "b101000".U
  def sllw = "b100001".U
  def srlw = "b100101".U
  def sraw = "b101101".U

  def isWordOp(func: UInt) = func(5)

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

  def isBru(func: UInt) = func(4)//[important]
  def pcPlus2(func: UInt) = func(5)//[important]
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

  val isBranch = ALUOpType.isBranch(func)
  val isBru = ALUOpType.isBru(func)
  // val pcPlus2 = ALUOpType.pcPlus2(func)
  val taken = LookupTree(ALUOpType.getBranchType(func), branchOpTable) ^ ALUOpType.isBranchInvert(func)
  val target = Mux(isBranch, io.cfIn.pc + io.offset, adderRes)(AddrBits-1,0)
  val predictWrong = (io.redirect.target =/= io.cfIn.pnpc)
  val isRVC = (io.cfIn.instr(1,0) =/= "b11".U)
  io.redirect.target := Mux(!taken && isBranch, Mux(isRVC, io.cfIn.pc + 2.U, io.cfIn.pc + 4.U), target)
  // with branch predictor, this is actually to fix the wrong prediction
  io.redirect.valid := valid && isBru && predictWrong
  // may be can be moved to ISU to calculate pc + 4
  // this is actually for jal and jalr to write pc + 4/2 to rd
  io.out.bits := Mux(isBru, Mux(!isRVC, io.cfIn.pc + 4.U, io.cfIn.pc + 2.U), aluRes)
  // when(pcPlus2 && isBru){
  //   printf("CJALR %x %x \n ", io.cfIn.instr, io.cfIn.pc)
  // }

  Debug(){
    when(valid && isBru){
      printf("[BRU] tgt %x, valid:%d, npc: %x, pdwrong: %x\n", io.redirect.target, io.redirect.valid, io.cfIn.pnpc, predictWrong)
      printf("[BRU] taken:%d addrRes:%x src1:%x src2:%x func:%x\n", taken, adderRes, src1, src2, func) 
    }
  }

  Debug(false){
    when(valid && isBru){
      printf("[BPW] pc %x tgt %x, npc: %x, pdwrong: %x type: %x%x%x%x\n", io.cfIn.pc, io.redirect.target, io.cfIn.pnpc, predictWrong, isBranch, (func === ALUOpType.jal || func === ALUOpType.call), func === ALUOpType.jalr, func === ALUOpType.ret)
    }

    when(true.B) {
      printf("[ALUIN0] valid:%d isBru:%d isBranch:%d \n", valid, isBru, isBranch)
      printf("[ALUIN1] pc %x instr %x tgt %x, npc: %x, pdwrong: %x type: %x%x%x%x\n", io.cfIn.pc, io.cfIn.instr, io.redirect.target, io.cfIn.pnpc, predictWrong, isBranch, (func === ALUOpType.jal || func === ALUOpType.call), func === ALUOpType.jalr, func === ALUOpType.ret)
      printf("[ALUIN2] func:%b ", func)
      printf(" bpuUpdateReq: valid:%d pc:%x isMissPredict:%d actualTarget:%x actualTaken:%x fuOpType:%x btbType:%x isRVC:%d \n", valid && isBru, io.cfIn.pc, predictWrong, target, taken, func, LookupTree(func, RV32I_BRUInstr.bruFuncTobtbTypeTable), isRVC)
      printf("[ALUIN3]tgt %x, npc: %x, pdwrong: %x\n", io.redirect.target, io.cfIn.pnpc, predictWrong)
      printf("[ALUIN4]taken:%d addrRes:%x src1:%x src2:%x func:%x\n", taken, adderRes, src1, src2, func)
    }
  }

  io.in.ready := true.B
  io.out.valid := valid

  val bpuUpdateReq = WireInit(0.U.asTypeOf(new BPUUpdateReq))
  bpuUpdateReq.valid := valid && isBru
  bpuUpdateReq.pc := io.cfIn.pc
  bpuUpdateReq.isMissPredict := predictWrong
  bpuUpdateReq.actualTarget := target
  bpuUpdateReq.actualTaken := taken
  bpuUpdateReq.fuOpType := func
  bpuUpdateReq.btbType := LookupTree(func, RV32I_BRUInstr.bruFuncTobtbTypeTable)
  bpuUpdateReq.isRVC := isRVC

  BoringUtils.addSource(RegNext(bpuUpdateReq), "bpuUpdateReq")

  val right = valid && isBru && !predictWrong
  val wrong = valid && isBru && predictWrong
  BoringUtils.addSource(right && isBranch, "MbpBRight")
  BoringUtils.addSource(wrong && isBranch, "MbpBWrong")
  BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h0".U && isRVC, "Custom1")
  BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h0".U && !isRVC, "Custom2")
  BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h2".U && isRVC, "Custom3")
  BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h2".U && !isRVC, "Custom4")
  BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h4".U && isRVC, "Custom5")
  BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h4".U && !isRVC, "Custom6")
  BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h6".U && isRVC, "Custom7")
  BoringUtils.addSource(wrong && isBranch && io.cfIn.pc(2,0)==="h6".U && !isRVC, "Custom8")
  BoringUtils.addSource(right && (func === ALUOpType.jal || func === ALUOpType.call), "MbpJRight")
  BoringUtils.addSource(wrong && (func === ALUOpType.jal || func === ALUOpType.call), "MbpJWrong")
  BoringUtils.addSource(right && func === ALUOpType.jalr, "MbpIRight")
  BoringUtils.addSource(wrong && func === ALUOpType.jalr, "MbpIWrong")
  BoringUtils.addSource(right && func === ALUOpType.ret, "MbpRRight")
  BoringUtils.addSource(wrong && func === ALUOpType.ret, "MbpRWrong")
}
