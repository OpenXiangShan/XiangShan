package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

trait HasALUOpType {
  val AluOpTypeNum  = 11

  def AluAdd  = "b00000".U
  def AluSll  = "b00001".U
  def AluSlt  = "b00010".U
  def AluSltu = "b00011".U
  def AluXor  = "b00100".U
  def AluSrl  = "b00101".U
  def AluOr   = "b00110".U
  def AluAnd  = "b00111".U
  def AluSub  = "b01000".U
  def AluSra  = "b01101".U
  def AluLui  = "b01111".U
}

object ALUInstr extends HasDecodeConst {
  def ADDI    = BitPat("b????????????_?????_000_?????_0010011")
  def SLLI    = BitPat("b0000000?????_?????_001_?????_0010011")
  def SLTI    = BitPat("b????????????_?????_010_?????_0010011")
  def SLTIU   = BitPat("b????????????_?????_011_?????_0010011")
  def XORI    = BitPat("b????????????_?????_100_?????_0010011")
  def SRLI    = BitPat("b0000000?????_?????_101_?????_0010011")
  def ORI     = BitPat("b????????????_?????_110_?????_0010011")
  def ANDI    = BitPat("b????????????_?????_111_?????_0010011")
  def SRAI    = BitPat("b0100000?????_?????_101_?????_0010011")

  def ADD     = BitPat("b0000000_?????_?????_000_?????_0110011")
  def SLL     = BitPat("b0000000_?????_?????_001_?????_0110011")
  def SLT     = BitPat("b0000000_?????_?????_010_?????_0110011")
  def SLTU    = BitPat("b0000000_?????_?????_011_?????_0110011")
  def XOR     = BitPat("b0000000_?????_?????_100_?????_0110011")
  def SRL     = BitPat("b0000000_?????_?????_101_?????_0110011")
  def OR      = BitPat("b0000000_?????_?????_110_?????_0110011")
  def AND     = BitPat("b0000000_?????_?????_111_?????_0110011")
  def SUB     = BitPat("b0100000_?????_?????_000_?????_0110011")
  def SRA     = BitPat("b0100000_?????_?????_101_?????_0110011")

  def AUIPC   = BitPat("b????????????????????_?????_0010111")
  def LUI     = BitPat("b????????????????????_?????_0110111")

  val table = Array(
    ADDI           -> List(InstrI, FuAlu, AluAdd),
    SLLI           -> List(InstrI, FuAlu, AluSll),
    SLTI           -> List(InstrI, FuAlu, AluSlt),
    SLTIU          -> List(InstrI, FuAlu, AluSltu),
    XORI           -> List(InstrI, FuAlu, AluXor),
    SRLI           -> List(InstrI, FuAlu, AluSrl),
    ORI            -> List(InstrI, FuAlu, AluOr ),
    ANDI           -> List(InstrI, FuAlu, AluAnd),
    SRAI           -> List(InstrI, FuAlu, AluSra),

    ADD            -> List(InstrR, FuAlu, AluAdd),
    SLL            -> List(InstrR, FuAlu, AluSll),
    SLT            -> List(InstrR, FuAlu, AluSlt),
    SLTU           -> List(InstrR, FuAlu, AluSltu),
    XOR            -> List(InstrR, FuAlu, AluXor),
    SRL            -> List(InstrR, FuAlu, AluSrl),
    OR             -> List(InstrR, FuAlu, AluOr ),
    AND            -> List(InstrR, FuAlu, AluAnd),
    SUB            -> List(InstrR, FuAlu, AluSub),
    SRA            -> List(InstrR, FuAlu, AluSra),

    AUIPC          -> List(InstrU, FuAlu, AluAdd),
    LUI            -> List(InstrU, FuAlu, AluLui)
  )
}

class ALUIO extends FunctionUnitIO {
  val pc = Input(UInt(32.W))
  val npc = Input(UInt(32.W))
  val offset = Input(UInt(32.W))
  val branch = new BranchIO
}

class ALU extends Module with HasALUOpType with HasBRUOpType {
  val io = IO(new ALUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val isAdderSub = (func =/= AluAdd) && !isJump(func)
  val adderRes = (src1 +& (src2 ^ Fill(32, isAdderSub))) + isAdderSub
  val xorRes = src1 ^ src2
  val sltu = !adderRes(32)
  val slt = xorRes(31) ^ sltu

  val shamt = src2(4, 0)
  val aluRes = LookupTree(func, 0.U, List(
    BruJal  -> adderRes,
    BruJalr -> adderRes,
    AluAdd  -> adderRes,
    AluSll  -> ((src1  << shamt)(31, 0)),
    AluSlt  -> Cat(0.U(31.W), slt),
    AluSltu -> Cat(0.U(31.W), sltu),
    AluXor  -> xorRes,
    AluSrl  -> (src1  >> shamt),
    AluOr   -> (src1  |  src2),
    AluAnd  -> (src1  &  src2),
    AluSub  -> adderRes,
    AluLui  -> src2,
    AluSra  -> ((src1.asSInt >> shamt).asUInt)
  ))

  val branchOpTable = List(
    getBranchType(BruBeq)  -> (src1 === src2),
    getBranchType(BruBlt)  -> (src1.asSInt < src2.asSInt),
    getBranchType(BruBltu) -> (src1 < src2)
  )

  val taken = LookupTree(getBranchType(func), false.B, branchOpTable) ^ isBranchInvert(func)
  val target = Mux(isBranch(func), io.pc + io.offset, adderRes)
  io.branch.target := Mux(!taken && isBranch(func), io.pc + 4.U, target)
  // with branch predictor, this is actually to fix the wrong prediction
  io.branch.isTaken := valid && isBru(func) && (io.branch.target =/= io.npc)
  // may be can move to ISU to calculate pc + 4
  io.out.bits := Mux(isBru(func), io.pc + 4.U, aluRes)

  io.in.ready := true.B
  io.out.valid := valid

  val bpuUpdateReq = WireInit(0.U.asTypeOf(new BPUUpdateReq))
  bpuUpdateReq.valid := valid && isBru(func)
  bpuUpdateReq.pc := io.pc
  bpuUpdateReq.isMissPredict := io.branch.target =/= io.npc
  bpuUpdateReq.actualTarget := target
  bpuUpdateReq.actualTaken := taken
  bpuUpdateReq.fuOpType := func
  bpuUpdateReq.btbType := LookupTree(func, BRUInstr.bruFuncTobtbTypeTable)

  BoringUtils.addSource(RegNext(bpuUpdateReq), "bpuUpdateReq")

  val right = valid && isBru(func) && (io.npc === io.branch.target)
  val wrong = valid && isBru(func) && (io.npc =/= io.branch.target)
  BoringUtils.addSource(right && isBranch(func), "MbpBRight")
  BoringUtils.addSource(wrong && isBranch(func), "MbpBWrong")
  BoringUtils.addSource(right && (func === BruJal || func === BruCall), "MbpJRight")
  BoringUtils.addSource(wrong && (func === BruJal || func === BruCall), "MbpJWrong")
  BoringUtils.addSource(right && func === BruJalr, "MbpIRight")
  BoringUtils.addSource(wrong && func === BruJalr, "MbpIWrong")
  BoringUtils.addSource(right && func === BruRet, "MbpRRight")
  BoringUtils.addSource(wrong && func === BruRet, "MbpRWrong")
}
