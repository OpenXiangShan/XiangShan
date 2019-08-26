package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

trait HasBRUOpType {
  val BruOpTypeNum  = 10

  def BruJal  = "b11000".U
  def BruJalr = "b11010".U
  def BruBeq  = "b10000".U
  def BruBne  = "b10001".U
  def BruBlt  = "b10100".U
  def BruBge  = "b10101".U
  def BruBltu = "b10110".U
  def BruBgeu = "b10111".U

  // for RAS
  def BruCall = "b11100".U
  def BruRet  = "b11110".U

  def isBru(func: UInt) = func(4)
  def isBranch(func: UInt) = !func(3)
  def isJump(func: UInt) = isBru(func) && !isBranch(func)
  def getBranchType(func: UInt) = func(2, 1)
  def isBranchInvert(func: UInt) = func(0)
}

object BRUInstr extends HasDecodeConst {
  def JAL     = BitPat("b????????????????????_?????_1101111")
  def JALR    = BitPat("b????????????_?????_000_?????_1100111")

  def BNE     = BitPat("b???????_?????_?????_001_?????_1100011")
  def BEQ     = BitPat("b???????_?????_?????_000_?????_1100011")
  def BLT     = BitPat("b???????_?????_?????_100_?????_1100011")
  def BGE     = BitPat("b???????_?????_?????_101_?????_1100011")
  def BLTU    = BitPat("b???????_?????_?????_110_?????_1100011")
  def BGEU    = BitPat("b???????_?????_?????_111_?????_1100011")

  val table = Array(
    JAL            -> List(InstrJ, FuAlu, BruJal),
    JALR           -> List(InstrI, FuAlu, BruJalr),

    BEQ            -> List(InstrB, FuAlu, BruBeq),
    BNE            -> List(InstrB, FuAlu, BruBne),
    BLT            -> List(InstrB, FuAlu, BruBlt),
    BGE            -> List(InstrB, FuAlu, BruBge),
    BLTU           -> List(InstrB, FuAlu, BruBltu),
    BGEU           -> List(InstrB, FuAlu, BruBgeu)
  )

  val bruFuncTobtbTypeTable = List(
    BruBeq  -> BTBtype.B,
    BruBne  -> BTBtype.B,
    BruBlt  -> BTBtype.B,
    BruBge  -> BTBtype.B,
    BruBltu -> BTBtype.B,
    BruBgeu -> BTBtype.B,
    BruCall -> BTBtype.J,
    BruRet  -> BTBtype.R,
    BruJal  -> BTBtype.J,
    BruJalr -> BTBtype.I
  )
}

class BRUIO extends FunctionUnitIO {
  val pc = Input(UInt(32.W))
  val npc = Input(UInt(32.W))
  val offset = Input(UInt(32.W))
  val branch = new BranchIO
}

class BRU extends Module with HasBRUOpType {
  val io = IO(new BRUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val branchOpTable = List(
    getBranchType(BruBeq)  -> (src1 === src2),
    getBranchType(BruBlt)  -> (src1.asSInt < src2.asSInt),
    getBranchType(BruBltu) -> (src1 < src2)
  )

  val taken = LookupTree(getBranchType(func), false.B, branchOpTable) ^ isBranchInvert(func)
  val target = Mux(func === BruJalr || func === BruRet, src1, io.pc) + io.offset
  io.branch.target := Mux(!taken && isBranch(func), io.pc + 4.U, target)
  // with branch predictor, this is actually to fix the wrong prediction
  io.branch.isTaken := valid && (io.branch.target =/= io.npc)
  io.out.bits := io.pc + 4.U

  io.in.ready := true.B
  io.out.valid := valid

  val bpuUpdateReq = WireInit(0.U.asTypeOf(new BPUUpdateReq))
  bpuUpdateReq.valid := valid
  bpuUpdateReq.pc := io.pc
  bpuUpdateReq.isMissPredict := io.branch.target =/= io.npc
  bpuUpdateReq.actualTarget := target
  bpuUpdateReq.actualTaken := taken
  bpuUpdateReq.fuOpType := func
  bpuUpdateReq.btbType := LookupTree(func, BRUInstr.bruFuncTobtbTypeTable)

  BoringUtils.addSource(RegNext(bpuUpdateReq), "bpuUpdateReq")

  val right = valid && (io.npc === io.branch.target)
  val wrong = valid && (io.npc =/= io.branch.target)
  BoringUtils.addSource(right && isBranch(func), "MbpBRight")
  BoringUtils.addSource(wrong && isBranch(func), "MbpBWrong")
  BoringUtils.addSource(right && (func === BruJal || func === BruCall), "MbpJRight")
  BoringUtils.addSource(wrong && (func === BruJal || func === BruCall), "MbpJWrong")
  BoringUtils.addSource(right && func === BruJalr, "MbpIRight")
  BoringUtils.addSource(wrong && func === BruJalr, "MbpIWrong")
  BoringUtils.addSource(right && func === BruRet, "MbpRRight")
  BoringUtils.addSource(wrong && func === BruRet, "MbpRWrong")
}
