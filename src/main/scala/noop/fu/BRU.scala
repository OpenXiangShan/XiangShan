package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

trait HasBRUOpType {
  val BruOpTypeNum  = 10

  def BruJal  = "b1000".U
  def BruJalr = "b1001".U
  def BruBeq  = "b0000".U
  def BruBne  = "b0001".U
  def BruBlt  = "b0100".U
  def BruBge  = "b0101".U
  def BruBltu = "b0110".U
  def BruBgeu = "b0111".U

  // for RAS
  def BruCall = "b1100".U
  def BruRet  = "b1101".U

  def isBranch(func: UInt) = !func(3)
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
    JAL            -> List(InstrJ, FuBru, BruJal),
    JALR           -> List(InstrI, FuBru, BruJalr),

    BEQ            -> List(InstrB, FuBru, BruBeq),
    BNE            -> List(InstrB, FuBru, BruBne),
    BLT            -> List(InstrB, FuBru, BruBlt),
    BGE            -> List(InstrB, FuBru, BruBge),
    BLTU           -> List(InstrB, FuBru, BruBltu),
    BGEU           -> List(InstrB, FuBru, BruBgeu)
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

  def xorBool(a: Bool, b: Bool): Bool = (a.asUInt ^ b.asUInt).toBool

  val table = List(
    BruBeq  -> ((src1 === src2),               io.offset(31), BTBtype.B),
    BruBne  -> ((src1 =/= src2),               io.offset(31), BTBtype.B),
    BruBlt  -> ((src1.asSInt  <  src2.asSInt), io.offset(31), BTBtype.B),
    BruBge  -> ((src1.asSInt >=  src2.asSInt), io.offset(31), BTBtype.B),
    BruBltu -> ((src1  <  src2),               io.offset(31), BTBtype.B),
    BruBgeu -> ((src1  >= src2),               io.offset(31), BTBtype.B),
    BruCall -> (true.B,                        true.B,        BTBtype.J),
    BruRet  -> (true.B,                        false.B,       BTBtype.R),
    BruJal  -> (true.B,                        true.B,        BTBtype.J),
    BruJalr -> (true.B,                        false.B,       BTBtype.I)
  )
  val taken = LookupTree(func, false.B, table.map(x => (x._1, x._2._1)))
  io.branch.target := Mux(func === BruJalr || func === BruRet,
    src1 + io.offset, io.pc + Mux(taken, io.offset, 4.U))
  // with branch predictor, this is actually to fix the wrong prediction
  // to improve timing, we move the prediction checking to WBU statge
  io.branch.isTaken := valid
  io.out.bits := io.pc + 4.U

  io.in.ready := true.B
  io.out.valid := valid

  val bpuUpdateReq = WireInit(0.U.asTypeOf(new BPUUpdateReq))
  bpuUpdateReq.valid := valid
  bpuUpdateReq.pc := io.pc
  bpuUpdateReq.isMissPredict := io.branch.target =/= io.npc
  bpuUpdateReq.actualTarget := Mux(func === BruJalr || func === BruRet, src1, io.pc) + io.offset
  bpuUpdateReq.actualTaken := taken
  bpuUpdateReq.fuOpType := func
  bpuUpdateReq.btbType := LookupTree(func, table.map(x => (x._1, x._2._3)))

  BoringUtils.addSource(bpuUpdateReq, "bpuUpdateReq")

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
