package noop

import chisel3._
import chisel3.util._

import utils._

trait HasCSROpType {
  val CsrOpTypeNum  = 4

  def CsrJmp  = "b00".U
  def CsrWrt  = "b01".U
  def CsrSet  = "b10".U
  def CsrClr  = "b11".U
}

object CSRInstr extends HasDecodeConst {
  def CSRRW   = BitPat("b????????????_?????_001_?????_1110011")
  def CSRRS   = BitPat("b????????????_?????_010_?????_1110011")
  def ECALL   = BitPat("b001100000010_00000_000_00000_1110011")
  def MRET    = BitPat("b000000000000_00000_000_00000_1110011")

  val table = Array(
    CSRRW          -> List(InstrI, FuCsr, CsrWrt),
    CSRRS          -> List(InstrI, FuCsr, CsrSet),
    ECALL          -> List(InstrI, FuCsr, CsrJmp),
    MRET           -> List(InstrI, FuCsr, CsrJmp)
  )
}

trait HasCSRConst {
  val Mstatus       = 0x300
  val Mtvec         = 0x305
  val Mepc          = 0x341
  val Mcause        = 0x342

  val Mcycle        = 0xb00
  val Minstret      = 0xb02
  val Mcycleh       = 0xb80
  val Minstreth     = 0xb82

  def privEcall = 0x000.U
  def privMret  = 0x302.U
}

class CSRIO extends FunctionUnitIO {
  val pc = Input(UInt(32.W))
  val csrjmp = new BranchIO
  // exception
  val isInvOpcode = Input(Bool())
  // perfcnt
  val instrCommit = Input(Bool())
}

class CSR extends Module with HasCSROpType with HasCSRConst {
  val io = IO(new CSRIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val mtvec = Reg(UInt(32.W))
  val mcause = Reg(UInt(32.W))
  val mstatus = Reg(UInt(32.W))
  val mepc = Reg(UInt(32.W))
  val mcycle = Reg(UInt(64.W))
  val minstret = Reg(UInt(64.W))

  val scalaMapping = List(
    Mtvec   -> mtvec,
    Mcause  -> mcause,
    Mepc    -> mepc,
    Mstatus -> mstatus,

    Mcycle  -> mcycle(31, 0),
    Mcycleh -> mcycle(63, 32),
    Minstret  -> minstret(31, 0),
    Minstreth -> minstret(63, 32)
  )

  val chiselMapping = scalaMapping.map { case (x, y) => (x.U -> y) }

  def readWithScala(addr: Int): UInt = {
    scalaMapping.filter { case (x, y) => x == addr } (0)._2
  }

  val addr = src2(11, 0)
  val rdata = LookupTree(addr, 0.U, chiselMapping)
  val wdata = LookupTree(func, 0.U, List(
    CsrWrt -> src1,
    CsrSet -> (rdata | src1),
    CsrClr -> (rdata & ~src1)
  ))

  when (valid && func =/= CsrJmp) {
    when (addr === Mtvec.U) { mtvec := wdata }
    when (addr === Mstatus.U) { mstatus := wdata }
    when (addr === Mepc.U) { mepc := wdata }
    when (addr === Mcause.U) { mcause := wdata }
  }

  io.out.bits := rdata

  val isMret = addr === privMret
  val isException = io.isInvOpcode
  val isEcall = (addr === privEcall) && !isException
  val exceptionNO = Mux1H(List(
    io.isInvOpcode -> 2.U,
    isEcall -> 11.U
  ))

  io.csrjmp.isTaken := (valid && func === CsrJmp) || isException
  io.csrjmp.target := Mux(isMret, mepc, mtvec)

  when (io.csrjmp.isTaken && !isMret) {
    mepc := io.pc
    mcause := exceptionNO
  }

  mcycle := mcycle + 1.U
  when (io.instrCommit) { minstret := minstret + 1.U }

  io.in.ready := true.B
  io.out.valid := valid
}
