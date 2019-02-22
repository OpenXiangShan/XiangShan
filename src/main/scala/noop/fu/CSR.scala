package noop

import chisel3._
import chisel3.util._

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

class CSR extends HasCSROpType with HasCSRConst {
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

  def access(isCsr: Bool, addr: UInt, src: UInt, cmd: UInt): UInt = {
    val rdata = LookupTree(addr, 0.U, chiselMapping)
    val wdata = LookupTree(cmd, 0.U, List(
      CsrWrt -> src,
      CsrSet -> (rdata | src),
      CsrClr -> (rdata & ~src)
    ))

    when (isCsr && cmd =/= CsrJmp) {
      when (addr === Mtvec.U) { mtvec := wdata }
      when (addr === Mstatus.U) { mstatus := wdata }
      when (addr === Mepc.U) { mepc := wdata }
      when (addr === Mcause.U) { mcause := wdata }
    }

    rdata
  }

  def jmp(isCsr: Bool, addr: UInt, pc: UInt, cmd: UInt, isException: Bool, exceptionNO: UInt): BranchIO = {
    val csrjmp = Wire(new BranchIO)
    csrjmp.isTaken := isCsr && cmd === CsrJmp
    csrjmp.target := LookupTree(addr, 0.U, List(
      privEcall -> mtvec,
      privMret  -> mepc
    ))

    val isEcall = (addr === privEcall)
    when (csrjmp.isTaken && (isEcall || isException)) {
      mepc := pc
      mcause := Mux(isException, exceptionNO, 11.U)
    }
    csrjmp
  }

  mcycle := mcycle + 1.U
  def instrCnt(instrCommit: Bool) {
    when (instrCommit) {
      minstret := minstret + 1.U
    }
  }
}
