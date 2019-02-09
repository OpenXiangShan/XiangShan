package core

import chisel3._
import chisel3.util._

trait HasCSROpType {
  val CsrOpTypeNum  = 4

  val CsrJmp  = "b00".U
  val CsrWrt  = "b01".U
  val CsrSet  = "b10".U
  val CsrClr  = "b11".U
}

trait CSRInstr extends HasDecodeConst {
  val CSRRW   = BitPat("b????????????_?????_001_?????_1110011")
  val CSRRS   = BitPat("b????????????_?????_010_?????_1110011")
  val ECALL   = BitPat("b001100000010_00000_000_00000_1110011")
  val MRET    = BitPat("b000000000000_00000_000_00000_1110011")

  val CSRInstrTable = Array(
    CSRRW          -> List(InstrI, FuCsr, CsrWrt),
    CSRRS          -> List(InstrI, FuCsr, CsrSet),
    ECALL          -> List(InstrI, FuCsr, CsrJmp),
    MRET           -> List(InstrI, FuCsr, CsrJmp)
  )
}

trait HasCSRConst {
  val Mstatus = 0x300
  val Mtvec   = 0x305
  val Mepc    = 0x341
  val Mcause  = 0x342

  val privEcall = 0x000.U
  val privMret  = 0x302.U
}

class CSR extends HasCSROpType with HasCSRConst {
  val mtvec = Reg(UInt(32.W))
  val mcause = Reg(UInt(32.W))
  val mstatus = Reg(UInt(32.W))
  val mepc = Reg(UInt(32.W))

  val scalaMapping = List(
    Mtvec   -> mtvec,
    Mcause  -> mcause,
    Mepc    -> mepc,
    Mstatus -> mstatus
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

  def jmp(isCsr: Bool, addr: UInt, pc: UInt, cmd: UInt): BranchIO = {
    val csrjmp = Wire(new BranchIO)
    csrjmp.isTaken := isCsr && cmd === CsrJmp
    csrjmp.target := LookupTree(addr, 0.U, List(
      privEcall -> mtvec,
      privMret  -> mepc
    ))

    when (csrjmp.isTaken && addr === privEcall) {
      mepc := pc
      mcause := 11.U
    }
    csrjmp
  }
}
