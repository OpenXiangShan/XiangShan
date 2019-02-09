package core

import chisel3._
import chisel3.util._

object CSR {
  val Mstatus = 0x300
  val Mtvec   = 0x305
  val Mepc    = 0x341
  val Mcause  = 0x342

  val privEcall = 0x000.U
  val privMret  = 0x302.U
}

import CSR._
import Decode._

class CSR {
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
