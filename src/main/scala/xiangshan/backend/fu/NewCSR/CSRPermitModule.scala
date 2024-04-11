package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState

class CSRPermitModule extends Module {
  val io = IO(new CSRPermitIO)

  val (wen, addr, privState) = (
    io.in.wen,
    io.in.addr,
    io.in.privState
  )

  private val isRO = addr(11, 10) === "b11".U

  private val accessTable = TruthTable(Seq(
    //       V PRVM ADDR
    BitPat("b?__00___00") -> BitPat.Y(), // HU/VU access U
    BitPat("b?__00___??") -> BitPat.N(), // HU/VU access the others
    BitPat("b1__01___00") -> BitPat.Y(), // VS access U
    BitPat("b1__01___01") -> BitPat.Y(), // VS access S
    BitPat("b1__01___??") -> BitPat.N(), // VS access the others
    BitPat("b0__01___11") -> BitPat.N(), // HS access M
    BitPat("b0__01___??") -> BitPat.Y(), // HS access the others
    BitPat("b0__11___??") -> BitPat.Y(), // M  access any
  ), BitPat.N())

  private val privilegeLegal = chisel3.util.experimental.decode.decoder(
    privState.V.asUInt ## privState.PRVM.asUInt ## addr(9, 8),
    accessTable
  ).asBool

  private val rwLegal = !(isRO && wen)

  io.out.legal := privilegeLegal && rwLegal
}

class CSRPermitIO extends Bundle {
  val in = new Bundle {
    val wen  = Bool()
    val addr = UInt(12.W)
    val privState = new PrivState
  }

  val out = new Bundle {
    val legal = Bool()
  }
}
