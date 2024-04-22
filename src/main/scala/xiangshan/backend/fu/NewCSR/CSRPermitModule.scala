package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState

class CSRPermitModule extends Module {
  val io = IO(new CSRPermitIO)

  private val (csrAccess, wen, addr, privState) = (
    io.in.csrAccess.valid,
    io.in.csrAccess.bits.wen,
    io.in.csrAccess.bits.addr,
    io.in.privState
  )

  private val (mret, sret) = (
    io.in.mret,
    io.in.sret,
  )

  private val (tsr, vtsr) = (
    io.in.status.tsr,
    io.in.status.vtsr,
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

  private val rwLegal = isRO && wen

  private val csrAccessIllegal = csrAccess && (!privilegeLegal || !rwLegal)

  private val mretIllegal = mret && !privState.isModeM

  private val sretIllegal = sret && (
    privState.isModeHS && tsr || privState.isModeVS && vtsr || privState.isModeHUorVU
  )

  io.out.illegal := csrAccessIllegal || mretIllegal || sretIllegal
}

class CSRPermitIO extends Bundle {
  val in = Input(new Bundle {
    val csrAccess = ValidIO(new Bundle {
      val wen = Bool()
      val addr = UInt(12.W)
    })
    val privState = new PrivState
    val mret = Bool()
    val sret = Bool()
    val status = new Bundle {
      // Trap SRET
      val tsr = Bool()
      // Virtual Trap SRET
      val vtsr = Bool()
    }
  })

  val out = Output(new Bundle {
    val illegal = Bool()
  })
}
