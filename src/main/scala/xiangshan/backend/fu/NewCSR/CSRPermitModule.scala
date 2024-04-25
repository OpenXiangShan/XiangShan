package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import xiangshan.backend.fu.NewCSR.CSRBundles.PrivState

class CSRPermitModule extends Module {
  val io = IO(new CSRPermitIO)

  private val (ren, wen, addr, privState) = (
    io.in.csrAccess.ren,
    io.in.csrAccess.wen,
    io.in.csrAccess.addr,
    io.in.privState
  )

  private val csrAccess = WireInit(ren || wen)

  private val (mret, sret) = (
    io.in.mret,
    io.in.sret,
  )

  private val (tsr, vtsr) = (
    io.in.status.tsr,
    io.in.status.vtsr,
  )

  private val csrIsRO = addr(11, 10) === "b11".U

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

  private val rwIllegal = csrIsRO && wen

  private val csrAccessIllegal = (!privilegeLegal || rwIllegal)

  private val mretIllegal = !privState.isModeM

  private val sretIllegal = sret && (
    privState.isModeHS && tsr || privState.isModeVS && vtsr || privState.isModeHUorVU
  )

  io.out.illegal := csrAccess && csrAccessIllegal || mret && mretIllegal || sret && sretIllegal

  // Todo: check correct
  io.out.EX_II := io.out.illegal && !privState.isVirtual
  io.out.EX_VI := io.out.illegal && privState.isVirtual

  io.out.hasLegalWen := io.in.csrAccess.wen && !csrAccessIllegal
  io.out.hasLegalMret := mret && !mretIllegal
  io.out.hasLegalSret := sret && !sretIllegal
}

class CSRPermitIO extends Bundle {
  val in = Input(new Bundle {
    val csrAccess = new Bundle {
      val ren = Bool()
      val wen = Bool()
      val addr = UInt(12.W)
    }
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
    val hasLegalWen = Bool()
    val hasLegalMret = Bool()
    val hasLegalSret = Bool()
    // Todo: split illegal into EX_II and EX_VI
    val illegal = Bool()
    val EX_II = Bool()
    val EX_VI = Bool()
  })
}
