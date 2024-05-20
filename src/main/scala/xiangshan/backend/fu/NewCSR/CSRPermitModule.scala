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

  private val (mret, sret, wfi) = (
    io.in.mret,
    io.in.sret,
    io.in.wfi,
  )

  private val (tsr, vtsr) = (
    io.in.status.tsr,
    io.in.status.vtsr,
  )

  private val (tw, vtw) = (
    io.in.status.tw,
    io.in.status.vtw
  )

  private val csrIsRO = addr(11, 10) === "b11".U

  private val accessTable = TruthTable(Seq(
    //       V PRVM ADDR
    BitPat("b0__00___00") -> BitPat.Y(), // HU access U
    BitPat("b1__00___00") -> BitPat.Y(), // VU access U
    BitPat("b0__01___00") -> BitPat.Y(), // HS access U
    BitPat("b0__01___01") -> BitPat.Y(), // HS access S
    BitPat("b1__01___00") -> BitPat.Y(), // VS access U
    BitPat("b1__01___01") -> BitPat.Y(), // VS access S
    BitPat("b0__11___00") -> BitPat.Y(), // M  access HU
    BitPat("b0__11___01") -> BitPat.Y(), // M  access HS
    BitPat("b0__11___11") -> BitPat.Y(), // M  access M
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

  private val wfi_EX_II = wfi && (!privState.isModeM && tw)
  private val wfi_EX_VI = wfi && (privState.isModeVS && vtw && !tw || privState.isModeVU && !tw)

  io.out.illegal := csrAccess && csrAccessIllegal || mret && mretIllegal || sret && sretIllegal

  // Todo: check correct
  io.out.EX_II := io.out.illegal && !privState.isVirtual || wfi_EX_II
  io.out.EX_VI := io.out.illegal && privState.isVirtual || wfi_EX_VI

  io.out.hasLegalWen := io.in.csrAccess.wen && !csrAccessIllegal
  io.out.hasLegalMret := mret && !mretIllegal
  io.out.hasLegalSret := sret && !sretIllegal
  io.out.hasLegalWfi := wfi && !wfi_EX_II && !wfi_EX_VI
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
    val wfi = Bool()
    val status = new Bundle {
      // Trap SRET
      val tsr = Bool()
      // Virtual Trap SRET
      val vtsr = Bool()
      // Timeout Wait
      val tw = Bool()
      // Virtual Timeout Wait
      val vtw = Bool()
    }
  })

  val out = Output(new Bundle {
    val hasLegalWen = Bool()
    val hasLegalMret = Bool()
    val hasLegalSret = Bool()
    val hasLegalWfi = Bool()
    // Todo: split illegal into EX_II and EX_VI
    val illegal = Bool()
    val EX_II = Bool()
    val EX_VI = Bool()
  })
}
