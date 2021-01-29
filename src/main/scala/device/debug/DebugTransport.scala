// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.debug

import chisel3._
import chisel3.util._
import chisel3.experimental.chiselName


import freechips.rocketchip.config._
import freechips.rocketchip.jtag._
import freechips.rocketchip.util.property._


case class JtagDTMConfig (
  idcodeVersion    : Int,      // chosen by manuf.
  idcodePartNum    : Int,      // Chosen by manuf.
  idcodeManufId    : Int,      // Assigned by JEDEC
  // Note: the actual fields are passed in through wires.
  // Do not forget to wire up io.jtag_mfr_id through your top-level to set the
  // mfr_id for this core.
  // If you wish to use this field in the config, you can obtain it along
  // the lines of p(JtagDTMKey).idcodeManufId.U(11.W).
  debugIdleCycles  : Int)

case object JtagDTMKey extends Field[JtagDTMConfig](new JtagDTMKeyDefault())

class JtagDTMKeyDefault extends JtagDTMConfig(
  idcodeVersion = 0,
  idcodePartNum = 0,
  idcodeManufId = 0,
  debugIdleCycles = 5) // Reasonable guess for synchronization.

object dtmJTAGAddrs {
  def IDCODE       = 0x1
  def DTM_INFO     = 0x10
  def DMI_ACCESS = 0x11
}

class DMIAccessUpdate(addrBits: Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val data = UInt(DMIConsts.dmiDataSize.W)
  val op = UInt(DMIConsts.dmiOpSize.W)

  override def cloneType = new DMIAccessUpdate(addrBits).asInstanceOf[this.type]
}

class DMIAccessCapture(addrBits: Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val data = UInt(DMIConsts.dmiDataSize.W)
  val resp = UInt(DMIConsts.dmiRespSize.W)

  override def cloneType = new DMIAccessCapture(addrBits).asInstanceOf[this.type]

}

class DTMInfo extends Bundle {
  val reserved1 = UInt(15.W)
  val dmireset = Bool()
  val reserved0 = UInt(1.W)
  val dmiIdleCycles = UInt(3.W)
  val dmiStatus = UInt(2.W)
  val debugAddrBits = UInt(6.W)
  val debugVersion = UInt(4.W)
}

/** A wrapper around JTAG providing a reset signal and manufacturer id. */
class SystemJTAGIO extends Bundle {
  val jtag = Flipped(new JTAGIO(hasTRSTn = false))
  val reset = Input(Reset())
  val mfr_id = Input(UInt(11.W))
  val part_number = Input(UInt(16.W))
  val version = Input(UInt(4.W))
}

// Use the Chisel Name macro due to the bulk of this being inside a withClockAndReset block
@chiselName
class DebugTransportModuleJTAG(debugAddrBits: Int, c: JtagDTMConfig)
  (implicit val p: Parameters) extends RawModule  {

  val io = IO(new Bundle {
    val jtag_clock = Input(Clock())
    val jtag_reset = Input(Reset()) // This is internally converted to AsyncReset.
                                    // We'd prefer to call this AsyncReset, but that's a fairly
                                    // invasive API change.
    val dmi = new DMIIO()(p)
    val jtag = Flipped(new JTAGIO(hasTRSTn = false)) // TODO: re-use SystemJTAGIO here?
    val jtag_mfr_id = Input(UInt(11.W))
    val jtag_part_number = Input(UInt(16.W))
    val jtag_version = Input(UInt(4.W))
  })
  val rf_reset = IO(Input(Reset()))    // RF transform

  withClockAndReset(io.jtag_clock, io.jtag_reset.asAsyncReset) {

  //--------------------------------------------------------
  // Reg and Wire Declarations

  val dtmInfo = Wire(new DTMInfo)

  val busyReg = RegInit(false.B)
  val stickyBusyReg = RegInit(false.B)
  val stickyNonzeroRespReg = RegInit(false.B)

  val downgradeOpReg = RegInit(false.B) // downgrade op because prev. failed.

  val busy = Wire(Bool())
  val nonzeroResp = Wire(Bool())

  val busyResp    = Wire(new DMIAccessCapture(debugAddrBits))
  val dmiResp     = Wire(new DMIAccessCapture(debugAddrBits))
  val nopResp     = Wire(new DMIAccessCapture(debugAddrBits))


  val dmiReqReg  = Reg(new DMIReq(debugAddrBits))
  val dmiReqValidReg = RegInit(false.B)

  val dmiStatus = Wire(UInt(2.W))

  //--------------------------------------------------------
  // DTM Info Chain Declaration

  dmiStatus := Cat(stickyNonzeroRespReg, stickyNonzeroRespReg | stickyBusyReg)

  dtmInfo.debugVersion  := 1.U // This implements version 1 of the spec.
  dtmInfo.debugAddrBits := debugAddrBits.U
  dtmInfo.dmiStatus     := dmiStatus
  dtmInfo.dmiIdleCycles := c.debugIdleCycles.U
  dtmInfo.reserved0     := 0.U
  dtmInfo.dmireset      := false.B // This is write-only
  dtmInfo.reserved1     := 0.U

  val dtmInfoChain = Module (CaptureUpdateChain(gen = new DTMInfo()))
  dtmInfoChain.io.capture.bits := dtmInfo

  //--------------------------------------------------------
  // Debug Access Chain Declaration

   val dmiAccessChain = Module(CaptureUpdateChain(genCapture = new DMIAccessCapture(debugAddrBits),
     genUpdate = new DMIAccessUpdate(debugAddrBits)))

  //--------------------------------------------------------
  // Debug Access Support

  // Busy Register. We become busy when we first try to send a request.
  // We stop being busy when we accept a response.

  when (io.dmi.req.valid) {
    busyReg := true.B
  }
  when (io.dmi.resp.fire()) {
    busyReg := false.B
  }

  // We are busy during a given CAPTURE
  // if we haven't received a valid response yet or if we
  // were busy last time without a reset.
  // busyReg will still be set when we check it,
  // so the logic for checking busy looks ahead.
  busy := (busyReg & !io.dmi.resp.valid) | stickyBusyReg;

  // Downgrade/Skip. We make the decision to downgrade or skip
  // during every CAPTURE_DR, and use the result in UPDATE_DR.
  // The sticky versions are reset by write to dmiReset in DTM_INFO.
  when (dmiAccessChain.io.update.valid) {
    downgradeOpReg := false.B
  }
  when (dmiAccessChain.io.capture.capture) {
    downgradeOpReg := (!busy & nonzeroResp)
    stickyBusyReg := busy
    stickyNonzeroRespReg := nonzeroResp
  }
  when (dtmInfoChain.io.update.valid) {
    when (dtmInfoChain.io.update.bits.dmireset) {
      stickyNonzeroRespReg := false.B
      stickyBusyReg := false.B
    }
  }

  // Especially for the first request, we must consider dtmResp.valid,
  // so that we don't consider junk in the FIFO to be an error response.
  // The current specification says that any non-zero response is an error.
  nonzeroResp := stickyNonzeroRespReg | (io.dmi.resp.valid & (io.dmi.resp.bits.resp =/= 0.U))
  cover(!nonzeroResp, "Should see a non-zero response (e.g. when accessing most DM registers when dmactive=0)")
  cover(!stickyNonzeroRespReg, "Should see a sticky non-zero response (e.g. when accessing most DM registers when dmactive=0)")

  busyResp.addr  := 0.U
  busyResp.resp  := ~(0.U(DMIConsts.dmiRespSize.W)) // Generalizing busy to 'all-F'
  busyResp.data  := 0.U

  dmiResp.addr := dmiReqReg.addr
  dmiResp.resp := io.dmi.resp.bits.resp
  dmiResp.data := io.dmi.resp.bits.data

  nopResp.addr := 0.U
  nopResp.resp := 0.U
  nopResp.data := 0.U

  //--------------------------------------------------------
  // Debug Access Chain Implementation

  dmiAccessChain.io.capture.bits := Mux(busy, busyResp, Mux(io.dmi.resp.valid, dmiResp, nopResp))

  //--------------------------------------------------------
  // Drive Ready Valid Interface

  val dmiReqValidCheck = WireInit(false.B)
  assert(!(dmiReqValidCheck && io.dmi.req.fire()), "Conflicting updates for dmiReqValidReg, should not happen.");

  when (dmiAccessChain.io.update.valid) {
    when (stickyBusyReg) {
      // Do Nothing
    }.elsewhen (downgradeOpReg || (dmiAccessChain.io.update.bits.op === DMIConsts.dmi_OP_NONE)) {
      //Do Nothing
      dmiReqReg.addr := 0.U
      dmiReqReg.data := 0.U
      dmiReqReg.op   := 0.U
    }.otherwise {
      dmiReqReg := dmiAccessChain.io.update.bits
      dmiReqValidReg := true.B
      dmiReqValidCheck := true.B
    }
  }

  when (io.dmi.req.fire()) {
    dmiReqValidReg := false.B
  }

  io.dmi.resp.ready := Mux(
    dmiReqReg.op === DMIConsts.dmi_OP_WRITE,
      // for write operations confirm resp immediately because we don't care about data
      io.dmi.resp.valid,
      // for read operations confirm resp when we capture the data
      dmiAccessChain.io.capture.capture & !busy)

  // incorrect operation - not enough time was spent in JTAG Idle state after DMI Write
  cover(dmiReqReg.op === DMIConsts.dmi_OP_WRITE & dmiAccessChain.io.capture.capture & busy, "Not enough Idle after DMI Write");
  // correct operation - enough time was spent in JTAG Idle state after DMI Write
  cover(dmiReqReg.op === DMIConsts.dmi_OP_WRITE & dmiAccessChain.io.capture.capture & !busy, "Enough Idle after DMI Write");

  // incorrect operation - not enough time was spent in JTAG Idle state after DMI Read
  cover(dmiReqReg.op === DMIConsts.dmi_OP_READ & dmiAccessChain.io.capture.capture & busy, "Not enough Idle after DMI Read");
  // correct operation - enough time was spent in JTAG Idle state after DMI Read
  cover(dmiReqReg.op === DMIConsts.dmi_OP_READ & dmiAccessChain.io.capture.capture & !busy, "Enough Idle after DMI Read");

  io.dmi.req.valid := dmiReqValidReg

  // This is a name-based, not type-based assignment. Do these still work?
  io.dmi.req.bits := dmiReqReg

  //--------------------------------------------------------
  // Actual JTAG TAP
  val idcode = WireInit(0.U.asTypeOf(new JTAGIdcodeBundle()))
  idcode.always1    := 1.U
  idcode.version    := io.jtag_version
  idcode.partNumber := io.jtag_part_number
  idcode.mfrId      := io.jtag_mfr_id

  val tapIO = JtagTapGenerator(irLength = 5,
    instructions = Map(
      dtmJTAGAddrs.DMI_ACCESS -> dmiAccessChain,
      dtmJTAGAddrs.DTM_INFO   -> dtmInfoChain),
    icode = Some(dtmJTAGAddrs.IDCODE)
  )

  tapIO.idcode.get := idcode
  tapIO.jtag <> io.jtag

  tapIO.control.jtag_reset := io.jtag_reset.asAsyncReset

  //--------------------------------------------------------
  // TAP Test-Logic-Reset state synchronously resets the debug registers.

  when (tapIO.output.tapIsInTestLogicReset) {
    busyReg := false.B
    stickyBusyReg := false.B
    stickyNonzeroRespReg := false.B
    downgradeOpReg := false.B
    dmiReqValidReg := false.B
  }

}}
