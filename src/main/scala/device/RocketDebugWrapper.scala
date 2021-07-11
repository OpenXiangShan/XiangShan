/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package device

import chisel3._
import xiangshan._
import chisel3.experimental.{IntParam, noPrefix}
import chisel3.util._
import chisel3.util.HasBlackBoxResource
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.LogicalModuleTree
import freechips.rocketchip.jtag._
import freechips.rocketchip.util._
import freechips.rocketchip.prci.{ClockSinkParameters, ClockSinkNode}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.debug

// this file uses code from rocketchip Periphery.scala
// to simplify the code we remove options for apb, cjtag and dmi
// this module creates wrapped dm, dtm, sba, and pulls out intr lines
class debugModule (implicit p: Parameters) extends LazyModule {

  val debug = LazyModule(new TLDebugModule(tlbus.beatBytes))

  debug.node := TLFragmenter() := peripheralXbar
  debug.dmInner.dmInner.customNode := debugCustomXbarOpt.get.node

  debug.dmInner.dmInner.sb2tlOpt.foreach { sb2tl  =>
    l2xbar := TLBuffer() := TLWidthWidget(1) := sb2tl.node
  }
  val fakeTreeNode = new GenericLogicalTreeNode
  LogicalModuleTree.add(fakeTreeNode, debug.logicalTreeNode)

  lazy val module = new LazyRawModuleImp(this) {
    val io = IO(new Bundle{
      val resetCtrl = new ResetCtrlIO(NumCores)(p)
      val debugIO = new DebugIO(p)
    })
    debug.module.io.tl_reset := reset
    debug.module.io.tl_clock := clock
    debug.module.io.hartIsInReset := io.resetCtrl.hartIsInReset
    resetCtrl.hartResetReq.foreach { rcio => debug.module.io.hartIsInReset.foreach { rcdm => rcio := rcdm }}

    debug.clockeddmi.foreach { dbg => debug.module.io.dmi.get <> dbg }
    debug.module.io.debug_reset := io.debugIO.reset
    debug.module.io.debug_clock := io.debugIO.clock
    io.debugIO.ndreset := debug.module.io.ctrl.ndreset
    io.debugIO.dmactive := debug.module.io.ctrl.dmactive
    debug.module.io.ctrl.dmactiveAck := io.debugIO.dmactiveAck
    io.debugIO.extTrigger.foreach { x => debug.module.io.extTrigger.foreach {y => x <> y}}

    val dtm = debug.flatMap(_.systemjtag.map(instantiateJtagDTM(_)))

    def instantiateJtagDTM(sj: SystemJTAGIO): DebugTransportModuleJTAG = {

      val dtm = Module(new DebugTransportModuleJTAG(p(DebugModuleKey).get.nDMIAddrSize, p(JtagDTMKey)))
      dtm.io.jtag <> sj.jtag

      io.debugIO.map(_.disableDebug.foreach { x => dtm.io.jtag.TMS := sj.jtag.TMS | x })  // force TMS high when debug is disabled

      dtm.io.jtag_clock  := sj.jtag.TCK
      dtm.io.jtag_reset  := sj.reset
      dtm.io.jtag_mfr_id := sj.mfr_id
      dtm.io.jtag_part_number := sj.part_number
      dtm.io.jtag_version := sj.version
      dtm.rf_reset := sj.reset
      debug.module.io.dmi.get.dmi <> dtm.io.dmi
      debug.module.io.dmi.get.dmiClock := sj.jtag.TCK
      debug.module.io.dmi.get.dmiReset := sj.reset
      dtm
    }
  }
}

class SimJTAG(tickDelay: Int = 50) extends BlackBox(Map("TICK_DELAY" -> IntParam(tickDelay)))
  with HasBlackBoxResource {
  val io = IO(new Bundle { 
    val clock = Input(Clock())
    val reset = Input(Bool())
    val jtag = new JTAGIO(hasTRSTn = true)
    val enable = Input(Bool())
    val init_done = Input(Bool())
    val exit = Output(UInt(32.W))
  })

  def connect(dutio: JTAGIO, tbclock: Clock, tbreset: Bool, init_done: Bool, tbsuccess: Bool) = {
    dutio.TCK := io.jtag.TCK
    dutio.TMS := io.jtag.TMS
    dutio.TDI := io.jtag.TDI
    io.jtag.TDO := dutio.TDO

    io.clock := tbclock
    io.reset := tbreset

    io.enable    := PlusArg("jtag_rbb_enable", 0, "Enable SimJTAG for JTAG Connections. Simulation will pause until connection is made.")
    io.init_done := init_done

    // Success is determined by the gdbserver
    // which is controlling this simulation.
    tbsuccess := io.exit === 1.U
    when (io.exit >= 2.U) {
      printf("*** FAILED *** (exit code = %d)\n", io.exit >> 1.U)
      stop(1)
    }
  }

  addResource("/vsrc/SimJTAG.v")
  addResource("/csrc/SimJTAG.cc")
  addResource("/csrc/remote_bitbang.h")
  addResource("/csrc/remote_bitbang.cc")
}

object Debug {
  def connectDebug(
      debugOpt: Option[DebugIO],
      resetctrlOpt: Option[ResetCtrlIO],
      psdio: PSDIO,
      c: Clock,
      r: Bool,
      out: Bool,
      tckHalfPeriod: Int = 2,
      cmdDelay: Int = 2,
      psd: PSDTestMode = 0.U.asTypeOf(new PSDTestMode()))
      (implicit p: Parameters): Unit =  {
    connectDebugClockAndReset(debugOpt, c)
    resetctrlOpt.map { rcio => rcio.hartIsInReset.map { _ := r }}
    debugOpt.map { debug =>
      debug.clockeddmi.foreach { d =>
        val dtm = Module(new SimDTM).connect(c, r, d, out)
      }
      debug.systemjtag.foreach { sj =>
        val jtag = Module(new SimJTAG(tickDelay=3)).connect(sj.jtag, c, r, ~r, out)
        sj.reset := r.asAsyncReset
        sj.mfr_id := p(JtagDTMKey).idcodeManufId.U(11.W)
        sj.part_number := p(JtagDTMKey).idcodePartNum.U(16.W)
        sj.version := p(JtagDTMKey).idcodeVersion.U(4.W)
      }
      debug.apb.foreach { apb =>
        require(false, "No support for connectDebug for an APB debug connection.")
      }
      psdio.psd.foreach { _ <> psd }
      debug.disableDebug.foreach { x => x := false.B }
    }
  }

  def connectDebugClockAndReset(debugOpt: Option[DebugIO], c: Clock, sync: Boolean = true)(implicit p: Parameters): Unit = {
    debugOpt.foreach { debug =>
      val dmi_reset = debug.clockeddmi.map(_.dmiReset.asBool).getOrElse(false.B) |
        debug.systemjtag.map(_.reset.asBool).getOrElse(false.B) |
        debug.apb.map(_.reset.asBool).getOrElse(false.B)
      connectDebugClockHelper(debug, dmi_reset, c, sync)
    }
  }

  def connectDebugClockHelper(debug: DebugIO, dmi_reset: Reset, c: Clock, sync: Boolean = true)(implicit p: Parameters): Unit = {
    val debug_reset = Wire(Bool())
    withClockAndReset(c, dmi_reset) {
      val debug_reset_syncd = if(sync) ~AsyncResetSynchronizerShiftReg(in=true.B, sync=3, name=Some("debug_reset_sync")) else dmi_reset
      debug_reset := debug_reset_syncd
    }
    // Need to clock DM during debug_reset because of synchronous reset, so keep
    // the clock alive for one cycle after debug_reset asserts to action this behavior.
    // The unit should also be clocked when dmactive is high.
    withClockAndReset(c, debug_reset.asAsyncReset) {
      val dmactiveAck = if (sync) ResetSynchronizerShiftReg(in=debug.dmactive, sync=3, name=Some("dmactiveAck")) else debug.dmactive
      val clock_en = RegNext(next=dmactiveAck, init=true.B)
      val gated_clock =
        if (!p(DebugModuleKey).get.clockGate) c
        else ClockGate(c, clock_en, "debug_clock_gate")
      debug.clock := gated_clock
      debug.reset := (if (p(SubsystemResetSchemeKey)==ResetSynchronous) debug_reset else debug_reset.asAsyncReset)
      debug.dmactiveAck := dmactiveAck
    }
  }

  def tieoffDebug(debugOpt: Option[DebugIO], resetctrlOpt: Option[ResetCtrlIO] = None, psdio: Option[PSDIO] = None)(implicit p: Parameters): Bool = {

    psdio.foreach(_.psd.foreach { _ <> 0.U.asTypeOf(new PSDTestMode()) } )
    resetctrlOpt.map { rcio => rcio.hartIsInReset.map { _ := false.B }}
    debugOpt.map { debug =>
      debug.clock := true.B.asClock
      debug.reset := (if (p(SubsystemResetSchemeKey)==ResetSynchronous) true.B else true.B.asAsyncReset)

      debug.systemjtag.foreach { sj =>
        sj.jtag.TCK := true.B.asClock
        sj.jtag.TMS := true.B
        sj.jtag.TDI := true.B
        sj.jtag.TRSTn.foreach { r => r := true.B }
        sj.reset := true.B.asAsyncReset
        sj.mfr_id := 0.U
        sj.part_number := 0.U
        sj.version := 0.U
      }

      debug.clockeddmi.foreach { d =>
        d.dmi.req.valid := false.B
        d.dmi.resp.ready := true.B
        d.dmiClock := false.B.asClock
        d.dmiReset := true.B.asAsyncReset
      }

      debug.apb.foreach { apb =>
        apb.tieoff()
        apb.clock := false.B.asClock
        apb.reset := true.B.asAsyncReset
        apb.psel := false.B
        apb.penable := false.B
      }

      debug.extTrigger.foreach { t =>
        t.in.req := false.B
        t.out.ack := t.out.req
      }
      debug.disableDebug.foreach { x => x := false.B }
      debug.ndreset
    }.getOrElse(false.B)
  }
}