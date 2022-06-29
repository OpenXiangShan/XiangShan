/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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
import chisel3.experimental.{ExtModule, IntParam, noPrefix}
import chisel3.util._
import chisel3.util.HasExtModuleResource
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.jtag._
import freechips.rocketchip.util._
import freechips.rocketchip.prci.{ClockSinkNode, ClockSinkParameters}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.debug.{DebugCustomXbar, DebugIO, DebugTransportModuleJTAG, JtagDTMConfig, PSDIO, ResetCtrlIO, SystemJTAGIO, TLDebugModule}
import freechips.rocketchip.devices.debug._

// this file uses code from rocketchip Periphery.scala
// to simplify the code we remove options for apb, cjtag and dmi
// this module creates wrapped dm and dtm


class DebugModule(numCores: Int)(implicit p: Parameters) extends LazyModule {

  val debug = LazyModule(new TLDebugModule(8)(p))

//  debug.node := TLFragmenter() := peripheralXbar
  val debugCustomXbarOpt = p(DebugModuleKey).map(params => LazyModule( new DebugCustomXbar(outputRequiresInput = false)))
  debug.dmInner.dmInner.customNode := debugCustomXbarOpt.get.node

//  debug.dmInner.dmInner.sb2tlOpt.foreach { sb2tl  =>
//    l2xbar := TLBuffer() := TLWidthWidget(1) := sb2tl.node
//  }

  lazy val module = new LazyRawModuleImp(this) {
    val io = IO(new Bundle{
      val resetCtrl = new ResetCtrlIO(numCores)(p)
      val debugIO = new DebugIO()(p)
      val clock = Input(Bool())
      val reset = Input(Reset())
    })
    debug.module.io.tl_reset := io.reset // this should be TL reset
    debug.module.io.tl_clock := io.clock.asClock // this should be TL clock
    debug.module.io.hartIsInReset := io.resetCtrl.hartIsInReset
    io.resetCtrl.hartResetReq.foreach { rcio => debug.module.io.hartResetReq.foreach { rcdm => rcio := rcdm }}

    io.debugIO.clockeddmi.foreach { dbg => debug.module.io.dmi.get <> dbg } // not connected in current case since we use dtm
    debug.module.io.debug_reset := io.debugIO.reset
    debug.module.io.debug_clock := io.debugIO.clock
    io.debugIO.ndreset := debug.module.io.ctrl.ndreset
    io.debugIO.dmactive := debug.module.io.ctrl.dmactive
    debug.module.io.ctrl.dmactiveAck := io.debugIO.dmactiveAck
    io.debugIO.extTrigger.foreach { x => debug.module.io.extTrigger.foreach {y => x <> y}}
    debug.module.io.ctrl.debugUnavail.foreach { _ := false.B }

    val dtm = io.debugIO.systemjtag.map(instantiateJtagDTM(_))

    def instantiateJtagDTM(sj: SystemJTAGIO): DebugTransportModuleJTAG = {
      val c = new JtagDTMKeyDefault
      val dtm = Module(new DebugTransportModuleJTAG(p(DebugModuleKey).get.nDMIAddrSize, c))
      dtm.io.jtag <> sj.jtag

      io.debugIO.disableDebug.foreach { x => dtm.io.jtag.TMS := sj.jtag.TMS | x }  // force TMS high when debug is disabled

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

object XSDebugModuleParams {

  def apply(xlen:Int /*TODO , val configStringAddr: Int*/): DebugModuleParams = {
    new DebugModuleParams().copy(
      nAbstractDataWords   = (if (xlen == 32) 1 else if (xlen == 64) 2 else 4),
      maxSupportedSBAccess = xlen,
      hasBusMaster = true,
      baseAddress = BigInt(0x1f10020000L),
      nScratch = 2
    )
  }
}

case object EnableJtag extends Field[Bool]

class SimJTAG(tickDelay: Int = 50)(implicit val p: Parameters) extends ExtModule(Map("TICK_DELAY" -> IntParam(tickDelay)))
  with HasExtModuleResource {

  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val jtag = IO(new JTAGIO(hasTRSTn = true))
  val enable = IO(Input(Bool()))
  val init_done = IO(Input(Bool()))
  val exit = IO(Output(UInt(32.W)))

  def connect(dutio: JTAGIO, tbclock: Clock, tbreset: Reset, done: Bool, tbsuccess: Bool) = {
    dutio.TCK := jtag.TCK
    dutio.TMS := jtag.TMS
    dutio.TDI := jtag.TDI
    jtag.TDO := dutio.TDO

    clock := tbclock
    reset := tbreset

    enable    := p(EnableJtag)
    init_done := done

    // Success is determined by the gdbserver
    // which is controlling this simulation.
    tbsuccess := exit === 1.U
    when (exit >= 2.U) {
      printf("*** FAILED *** (exit code = %d)\n", exit >> 1.U)
      stop(1)
    }
  }
}
