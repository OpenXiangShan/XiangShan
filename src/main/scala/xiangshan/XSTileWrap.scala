/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan

import chisel3._
import chisel3.util._
import freechips.rocketchip.devices.debug
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import system.HasSoCParameter
import coupledL2.tl2chi.{AsyncPortIO, CHIAsyncBridgeSource, PortIO}
import utility.sram.SramBroadcastBundle
import utility.{DFTResetSignals, IntBuffer, ResetGen}
import freechips.rocketchip.devices.debug.{ClockedDMIIO, DebugExtTriggerIO, DebugModuleKey, ExportDebug, TLDebugModule}
import xiangshan.backend.trace.TraceCoreInterface
import device.{CHIAsyncDEVCJ, CHIAsyncIOCJ, ClintAsyncCJ, DebugModule, IMSICMapCJ, LpIOCJ}
import utils.PowerSwitchBuffer

// This module is used for XSNoCTop for async time domain and divide different
// voltage domain. Everything in this module should be in the core clock domain
// and higher voltage domain.
class XSTileWrap()(implicit p: Parameters) extends LazyModule
    with HasXSParameter
    with HasSoCParameter {
  override def shouldBeInlined: Boolean = false
  val tile = LazyModule(new XSTile())
  // seperate DebugModule bus
  val EnableDMAsync = EnableSeperateTLAsync
  // interrupts sync
  val DMTnTopSync = UseDMInTop && (!EnableDMAsync) //dm integreted in xstop,and sync with cpu for mmio cfg.
  val clintIntNode = IntIdentityNode()
  val debugIntNode = Option.when(!DMTnTopSync)(IntIdentityNode()) // from standalone or xstop
  val debugIntNodeSync = Option.when(DMTnTopSync)(IntIdentityNode()) // xstilewrap
  val plicIntNode = IntIdentityNode()
  val beuIntNode = IntIdentityNode()
  val nmiIntNode = IntIdentityNode()
  tile.clint_int_node := IntBuffer(3, cdc = true) := clintIntNode
  tile.plic_int_node :*= IntBuffer(3, cdc = true) :*= plicIntNode
  tile.nmi_int_node := IntBuffer(3, cdc = true) := nmiIntNode
  beuIntNode := IntBuffer() := tile.beu_int_source
  // seperate TL bus
  println(s"SeperateTLBus = $SeperateTLBus")
  println(s"EnableSeperateTLAsync = $EnableSeperateTLAsync")
  // asynchronous bridge source node
  val tlAsyncSourceOpt = Option.when(SeperateTLBus && EnableSeperateTLAsync)(LazyModule(new TLAsyncCrossingSource()))
  tlAsyncSourceOpt.foreach(_.node := tile.sep_tl_opt.get)
  // synchronous source node
  val tlSyncSourceOpt = Option.when(SeperateTLBus && !EnableSeperateTLAsync && (!UseDMInTop))(TLTempNode())
  tlSyncSourceOpt.foreach(_ := tile.sep_tl_opt.get)
  val l_debugModule = Option.when(DMTnTopSync)(LazyModule(new DebugModule(numCores = NumCores)(p)))
  // instance
  val dmXbar = Option.when(DMTnTopSync)(TLXbar())
  l_debugModule.foreach(_.debug.node := dmXbar.get)
  dmXbar.foreach(_ := tile.sep_tl_opt.get)
  // interrupts
//  val o_debug_int = InModuleBody(debugModuleIntNode.map(_.makeIOs()))
  DMTnTopSync match{
    case(false) =>
      tile.debug_int_node := IntBuffer(3, cdc = true) := debugIntNode.get
    case(true) =>
      debugIntNodeSync.get := l_debugModule.get.debug.dmOuter.dmOuter.intnode
      tile.debug_int_node := IntBuffer(3, cdc = true) := debugIntNodeSync.get
  }
  class XSTileWrapImp(wrapper: LazyModule) extends LazyRawModuleImp(wrapper) {
    val clock = IO(Input(Clock()))
    val reset = IO(Input(AsyncReset()))
    val noc_reset = EnableCHIAsyncBridge.map(_ => IO(Input(AsyncReset())))
    val soc_reset = Option.when(!ClintAsyncFromCJ || (SeperateTLBus && EnableSeperateTLAsync))(IO(Input(AsyncReset())))
    val i = Option.when(CHIAsyncFromCJ)(IO(new Bundle { // for customer J
      val dft = Input(new Bundle {
        val icg_scan_en = Bool()
        val scan_enable = Bool()
      })
    }))
    val io = IO(new Bundle {
      val hartId = Input(UInt(hartIdLen.W))
      val msiInfo = Input(ValidIO(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W)))
      val msiAck = Output(Bool())
      val reset_vector = Input(UInt(PAddrBits.W))
      val cpu_halt = Output(Bool())
      val cpu_crtical_error = Output(Bool())
      // ==UseDMInTop start: 1- DebugModule is integrated in XSTOP, only customer J need it,other 0.==
      val hartResetReq = Option.when(!DMTnTopSync)(Input(Bool()))
      val hartIsInReset = Option.when(!DMTnTopSync)(Output(Bool()))
      val dm = Option.when(DMTnTopSync)(new Bundle {
//        val dmi = (!p(ExportDebug).apb && p(ExportDebug).dmi).option(Flipped(new ClockedDMIIO()))
        val dmi = Flipped(new ClockedDMIIO())
        val extTrigger = (p(DebugModuleKey).get.nExtTriggers > 0).option(new DebugExtTriggerIO())
        val ndreset = Output(Bool()) // output of top,to request that soc can reset system logic exclude debug.
        val debugIntrSync = Output (Bool())
      })
      val cpulp = Option.when(EnableCHIAsyncBridge.isDefined && CHIAsyncFromCJ)(new LpIOCJ)
      // ==UseDMInTop end ==
      val traceCoreInterface = new TraceCoreInterface
      val debugTopDown = new Bundle {
        val robHeadPaddr = Valid(UInt(PAddrBits.W))
        val l3MissMatch = Input(Bool())
      }

      val l3Miss = Input(Bool())
      val chi = (EnableCHIAsyncBridge, CHIAsyncFromCJ) match {
        case (Some(param), true) => Flipped(new CHIAsyncIOCJ())
        case (Some(param), false) =>new AsyncPortIO(param)
        case _ => new PortIO
      }
      val nodeID = if (enableCHI) Some(Input(UInt(NodeIDWidth.W))) else None
      val clintTime = EnableClintAsyncBridge match {
        case Some(param) =>
          if (ClintAsyncFromCJ) Input(ValidIO(UInt(64.W))) else Flipped(new AsyncBundle(UInt(64.W), param))
        case None => Input(ValidIO(UInt(64.W)))
      }
      val dft = Option.when(hasDFT)(Input(new SramBroadcastBundle))
      val dft_reset = Option.when(hasMbist)(Input(new DFTResetSignals()))
      val l2_flush_en = Option.when(EnablePowerDown) (Output(Bool()))
      val l2_flush_done = Option.when(EnablePowerDown) (Output(Bool()))
      val pwrdown_req_n = Option.when(EnablePowerDown) (Input (Bool()))
      val pwrdown_ack_n = Option.when(EnablePowerDown) (Output (Bool()))
      val iso_en = Option.when(EnablePowerDown) (Input (Bool()))
    })
    val debugModule = l_debugModule.map(_.module)
    val hartResetReq = WireDefault(Bool(),false.B) // derive from io.hartResetReq or debugwrapper in top
    io.hartResetReq.foreach(iohartResetReq => hartResetReq := iohartResetReq)
    debugModule.foreach{debugModule => hartResetReq := debugModule.io.resetCtrl.hartResetReq.get.head}
    val reset_sync = withClockAndReset(clock, (reset.asBool || hartResetReq).asAsyncReset)(ResetGen(io.dft_reset))
    val noc_reset_sync = Wire(AsyncReset())
    (EnableCHIAsyncBridge, CHIAsyncFromCJ) match {
      case (Some(param), false) =>
        noc_reset_sync := withClockAndReset(clock, noc_reset.get)(ResetGen(io.dft_reset))
      case (Some(param), true) =>
        noc_reset_sync := reset_sync
      case (None, _) =>
        noc_reset_sync := false.asBool.asAsyncReset
    }
    val soc_reset_sync = Option.when(!ClintAsyncFromCJ | EnableDMAsync)(withClockAndReset(clock, soc_reset.get) { ResetGen(io.dft_reset) })
    val imsicMapCJ = (soc.IMSICBusType == device.IMSICBusType.NONE) && ClintAsyncFromCJ && CHIAsyncFromCJ
    val imsicMap = Option.when(imsicMapCJ)(withClockAndReset(clock, reset_sync)(Module(new IMSICMapCJ())))
    imsicMap.foreach(_.io.i_msi := io.msiInfo)
    // override LazyRawModuleImp's clock and reset
    childClock := clock
    childReset := reset_sync
    tile.module.io.hartId := io.hartId
    // connect msi info io with xstile
    when(imsicMapCJ.B){
      imsicMap.foreach(tile.module.io.msiInfo <> _.io.o_msi)
    }.otherwise {
      tile.module.io.msiInfo := io.msiInfo
    }
    io.msiAck := tile.module.io.msiAck
    tile.module.io.reset_vector := io.reset_vector
    tile.module.io.dft.zip(io.dft).foreach({ case (a, b) => a := b })
    tile.module.io.dft_reset.zip(io.dft_reset).foreach({ case (a, b) => a := b })
    io.cpu_halt := tile.module.io.cpu_halt
    io.cpu_crtical_error := tile.module.io.cpu_crtical_error
    io.hartIsInReset.foreach(_ := tile.module.io.hartIsInReset)
    io.traceCoreInterface <> tile.module.io.traceCoreInterface
    io.debugTopDown <> tile.module.io.debugTopDown
    tile.module.io.l3Miss := io.l3Miss
    tile.module.io.nodeID.foreach(_ := io.nodeID.get)
    io.l2_flush_en.foreach { _ := tile.module.io.l2_flush_en.getOrElse(false.B) }
    io.l2_flush_done.foreach { _ := tile.module.io.l2_flush_done.getOrElse(false.B) }
    io.pwrdown_ack_n.foreach { _ := DontCare }
    io.pwrdown_ack_n zip io.pwrdown_req_n foreach { case (ack, req) =>
      val powerSwitchBuffer = Module(new PowerSwitchBuffer)
      ack := powerSwitchBuffer.ack
      powerSwitchBuffer.sleep := req
    }

    // instance :TL DebugModule
    debugModule.foreach { debugModule =>
      debugModule.io.reset := reset_sync
      debugModule.io.clock := clock
      debugModule.io.debugIO.clock := clock
      debugModule.io.debugIO.reset := reset_sync
      withClockAndReset(debugModule.io.clock, debugModule.io.reset.asAsyncReset) {
        debugModule.io.resetCtrl.hartIsInReset.head := AsyncResetSynchronizerShiftReg(tile.module.io.hartIsInReset, 3, 0)
      }
      val dmactiveAck = debugModule.io.debugIO.dmactive
      debugModule.io.debugIO.dmactiveAck      := dmactiveAck
      // theroretically,ExportDebug => DebugAttachParams(protocols = Set(DMI)),no set jtag
//      debugModule.io.debugIO.systemjtag.map(_<> DontCare)
      //debugModule.io.debugIO.extTrigger.map(_<>)
      io.dm.foreach { iodm =>
        iodm.ndreset := debugModule.io.debugIO.ndreset
        iodm.dmi.dmi <> debugModule.io.debugIO.clockeddmi.get.dmi
        debugModule.io.debugIO.clockeddmi.get.dmiClock := iodm.dmi.dmiClock
        debugModule.io.debugIO.clockeddmi.get.dmiReset := withClockAndReset(iodm.dmi.dmiClock, iodm.dmi.dmiReset.asAsyncReset) {
          ResetGen(io.dft_reset)
        }
        iodm.extTrigger.foreach(_ <> debugModule.io.debugIO.extTrigger.get)
        iodm.debugIntrSync := debugIntNodeSync.get.out.head._1(0)
      }
    }
    // CLINT Async Queue Sink
    (EnableClintAsyncBridge, ClintAsyncFromCJ) match {
      case (Some(param), false) =>
        val sink = withClockAndReset(clock, soc_reset_sync.get)(Module(new AsyncQueueSink(UInt(64.W), param)))
        sink.io.async <> io.clintTime
        sink.io.deq.ready := true.B
        tile.module.io.clintTime.valid := sink.io.deq.valid
        tile.module.io.clintTime.bits := sink.io.deq.bits
      case (Some(param), true) =>   //clint async proc ip is from customer J
        val sink = withClockAndReset(clock, reset_sync)(Module(new ClintAsyncCJ()))  //use cpu reset
        sink.io.i_time <> io.clintTime
        tile.module.io.clintTime := sink.io.o_time
      case _ =>
        tile.module.io.clintTime := io.clintTime
    }
    // CHI Async Queue Source
    (EnableCHIAsyncBridge, CHIAsyncFromCJ) match {
      case (Some(param), true) => // chiasync bridge can be provided by customer J.
        val source = withClockAndReset(clock, noc_reset_sync)(Module(new CHIAsyncDEVCJ()))
        source.i.dft.icg_scan_en := i.get.dft.icg_scan_en
        source.i.dft.scan_enable := i.get.dft.scan_enable
        source.io.chi <> tile.module.io.chi.get
        source.io.cpulp <> io.cpulp.get
        io.chi <> source.io.cdb
      case (Some(param), false) =>
        val source = withClockAndReset(clock, noc_reset_sync)(Module(new CHIAsyncBridgeSource(param)))
        source.io.enq <> tile.module.io.chi.get
        io.chi <> source.io.async
      case _ =>
        require(enableCHI)
        io.chi <> tile.module.io.chi.get
    }
    // Seperate DebugModule TL Async Queue Source
    if (SeperateTLBus && EnableSeperateTLAsync) {
      tlAsyncSourceOpt.get.module.clock := clock
      tlAsyncSourceOpt.get.module.reset := soc_reset_sync.get
    }
    withClockAndReset(clock, reset_sync) {
      // Modules are reset one by one
      // reset ----> SYNC --> XSTile
      val resetChain = Seq(Seq(tile.module))
      ResetGen(resetChain, reset_sync, !debugOpts.FPGAPlatform, io.dft_reset)
    }
    dontTouch(io.hartId)
    dontTouch(io.msiInfo)
    io.pwrdown_req_n.foreach(dontTouch(_))
    io.pwrdown_ack_n.foreach(dontTouch(_))
    io.iso_en.foreach(dontTouch(_))
  }
  lazy val module = new XSTileWrapImp(this)
}
