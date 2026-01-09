/***************************************************************************************
* Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2024-2025 Institute of Computing Technology, Chinese Academy of Sciences
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
import device.{TIMER, TIMERParams, TimeAsync}
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import system.HasSoCParameter
import coupledL2.tl2chi.{AsyncPortIO, CHIAsyncBridgeSource, PortIO}
import utility.sram.SramBroadcastBundle
import utility.{DFTResetSignals, IntBuffer, ResetGen}
import xiangshan.backend.trace.TraceCoreInterface
import utils.PowerSwitchBuffer

// This module is used for XSNoCTop for async time domain and divide different
// voltage domain. Everything in this module should be in the core clock domain
// and higher voltage domain.
class XSTileWrap()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter
{
  override def shouldBeInlined: Boolean = false

  val tile = LazyModule(new XSTile())

  // interrupts sync
  val clintIntNode = Option.when(!UsePrivateClint)(IntIdentityNode()) // interrupt from CHI or (TL & async)
  val debugIntNode = IntIdentityNode()
  val plicIntNode = IntIdentityNode()
  val beuIntNode = IntIdentityNode()
  val nmiIntNode = IntIdentityNode()
  // instance clint timer
  val timer = Option.when(UsePrivateClint)(LazyModule(new TIMER(TIMERParams(IsSelfTest=false,soc.TIMERRange.base), 8))) // TL & sync
  tile.clint_int_node := IntBuffer(3, cdc = true) := clintIntNode.getOrElse(timer.get.intnode)
  tile.debug_int_node := IntBuffer(3, cdc = true) := debugIntNode
  tile.plic_int_node :*= IntBuffer(3, cdc = true) :*= plicIntNode
  tile.nmi_int_node := IntBuffer(3, cdc = true) := nmiIntNode
  beuIntNode := IntBuffer() := tile.beu_int_source

  // seperate TL bus
  println(s"SeperateBus = $SeperateBus")
  val tlXbar = Option.when(SeperateBus != top.SeperatedBusType.NONE)(TLXbar())
  tlXbar.map(_ := tile.sep_tl_opt.get) // TLXbar node in connect with tile master
  timer.map(_.node := tlXbar.get) // TLXbar node out connnect with timer mmio
  // asynchronous bridge source node
  val tlAsyncSourceOpt = Option.when(SeperateBus != top.SeperatedBusType.NONE)(LazyModule(new TLAsyncCrossingSource()))
  tlAsyncSourceOpt.foreach(_.node := tlXbar.get)

  class XSTileWrapImp(wrapper: LazyModule) extends LazyRawModuleImp(wrapper) {
    val clock = IO(Input(Clock()))
    val reset = IO(Input(AsyncReset()))
    val noc_reset = EnableCHIAsyncBridge.map(_ => IO(Input(AsyncReset())))
    val soc_reset = IO(Input(AsyncReset()))
    val io = IO(new Bundle {
      val hartId = Input(UInt(hartIdLen.W))
      val msiInfo = Input(ValidIO(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W)))
      val msiAck = Output(Bool())
      val teemsiInfo = Option.when(soc.IMSICParams.HasTEEIMSIC)(Input(ValidIO(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W))))
      val teemsiAck = Option.when(soc.IMSICParams.HasTEEIMSIC)(Output(Bool()))
      val reset_vector = Input(UInt(PAddrBits.W))
      val cpu_halt = Output(Bool())
      val cpu_crtical_error = Output(Bool())
      val hartResetReq = Input(Bool())
      val hartIsInReset = Output(Bool())
      val traceCoreInterface = new TraceCoreInterface
      val debugTopDown = new Bundle {
        val robHeadPaddr = Valid(UInt(PAddrBits.W))
        val l3MissMatch = Input(Bool())
      }
      val l3Miss = Input(Bool())
      val chi = EnableCHIAsyncBridge match {
        case Some(param) => new AsyncPortIO(param)
        case None => new PortIO
      }
      val nodeID = if (enableCHI) Some(Input(UInt(NodeIDWidth.W))) else None
      val clintTime = EnableClintAsyncBridge match {
        case Some(param) => Flipped(new AsyncBundle(UInt(64.W), param))
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

    val reset_sync = withClockAndReset(clock, (reset.asBool || io.hartResetReq).asAsyncReset)(ResetGen(io.dft_reset))
    val noc_reset_sync = EnableCHIAsyncBridge.map(_ => withClockAndReset(clock, noc_reset.get)(ResetGen(io.dft_reset)))
    val soc_reset_sync = withClockAndReset(clock, soc_reset)(ResetGen(io.dft_reset))

    // override LazyRawModuleImp's clock and reset
    childClock := clock
    childReset := reset_sync

    timer.foreach { timerInst =>
      timerInst.module.io.hartId := io.hartId
      timerInst.module.io.time := tile.module.io.clintTime
    }
    // sync about msiinfo
    val msi_vld_cpu = WireInit(false.B)
    val msi_data_cpu = WireInit(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W),0.U)
    withClockAndReset(clock, reset_sync) {
      val msi_vld_sync = AsyncResetSynchronizerShiftReg(io.msiInfo.valid, 3, 0)
      msi_vld_cpu := RegNext(msi_vld_sync)
      msi_data_cpu := RegEnable(io.msiInfo.bits, msi_vld_sync)
    }
    tile.module.io.hartId := io.hartId
    tile.module.io.msiInfo.valid := msi_vld_cpu
    tile.module.io.msiInfo.bits := msi_data_cpu

    tile.module.io.teemsiInfo zip io.teemsiInfo foreach { case (tile_teemsiInfo, io_teemsiInfo) =>
      // sync about msiinfo
      val teemsi_vld_cpu = WireInit(false.B)
      val teemsi_data_cpu = WireInit(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W),0.U)
      withClockAndReset(clock, reset_sync) {
        val teemsi_vld_sync = AsyncResetSynchronizerShiftReg(io_teemsiInfo.valid, 3, 0)
        teemsi_vld_cpu := RegNext(teemsi_vld_sync)
        teemsi_data_cpu := RegEnable(io_teemsiInfo.bits, teemsi_vld_sync)
      }
      tile_teemsiInfo.valid := teemsi_vld_cpu
      tile_teemsiInfo.bits := teemsi_data_cpu
    }
    tile.module.io.reset_vector := io.reset_vector
    tile.module.io.dft.zip(io.dft).foreach({ case (a, b) => a := b })
    tile.module.io.dft_reset.zip(io.dft_reset).foreach({ case (a, b) => a := b })
    io.cpu_halt := tile.module.io.cpu_halt
    io.cpu_crtical_error := tile.module.io.cpu_crtical_error
    io.msiAck := tile.module.io.msiAck
    io.teemsiAck zip tile.module.io.teemsiAck foreach { case (io_teemsiAck, tile_teemsiAck) =>
      io_teemsiAck := tile_teemsiAck
    }
    io.hartIsInReset := tile.module.io.hartIsInReset
    withClockAndReset(clock, reset_sync) {
      tile.module.io.traceCoreInterface.fromEncoder.enable := 
        AsyncResetSynchronizerShiftReg(io.traceCoreInterface.fromEncoder.enable, 3, 0)
      tile.module.io.traceCoreInterface.fromEncoder.stall := 
        AsyncResetSynchronizerShiftReg(io.traceCoreInterface.fromEncoder.stall, 3, 0)
    }
    io.traceCoreInterface.toEncoder := tile.module.io.traceCoreInterface.toEncoder
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

    // CLINT Async Queue Sink
    EnableClintAsyncBridge match {
      case Some(param) =>
        val time_sink = withClockAndReset(clock, reset_sync)(Module(new AsyncQueueSink(UInt(64.W), param)))
        time_sink.io.async <> io.clintTime
        time_sink.io.deq.ready := true.B
        tile.module.io.clintTime.valid := time_sink.io.deq.valid
        tile.module.io.clintTime.bits := time_sink.io.deq.bits
      case None =>
        tile.module.io.clintTime := io.clintTime
    }

    // CHI Async Queue Source
    EnableCHIAsyncBridge match {
      case Some(param) =>
        val source = withClockAndReset(clock, reset_sync)(Module(new CHIAsyncBridgeSource(param)))
        source.io.enq <> tile.module.io.chi.get
        io.chi <> source.io.async
      case None =>
        require(enableCHI)
        io.chi <> tile.module.io.chi.get
    }

    withClockAndReset(clock, reset_sync) {
      // Modules are reset one by one
      // reset ----> SYNC --> XSTile
      val resetChain = Seq(Seq(tile.module))
      ResetGen(resetChain, reset_sync, !debugOpts.FPGAPlatform, io.dft_reset)
    }
    dontTouch(io.hartId)
    dontTouch(io.msiInfo)
    io.teemsiInfo.foreach(dontTouch(_))
    io.pwrdown_req_n.foreach(dontTouch(_))
    io.pwrdown_ack_n.foreach(dontTouch(_))
    io.iso_en.foreach(dontTouch(_))
  }
  lazy val module = new XSTileWrapImp(this)
}
