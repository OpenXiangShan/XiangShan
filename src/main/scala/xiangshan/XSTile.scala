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

package xiangshan

import org.chipsalliance.cde.config.{Config, Parameters}
import chisel3._
import chisel3.util.{Valid, ValidIO, log2Up}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, BusErrors}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import system.HasSoCParameter
import top.{ArgParser, BusPerfMonitor, Generator}
import utility.{ChiselDB, Constantin, DFTResetSignals, DelayN, FileRegisters, ResetGen, TLClientsMerger, TLEdgeBuffer, TLLogger}
import utility.sram.SramBroadcastBundle
import coupledL2.EnableCHI
import coupledL2.tl2chi.PortIO
import xiangshan.backend.trace.TraceCoreInterface

class XSTile()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter
{
  override def shouldBeInlined: Boolean = false
  val core = LazyModule(new XSCore())
  val l2top = LazyModule(new L2Top())

  val enableL2 = coreParams.L2CacheParamsOpt.isDefined
  // =========== Public Ports ============
  val memBlock = core.memBlock.inner
  val core_l3_pf_port = memBlock.l3_pf_sender_opt
  val memory_port = if (enableCHI && enableL2) None else Some(l2top.inner.memory_port.get)
  val tl_uncache = l2top.inner.mmio_port
  val sep_tl_opt = l2top.inner.sep_tl_port_opt
  val beu_int_source = l2top.inner.beu.intNode
  val core_reset_sink = BundleBridgeSink(Some(() => Reset()))
  val clint_int_node = l2top.inner.clint_int_node
  val plic_int_node = l2top.inner.plic_int_node
  val debug_int_node = l2top.inner.debug_int_node
  val nmi_int_node = l2top.inner.nmi_int_node
  memBlock.clint_int_sink := clint_int_node
  memBlock.plic_int_sink :*= plic_int_node
  memBlock.debug_int_sink := debug_int_node
  memBlock.nmi_int_sink := nmi_int_node
  memBlock.beu_local_int_sink := l2top.inner.beu_local_int_source_buffer

  // =========== Components' Connection ============
  // L1 to l1_xbar
  coreParams.dcacheParametersOpt.map { _ =>
    l2top.inner.misc_l2_pmu := l2top.inner.l1d_logger := memBlock.dcache_port :=
      memBlock.l1d_to_l2_buffer.node := memBlock.dcache.clientNode
  }

  l2top.inner.misc_l2_pmu := l2top.inner.l1i_logger := memBlock.frontendBridge.icache_node
  if (!coreParams.softPTW) {
    l2top.inner.misc_l2_pmu := l2top.inner.ptw_logger := l2top.inner.ptw_to_l2_buffer.node := memBlock.ptw_to_l2_buffer.node
  }

  // L2 Prefetch
  l2top.inner.l2cache match {
    case Some(l2) =>
      l2.pf_recv_node.foreach(recv => {
        println("Connecting L1 prefetcher to L2!")
        recv := memBlock.l2_pf_sender_opt.get
      })
    case None =>
  }

  val core_l3_tpmeta_source_port = l2top.inner.l2cache match {
    case Some(l2) => l2.tpmeta_source_node
    case None => None
  }
  val core_l3_tpmeta_sink_port = l2top.inner.l2cache match {
    case Some(l2) => l2.tpmeta_sink_node
    case None => None
  }

  // mmio
  l2top.inner.i_mmio_port := l2top.inner.i_mmio_buffer.node := memBlock.frontendBridge.instr_uncache_node
  if (icacheParameters.cacheCtrlAddressOpt.nonEmpty) {
    memBlock.frontendBridge.icachectrl_node := l2top.inner.icachectrl_port_opt.get
  }
  l2top.inner.d_mmio_port := memBlock.uncache_port

  // =========== IO Connection ============
  class XSTileImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val io = IO(new Bundle {
      val hartId = Input(UInt(hartIdLen.W))
      val msiInfo = Input(ValidIO(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W)))
      val msiAck = Output(Bool())
      val reset_vector = Input(UInt(PAddrBits.W))
      val cpu_halt = Output(Bool())
      val cpu_crtical_error = Output(Bool())
      val hartIsInReset = Output(Bool())
      val traceCoreInterface = new TraceCoreInterface
      val debugTopDown = new Bundle {
        val robHeadPaddr = Valid(UInt(PAddrBits.W))
        val l3MissMatch = Input(Bool())
      }
      val l3Miss = Input(Bool())
      val chi = if (enableCHI) Some(new PortIO) else None
      val nodeID = if (enableCHI) Some(Input(UInt(NodeIDWidth.W))) else None
      val clintTime = Input(ValidIO(UInt(64.W)))
      val dft = Option.when(hasDFT)(Input(new SramBroadcastBundle))
      val dft_reset = Option.when(hasMbist)(Input(new DFTResetSignals()))
      val l2_flush_en = Option.when(EnablePowerDown) (Output(Bool()))
      val l2_flush_done = Option.when(EnablePowerDown) (Output(Bool()))
    })

    dontTouch(io.hartId)
    dontTouch(io.msiInfo)
    if (!io.chi.isEmpty) { dontTouch(io.chi.get) }

    val core_soft_rst = core_reset_sink.in.head._1 // unused

    l2top.module.io.hartId.fromTile := io.hartId
    core.module.io.hartId := l2top.module.io.hartId.toCore
    core.module.io.reset_vector := l2top.module.io.reset_vector.toCore
    core.module.io.msiInfo := l2top.module.io.msiInfo.toCore
    l2top.module.io.msiInfo.fromTile := io.msiInfo
    core.module.io.clintTime := l2top.module.io.clintTime.toCore
    l2top.module.io.clintTime.fromTile := io.clintTime
    l2top.module.io.reset_vector.fromTile := io.reset_vector
    l2top.module.io.cpu_halt.fromCore := core.module.io.cpu_halt
    io.cpu_halt := l2top.module.io.cpu_halt.toTile
    l2top.module.io.cpu_critical_error.fromCore := core.module.io.cpu_critical_error
    io.cpu_crtical_error := l2top.module.io.cpu_critical_error.toTile
    l2top.module.io.msiAck.fromCore := core.module.io.msiAck
    io.msiAck := l2top.module.io.msiAck.toTile

    l2top.module.io.hartIsInReset.resetInFrontend := core.module.io.resetInFrontend
    io.hartIsInReset := l2top.module.io.hartIsInReset.toTile
    l2top.module.io.traceCoreInterface.fromCore <> core.module.io.traceCoreInterface
    io.traceCoreInterface <> l2top.module.io.traceCoreInterface.toTile

    l2top.module.io.beu_errors.icache <> core.module.io.beu_errors.icache
    l2top.module.io.beu_errors.dcache <> core.module.io.beu_errors.dcache
    l2top.module.io.beu_errors.uncache <> core.module.io.beu_errors.uncache

    l2top.module.io.l2_flush_en.foreach { _ := core.module.io.l2_flush_en }
    io.l2_flush_en.foreach { _ := core.module.io.l2_flush_en }
    core.module.io.l2_flush_done := l2top.module.io.l2_flush_done.getOrElse(false.B)
    io.l2_flush_done.foreach { _ := l2top.module.io.l2_flush_done.getOrElse(false.B) }

    l2top.module.io.dft.zip(io.dft).foreach({ case (a, b) => a := b })
    l2top.module.io.dft_reset.zip(io.dft_reset).foreach({ case (a, b) => a := b })
    core.module.io.dft.zip(io.dft).foreach({ case (a, b) => a := b })
    core.module.io.dft_reset.zip(io.dft_reset).foreach({ case (a, b) => a := b })

    if (enableL2) {
      // TODO: add ECC interface of L2
      l2top.module.io.pfCtrlFromCore := core.module.io.l2PfCtrl

      l2top.module.io.beu_errors.l2 <> 0.U.asTypeOf(l2top.module.io.beu_errors.l2)
      core.module.io.l2_hint.bits.sourceId := l2top.module.io.l2_hint.bits.sourceId
      core.module.io.l2_hint.bits.isKeyword := l2top.module.io.l2_hint.bits.isKeyword
      core.module.io.l2_hint.valid := l2top.module.io.l2_hint.valid

      core.module.io.l2PfqBusy := false.B
      core.module.io.debugTopDown.l2MissMatch := l2top.module.io.debugTopDown.l2MissMatch
      l2top.module.io.debugTopDown.robHeadPaddr := core.module.io.debugTopDown.robHeadPaddr
      l2top.module.io.debugTopDown.robTrueCommit := core.module.io.debugTopDown.robTrueCommit
      l2top.module.io.l2_pmp_resp := core.module.io.l2_pmp_resp
      core.module.io.l2_tlb_req <> l2top.module.io.l2_tlb_req
      core.module.io.topDownInfo.l2Miss := l2top.module.io.l2Miss

      core.module.io.perfEvents <> l2top.module.io.perfEvents
    } else {

      l2top.module.io.beu_errors.l2 <> 0.U.asTypeOf(l2top.module.io.beu_errors.l2)
      core.module.io.l2_hint.bits.sourceId := l2top.module.io.l2_hint.bits.sourceId
      core.module.io.l2_hint.bits.isKeyword := l2top.module.io.l2_hint.bits.isKeyword
      core.module.io.l2_hint.valid := l2top.module.io.l2_hint.valid

      core.module.io.l2PfqBusy := false.B
      core.module.io.debugTopDown.l2MissMatch := false.B
      core.module.io.topDownInfo.l2Miss := false.B

      core.module.io.l2_tlb_req.req.valid := false.B
      core.module.io.l2_tlb_req.req.bits := DontCare
      core.module.io.l2_tlb_req.req_kill := DontCare
      core.module.io.l2_tlb_req.resp.ready := true.B

      core.module.io.perfEvents <> DontCare
    }

    io.debugTopDown.robHeadPaddr := core.module.io.debugTopDown.robHeadPaddr
    core.module.io.debugTopDown.l3MissMatch := io.debugTopDown.l3MissMatch
    l2top.module.io.l3Miss.fromTile := io.l3Miss
    core.module.io.topDownInfo.l3Miss := l2top.module.io.l3Miss.toCore

    io.chi.foreach(_ <> l2top.module.io.chi.get)
    l2top.module.io.nodeID.foreach(_ := io.nodeID.get)

    if (debugOpts.ResetGen && enableL2) {
      core.module.reset := l2top.module.reset_core
    }
  }

  lazy val module = new XSTileImp(this)
}
