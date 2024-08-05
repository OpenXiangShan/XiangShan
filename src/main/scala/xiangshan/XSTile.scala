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
import device.MsiInfoBundle
import system.HasSoCParameter
import top.{BusPerfMonitor, ArgParser, Generator}
import utility.{DelayN, ResetGen, TLClientsMerger, TLEdgeBuffer, TLLogger, Constantin, ChiselDB, FileRegisters}
import coupledL2.EnableCHI
import coupledL2.tl2chi.PortIO

class XSTile()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter
{
  override def shouldBeInlined: Boolean = false
  val core = LazyModule(new XSCore())
  val l2top = LazyModule(new L2Top())

  val enableL2 = coreParams.L2CacheParamsOpt.isDefined
  // =========== Public Ports ============
  val core_l3_pf_port = core.memBlock.l3_pf_sender_opt
  val memory_port = if (enableCHI && enableL2) None else Some(l2top.memory_port.get)
  val tl_uncache = l2top.mmio_port
  // val axi4_uncache = if (enableCHI) Some(AXI4UserYanker()) else None
  val beu_int_source = l2top.beu.intNode
  val core_reset_sink = BundleBridgeSink(Some(() => Reset()))
  val clint_int_node = l2top.clint_int_node
  val plic_int_node = l2top.plic_int_node
  val debug_int_node = l2top.debug_int_node
  core.memBlock.clint_int_sink := clint_int_node
  core.memBlock.plic_int_sink :*= plic_int_node
  core.memBlock.debug_int_sink := debug_int_node

  // =========== Components' Connection ============
  // L1 to l1_xbar
  coreParams.dcacheParametersOpt.map { _ =>
    l2top.misc_l2_pmu := l2top.l1d_logger := core.memBlock.dcache_port :=
      core.memBlock.l1d_to_l2_buffer.node := core.memBlock.dcache.clientNode
  }

  l2top.misc_l2_pmu := l2top.l1i_logger := core.memBlock.frontendBridge.icache_node
  if (!coreParams.softPTW) {
    l2top.misc_l2_pmu := l2top.ptw_logger := l2top.ptw_to_l2_buffer.node := core.memBlock.ptw_to_l2_buffer.node
  }

  // L2 Prefetch
  l2top.l2cache match {
    case Some(l2) =>
      l2.pf_recv_node.foreach(recv => {
        println("Connecting L1 prefetcher to L2!")
        recv := core.memBlock.l2_pf_sender_opt.get
      })
    case None =>
  }

  val core_l3_tpmeta_source_port = l2top.l2cache match {
    case Some(l2) => l2.tpmeta_source_node
    case None => None
  }
  val core_l3_tpmeta_sink_port = l2top.l2cache match {
    case Some(l2) => l2.tpmeta_sink_node
    case None => None
  }

  // mmio
  l2top.i_mmio_port := l2top.i_mmio_buffer.node := core.memBlock.frontendBridge.instr_uncache_node
  l2top.d_mmio_port := core.memBlock.uncache.clientNode

  // =========== IO Connection ============
  class XSTileImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val io = IO(new Bundle {
      val hartId = Input(UInt(hartIdLen.W))
      val msiInfo = Input(ValidIO(new MsiInfoBundle))
      val reset_vector = Input(UInt(PAddrBits.W))
      val cpu_halt = Output(Bool())
      val debugTopDown = new Bundle {
        val robHeadPaddr = Valid(UInt(PAddrBits.W))
        val l3MissMatch = Input(Bool())
      }
      val chi = if (enableCHI) Some(new PortIO) else None
      val nodeID = if (enableCHI) Some(Input(UInt(NodeIDWidth.W))) else None
      val clintTime = Input(ValidIO(UInt(64.W)))
    })

    dontTouch(io.hartId)
    dontTouch(io.msiInfo)
    if (!io.chi.isEmpty) { dontTouch(io.chi.get) }

    val core_soft_rst = core_reset_sink.in.head._1 // unused

    l2top.module.hartId.fromTile := io.hartId
    core.module.io.hartId := l2top.module.hartId.toCore
    core.module.io.reset_vector := l2top.module.reset_vector.toCore
    core.module.io.msiInfo := io.msiInfo
    core.module.io.clintTime := io.clintTime
    l2top.module.reset_vector.fromTile := io.reset_vector
    l2top.module.cpu_halt.fromCore := core.module.io.cpu_halt
    io.cpu_halt := l2top.module.cpu_halt.toTile

    core.module.io.perfEvents <> DontCare

    l2top.module.beu_errors.icache <> core.module.io.beu_errors.icache
    l2top.module.beu_errors.dcache <> core.module.io.beu_errors.dcache
    if (enableL2) {
      // TODO: add ECC interface of L2

      l2top.module.beu_errors.l2 <> 0.U.asTypeOf(l2top.module.beu_errors.l2)
      core.module.io.l2_hint.bits.sourceId := l2top.module.l2_hint.bits.sourceId
      core.module.io.l2_hint.bits.isKeyword := l2top.module.l2_hint.bits.isKeyword
      core.module.io.l2_hint.valid := l2top.module.l2_hint.valid

      core.module.io.l2PfqBusy := false.B
      core.module.io.debugTopDown.l2MissMatch := l2top.module.debugTopDown.l2MissMatch
      l2top.module.debugTopDown.robHeadPaddr := core.module.io.debugTopDown.robHeadPaddr
      l2top.module.debugTopDown.robTrueCommit := core.module.io.debugTopDown.robTrueCommit
      l2top.module.l2_pmp_resp := core.module.io.l2_pmp_resp
      core.module.io.l2_tlb_req <> l2top.module.l2_tlb_req
    } else {

      l2top.module.beu_errors.l2 <> 0.U.asTypeOf(l2top.module.beu_errors.l2)
      core.module.io.l2_hint.bits.sourceId := l2top.module.l2_hint.bits.sourceId
      core.module.io.l2_hint.bits.isKeyword := l2top.module.l2_hint.bits.isKeyword
      core.module.io.l2_hint.valid := l2top.module.l2_hint.valid

      core.module.io.l2PfqBusy := false.B
      core.module.io.debugTopDown.l2MissMatch := false.B

      core.module.io.l2_tlb_req.req.valid := false.B
      core.module.io.l2_tlb_req.req.bits := DontCare
      core.module.io.l2_tlb_req.req_kill := DontCare
      core.module.io.l2_tlb_req.resp.ready := true.B
    }

    io.debugTopDown.robHeadPaddr := core.module.io.debugTopDown.robHeadPaddr
    core.module.io.debugTopDown.l3MissMatch := io.debugTopDown.l3MissMatch

    io.chi.foreach(_ <> l2top.module.chi.get)
    l2top.module.nodeID.foreach(_ := io.nodeID.get)

    if (debugOpts.ResetGen && enableL2) {
      core.module.reset := l2top.module.reset_core
    }
  }

  lazy val module = new XSTileImp(this)
}
