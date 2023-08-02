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

import chisel3._
import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3.util.{Valid, ValidIO}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, BusErrors}
import freechips.rocketchip.tilelink._
import coupledL2.{L2ParamKey, CoupledL2}
import system.HasSoCParameter
import top.BusPerfMonitor
import utility.{DelayN, ResetGen, TLClientsMerger, TLEdgeBuffer, TLLogger}

class XSTile()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter
{
  private val core = LazyModule(new XSCore())
  private val l2top = LazyModule(new L2Top())

  // =========== Public Ports ============
  val memory_port = l2top.memory_port
  val uncache = l2top.mmio_port
  val clint_int_sink = core.memBlock.clint_int_sink
  val plic_int_sink = core.memBlock.plic_int_sink
  val debug_int_sink = core.memBlock.debug_int_sink
  val beu_int_source = l2top.beu.intNode
  val core_reset_sink = BundleBridgeSink(Some(() => Reset()))

  // =========== Components' Connection ============
  // L1 to l1_xbar (same as before)
  coreParams.dcacheParametersOpt.map { _ =>
    l2top.misc_l2_pmu := l2top.l1d_logger := l2top.l1d_l2_bufferOpt.get.node :=
      l2top.l1d_l2_pmu := core.memBlock.dcache.clientNode
  }
  l2top.misc_l2_pmu := l2top.l1i_logger := core.memBlock.frontendBridge.icache_node
  if (!coreParams.softPTW) {
    l2top.misc_l2_pmu := l2top.ptw_logger := core.memBlock.ptw_to_l2_buffer.node
  }
  l2top.l1_xbar :=* l2top.misc_l2_pmu

  val l2cache = l2top.l2cache
  // l1_xbar to l2
  l2cache match {
    case Some(l2) =>
      l2.node :*= l2top.l1_xbar
      l2.pf_recv_node.map(recv => {
          println("Connecting L1 prefetcher to L2!")
          recv := core.memBlock.pf_sender_opt.get
      })
    case None =>
      val dummyMatch = WireDefault(false.B)
      ExcitingUtils.addSource(dummyMatch, s"L2MissMatch_${p(XSCoreParamsKey).HartId}", ExcitingUtils.Perf, true)
  }

  // mmio
  l2top.i_mmio_port := core.memBlock.frontendBridge.instr_uncache_node
  l2top.d_mmio_port := core.memBlock.uncache.clientNode

  // =========== IO Connection ============
  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val hartId = Input(UInt(64.W))
      val reset_vector = Input(UInt(PAddrBits.W))
      val cpu_halt = Output(Bool())
    })

    dontTouch(io.hartId)

    val core_soft_rst = core_reset_sink.in.head._1 // unused

    core.module.io.hartId := io.hartId
    core.module.io.reset_vector := l2top.module.reset_vector.toCore
    l2top.module.reset_vector.fromTile := io.reset_vector
    io.cpu_halt := core.module.io.cpu_halt

    if (l2cache.isDefined) {
      // TODO: add perfEvents of L2
      // core.module.io.perfEvents.zip(l2cache.get.module.io.perfEvents.flatten).foreach(x => x._1.value := x._2)
    }
    else {
      core.module.io.perfEvents <> 0.U.asTypeOf(core.module.io.perfEvents)
    }

    l2top.module.beu_errors.icache <> core.module.io.beu_errors.icache
    l2top.module.beu_errors.dcache <> core.module.io.beu_errors.dcache

    l2top.module.beu_errors.l2 <> 0.U.asTypeOf(l2top.module.beu_errors.l2) // TODO: add ECC interface of L2
    core.module.io.l2_hint.bits.sourceId := l2top.module.l2_hint.bits
    core.module.io.l2_hint.valid := l2top.module.l2_hint.valid

    // Modules are reset one by one
    // io_reset ----
    //             |
    //             v
    // reset ----> OR_SYNC --> {Misc, L2 Cache, Cores}
//    val resetChain = Seq(
//      Seq(l2top.module, core.module) // TTTODO: problem of l2cache resetting twice
//    )
//    ResetGen(resetChain, reset, !debugOpts.FPGAPlatform)
  }
}
