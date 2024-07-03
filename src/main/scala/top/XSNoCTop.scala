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

package top

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import utility._
import system._
import device._
import org.chipsalliance.cde.config._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._
import coupledL2.tl2chi.PortIO
import freechips.rocketchip.tile.MaxHartIdBits

class XSNoCTop()(implicit p: Parameters) extends BaseXSSoc with HasSoCParameter
{
  override lazy val desiredName: String = "XSTop"

  ResourceBinding {
    val width = ResourceInt(2)
    val model = "freechips,rocketchip-unknown"
    Resource(ResourceAnchors.root, "model").bind(ResourceString(model))
    Resource(ResourceAnchors.root, "compat").bind(ResourceString(model + "-dev"))
    Resource(ResourceAnchors.soc, "compat").bind(ResourceString(model + "-soc"))
    Resource(ResourceAnchors.root, "width").bind(width)
    Resource(ResourceAnchors.soc, "width").bind(width)
    Resource(ResourceAnchors.cpus, "width").bind(ResourceInt(1))
    def bindManagers(xbar: TLNexusNode) = {
      ManagerUnification(xbar.edges.in.head.manager.managers).foreach{ manager =>
        manager.resources.foreach(r => r.bind(manager.toResource))
      }
    }
  }

  // xstile
  val core_with_l2 = LazyModule(new XSTile()(p.alterPartial({
    case XSCoreParamsKey => tiles.head
  })))

  // imsic bus top
  val u_imsic_bus_top = LazyModule(new imsic_bus_top(soc.IMSICUseTL))

  // interrupts
  val clintIntNode = IntSourceNode(IntSourcePortSimple(1, 1, 2))
  val debugIntNode = IntSourceNode(IntSourcePortSimple(1, 1, 1))
  val plicIntNode = IntSourceNode(IntSourcePortSimple(1, 2, 1))
  val beuIntNode = IntSinkNode(IntSinkPortSimple(1, 1))
  core_with_l2.clint_int_node := IntBuffer() := clintIntNode
  core_with_l2.debug_int_node := IntBuffer() := debugIntNode
  core_with_l2.plic_int_node :*= IntBuffer() :*= plicIntNode
  beuIntNode := IntBuffer() := core_with_l2.beu_int_source
  val clint = InModuleBody(clintIntNode.makeIOs())
  val debug = InModuleBody(debugIntNode.makeIOs())
  val plic = InModuleBody(plicIntNode.makeIOs())
  val beu = InModuleBody(beuIntNode.makeIOs())

  // reset nodes
  val core_rst_node = BundleBridgeSource(() => Reset())
  core_with_l2.core_reset_sink := core_rst_node

  class XSNoCTopImp(wrapper: XSNoCTop) extends LazyRawModuleImp(wrapper) {
    FileRegisters.add("dts", dts)
    FileRegisters.add("graphml", graphML)
    FileRegisters.add("json", json)
    FileRegisters.add("plusArgs", freechips.rocketchip.util.PlusArgArtefacts.serialize_cHeader())

    val clock = IO(Input(Clock()))
    val reset = IO(Input(AsyncReset()))
    val bus_clock = IO(Input(Clock()))
    val bus_reset = IO(Input(AsyncReset()))
    val io = IO(new Bundle {
      val hartId = Input(UInt(p(MaxHartIdBits).W))
      val riscv_halt = Output(Bool())
      val riscv_rst_vec = Input(UInt(38.W))
      val chi = new PortIO
      val nodeID = Input(UInt(p(SoCParamsKey).NodeIDWidth.W))
    })
    // imsic axi4lite io
    val imsic_m_s = wrapper.u_imsic_bus_top.module.m_s.map(x => IO(chiselTypeOf(x)))
    val imsic_s_s = wrapper.u_imsic_bus_top.module.s_s.map(x => IO(chiselTypeOf(x)))
    // imsic tl io
    val imsic_m_tl = wrapper.u_imsic_bus_top.tl_m.map(x => IO(chiselTypeOf(x.getWrappedValue)))
    val imsic_s_tl = wrapper.u_imsic_bus_top.tl_s.map(x => IO(chiselTypeOf(x.getWrappedValue)))

    val reset_sync = withClockAndReset(clock, reset) { ResetGen() }
    val bus_reset_sync = withClockAndReset(bus_clock, bus_reset) { ResetGen() }

    // override LazyRawModuleImp's clock and reset
    childClock := clock
    childReset := reset_sync

    // bus clock and reset
    wrapper.u_imsic_bus_top.module.clock := bus_clock
    wrapper.u_imsic_bus_top.module.reset := bus_reset_sync

    // imsic axi4lite io connection
    wrapper.u_imsic_bus_top.module.m_s.foreach(_ <> imsic_m_s.get)
    wrapper.u_imsic_bus_top.module.s_s.foreach(_ <> imsic_s_s.get)

    // imsic tl io connection
    wrapper.u_imsic_bus_top.tl_m.foreach(_ <> imsic_m_tl.get)
    wrapper.u_imsic_bus_top.tl_s.foreach(_ <> imsic_s_tl.get)

    // input
    dontTouch(io)

    core_with_l2.module.io.hartId := io.hartId
    core_with_l2.module.io.nodeID.get := io.nodeID
    core_with_l2.module.io.chi.get <> io.chi
    io.riscv_halt := core_with_l2.module.io.cpu_halt
    core_with_l2.module.io.reset_vector := io.riscv_rst_vec
    // tie off core soft reset
    core_rst_node.out.head._1 := false.B.asAsyncReset

    core_with_l2.module.io.debugTopDown.l3MissMatch := false.B

    withClockAndReset(clock, reset_sync) {
      // Modules are reset one by one
      // reset ----> SYNC --> Core
      val resetChain = Seq(Seq(core_with_l2.module))
      ResetGen(resetChain, reset_sync, !debugOpts.FPGAPlatform)
    }

  }

  lazy val module = new XSNoCTopImp(this)
}
