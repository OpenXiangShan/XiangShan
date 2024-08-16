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
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import system.HasSoCParameter
import device.MsiInfoBundle
import coupledL2.tl2chi.PortIO
import utility.IntBuffer

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
  val clintIntNode = IntIdentityNode()
  val debugIntNode = IntIdentityNode()
  val plicIntNode = IntIdentityNode()
  tile.clint_int_node := IntBuffer(2) := clintIntNode
  tile.debug_int_node := IntBuffer(2) := debugIntNode
  tile.plic_int_node :*= IntBuffer(2) :*= plicIntNode
  class XSTileWrapImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
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
      val clintTimeAsync = Flipped(new AsyncBundle(UInt(64.W), AsyncQueueParams(1)))
    })
    tile.module.io.hartId := io.hartId
    tile.module.io.msiInfo := io.msiInfo
    tile.module.io.reset_vector := io.reset_vector
    io.cpu_halt := tile.module.io.cpu_halt 
    io.debugTopDown <> tile.module.io.debugTopDown
    io.chi.foreach(_ <> tile.module.io.chi.get)
    tile.module.io.nodeID.foreach(_ := io.nodeID.get)

    val clintTimeAsyncQueueSink = Module(new AsyncQueueSink(UInt(64.W), AsyncQueueParams(1)))
    clintTimeAsyncQueueSink.io.async <> io.clintTimeAsync
    clintTimeAsyncQueueSink.io.deq.ready := true.B
    tile.module.io.clintTime.valid := clintTimeAsyncQueueSink.io.deq.valid
    tile.module.io.clintTime.bits := clintTimeAsyncQueueSink.io.deq.bits

    dontTouch(io.hartId)
    dontTouch(io.msiInfo)
  }
  lazy val module = new XSTileWrapImp(this)
}
