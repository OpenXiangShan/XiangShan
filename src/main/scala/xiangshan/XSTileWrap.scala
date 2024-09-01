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
import device.{IMSICAsync, MsiInfoBundle}
import coupledL2.tl2chi.{PortIO, AsyncPortIO, CHIAsyncBridgeSource}
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
      val chi = EnableCHIAsyncBridge match {
        case Some(param) => Some(new AsyncPortIO(param))
        case None => Some(new PortIO)
      }
      val nodeID = if (enableCHI) Some(Input(UInt(NodeIDWidth.W))) else None
      val clintTime = EnableClintAsyncBridge match {
        case Some(param) => Some(Flipped(new AsyncBundle(UInt(64.W), param)))
        case None => Some(Input(ValidIO(UInt(64.W))))
      }
    })

    val imsicAsync = Module(new IMSICAsync())
    imsicAsync.i.msiInfo := io.msiInfo

    tile.module.io.hartId := io.hartId
    tile.module.io.msiInfo := imsicAsync.o.msiInfo
    tile.module.io.reset_vector := io.reset_vector
    io.cpu_halt := tile.module.io.cpu_halt 
    io.debugTopDown <> tile.module.io.debugTopDown
    tile.module.io.nodeID.foreach(_ := io.nodeID.get)

    // CLINT Async Queue Sink
    EnableClintAsyncBridge match {
      case Some(param) =>
        val sink = Module(new AsyncQueueSink(UInt(64.W), param))
        sink.io.async <> io.clintTime.get
        sink.io.deq.ready := true.B
        tile.module.io.clintTime.valid := sink.io.deq.valid
        tile.module.io.clintTime.bits := sink.io.deq.bits
      case None =>
        tile.module.io.clintTime := io.clintTime.get
    }

    // CHI Async Queue Source
    EnableCHIAsyncBridge match {
      case Some(param) =>
        val source = Module(new CHIAsyncBridgeSource(param))
        source.io.enq <> tile.module.io.chi.get
        io.chi.get <> source.io.async
      case None =>
        require(enableCHI)
        io.chi.get <> tile.module.io.chi.get
    }

    dontTouch(io.hartId)
    dontTouch(io.msiInfo)
  }
  lazy val module = new XSTileWrapImp(this)
}
