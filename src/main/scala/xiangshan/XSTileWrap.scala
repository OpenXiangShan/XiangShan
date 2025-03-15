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

//import device.{IMSICAsync, MsiInfoBundle}
import aia._
import chisel3._
import chisel3.util._
import coupledL2.tl2chi.AsyncPortIO
import coupledL2.tl2chi.CHIAsyncBridgeSource
import coupledL2.tl2chi.PortIO
import device._
import freechips.rocketchip.devices.debug
import freechips.rocketchip.devices.debug.ClockedDMIIO
import freechips.rocketchip.devices.debug.ExportDebug
import freechips.rocketchip.devices.debug.TLDebugModule
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink.TLWidthWidget
import freechips.rocketchip.tilelink.TLXbar
import freechips.rocketchip.util._
import org.chipsalliance.cde.config._
import system.HasSoCParameter
import utility.IntBuffer
import utility.ResetGen
import xiangshan.backend.trace.TraceCoreInterface

// This module is used for XSNoCTop for async time domain and divide different
// voltage domain. Everything in this module should be in the core clock domain
// and higher voltage domain.
class XSTileWrap()(implicit p: Parameters) extends LazyModule
    with HasXSParameter
    with HasSoCParameter {
  override def shouldBeInlined: Boolean = false

  val tile = LazyModule(new XSTile())

  // interrupts sync
  val clintIntNode = IntIdentityNode()
  val debugIntNode = IntIdentityNode()
  val plicIntNode  = IntIdentityNode()
  val beuIntNode   = IntIdentityNode()
  val nmiIntNode   = IntIdentityNode()
  tile.clint_int_node := IntBuffer(3, cdc = true) := clintIntNode
  tile.debug_int_node := IntBuffer(3, cdc = true) := debugIntNode
  tile.plic_int_node :*= IntBuffer(3, cdc = true) :*= plicIntNode
  tile.nmi_int_node := IntBuffer(3, cdc = true) := nmiIntNode
  beuIntNode        := IntBuffer()              := tile.beu_int_source
  class XSTileWrapImp(wrapper: LazyModule) extends LazyRawModuleImp(wrapper) {
    val clock     = IO(Input(Clock()))
    val reset     = IO(Input(AsyncReset()))
    val noc_reset = EnableCHIAsyncBridge.map(_ => IO(Input(AsyncReset())))
    val soc_reset = Option.when(!ClintAsyncFromSPMT)(IO(Input(AsyncReset())))
    val io = IO(new Bundle {
      val hartId            = Input(UInt(hartIdLen.W))
      val msiinfo           = new MSITransBundle(aia.IMSICParams())
      val reset_vector      = Input(UInt(PAddrBits.W))
      val cpu_halt          = Output(Bool())
      val cpu_crtical_error = Output(Bool())
      // ==UseDMInTop start: 1- DebugModule is integrated in XSTOP, only spacemit need it,other 0.==
      val hartResetReq  = Option.when(!UseDMInTop)(Input(Bool()))
      val hartIsInReset = Option.when(!UseDMInTop)(Output(Bool()))
      val dm = Option.when(UseDMInTop)(new Bundle {
        val dmi     = (!p(ExportDebug).apb).option(Flipped(new ClockedDMIIO()))
        val ndreset = Output(Bool()) // output of top,to request that soc can reset system logic exclude debug.
      })
      // ==UseDMInTop end ==
      val traceCoreInterface = new TraceCoreInterface
      val debugTopDown = new Bundle {
        val robHeadPaddr = Valid(UInt(PAddrBits.W))
        val l3MissMatch  = Input(Bool())
      }

      val l3Miss = Input(Bool())
      val chi = EnableCHIAsyncBridge match {
        case Some(param) => if (CHIAsyncFromSPMT) new CHIAsyncIOSPMT() else new AsyncPortIO(param)
        case None        => new PortIO
      }
      val nodeID = if (enableCHI) Some(Input(UInt(NodeIDWidth.W))) else None
      val clintTime = EnableClintAsyncBridge match {
        case Some(param) =>
          if (ClintAsyncFromSPMT) Input(ValidIO(UInt(64.W))) else Flipped(new AsyncBundle(UInt(64.W), param))
        case None => Input(ValidIO(UInt(64.W)))
      }
    })
    val hartResetReq = Wire(Bool()) // derive from io.hartResetReq or debugwrapper in top
    io.hartResetReq.foreach(iohartResetReq => hartResetReq := iohartResetReq)
    val reset_sync     = withClockAndReset(clock, (reset.asBool || hartResetReq).asAsyncReset)(ResetGen())
    val noc_reset_sync = EnableCHIAsyncBridge.map(_ => withClockAndReset(clock, noc_reset.get)(ResetGen()))
    val soc_reset_sync = withClockAndReset(clock, soc_reset.get)(ResetGen())

    // override LazyRawModuleImp's clock and reset
    childClock := clock
    childReset := reset_sync

    tile.module.io.hartId := io.hartId
    // connect msi info io with xstile
    // start :TBD zhaohong ,wait tile update the msi interface,

    tile.module.io.msiInfo.valid := io.msiinfo.vld_req
    io.msiinfo.vld_ack           := io.msiinfo.vld_req // TODO
    tile.module.io.msiInfo.bits.info := io.msiinfo.data // 1.U // TODO for compile error since type donot match io.msiinfo.data
    // end :TBD zhaohong

    tile.module.io.reset_vector := io.reset_vector
    io.cpu_halt                 := tile.module.io.cpu_halt
    io.cpu_crtical_error        := tile.module.io.cpu_crtical_error
    io.hartIsInReset.foreach(_ := tile.module.io.hartIsInReset)
    io.traceCoreInterface <> tile.module.io.traceCoreInterface
    io.debugTopDown <> tile.module.io.debugTopDown
    tile.module.io.l3Miss := io.l3Miss
    tile.module.io.nodeID.foreach(_ := io.nodeID.get)

    // instance :TL DebugModule
//    val debugModule = Option.when(UseDMInTop){
//      val l_debugModule = LazyModule(new DebugModule(numCores = NumCores, defDTM = false)(p))
////      l_debugModule.debug.node := TLBuffer() := TLFragmenter(8,32) := xbar // TLXbar())
//      l_debugModule.debug.node := TLXbar()
//      l_debugModule
//    }
    val l_debugModule = Option.when(UseDMInTop)(LazyModule(new DebugModule(numCores = NumCores, defDTM = false)(p)))
//    val debugModule = Option.when(UseDMInTop)(Module(l_debugModule.get.module))
    val debugModule = l_debugModule.map(l_debugModule => Module(l_debugModule.module))
    l_debugModule.foreach(_.debug.node := TLXbar()) // TBD connect with xstile.tl.out
    //    debugModule.foreach(_.module.debug.dmInner.dmInner.sb2tlOpt.foreach { sb2tl =>
    //      io.dm.mbus := sb2tl.node    //master node connect about debug
    //    })
    //    val io = IO(new DebugModuleIO)
    debugModule.foreach { debugModule =>
      debugModule.io.reset                   := reset
      debugModule.io.clock                   := clock
      debugModule.io.debugIO.clock           := clock
      debugModule.io.debugIO.reset           := reset
      debugModule.io.resetCtrl.hartIsInReset := tile.module.io.hartIsInReset
      hartResetReq                           := debugModule.io.resetCtrl.hartResetReq.get
      io.dm.foreach { iodm =>
        iodm.ndreset := debugModule.io.debugIO.ndreset
        iodm.dmi.map(_ <> debugModule.io.debugIO.clockeddmi.get)
      }
    }

    // CLINT Async Queue Sink
    EnableClintAsyncBridge match {
      case Some(param) =>
        val sink = withClockAndReset(clock, soc_reset_sync)(Module(new AsyncQueueSink(UInt(64.W), param)))
        sink.io.async <> io.clintTime
        sink.io.deq.ready              := true.B
        tile.module.io.clintTime.valid := sink.io.deq.valid
        tile.module.io.clintTime.bits  := sink.io.deq.bits
      case None =>
        tile.module.io.clintTime := io.clintTime
    }

    // CHI Async Queue Source
    EnableCHIAsyncBridge match {
      case Some(param) =>
        val source = withClockAndReset(clock, noc_reset_sync.get)(Module(new CHIAsyncBridgeSource(param)))
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
      ResetGen(resetChain, reset_sync, !debugOpts.FPGAPlatform)
    }
    dontTouch(io.hartId)
//    dontTouch(io.msiInfo)
  }
  lazy val module = new XSTileWrapImp(this)
}
