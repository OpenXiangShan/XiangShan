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

import org.chipsalliance.cde.config
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2.PrefetchCtrlFromCore
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.HasFPUParameters
import system.HasSoCParameter
import utils._
import utility._
import utility.mbist.{MbistInterface, MbistPipeline}
import utility.sram.{SramBroadcastBundle, SramHelper}
import xiangshan.frontend._
import xiangshan.backend._
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.trace.TraceCoreInterface
import xiangshan.mem._
import xiangshan.cache.mmu._
import xiangshan.cache.mmu.TlbRequestIO
import scala.collection.mutable.ListBuffer

abstract class XSModule(implicit val p: Parameters) extends Module
  with HasXSParameter
  with HasFPUParameters

//remove this trait after impl module logic
trait NeedImpl {
  this: RawModule =>
  protected def IO[T <: Data](iodef: T): T = {
    println(s"[Warn]: (${this.name}) please reomve 'NeedImpl' after implement this module")
    val io = chisel3.IO(iodef)
    io <> DontCare
    io
  }
}

abstract class XSBundle(implicit val p: Parameters) extends Bundle
  with HasXSParameter

abstract class XSCoreBase()(implicit p: config.Parameters) extends LazyModule
  with HasXSParameter
{
  override def shouldBeInlined: Boolean = false
  // outer facing nodes
  val frontend = LazyModule(new Frontend())
  val csrOut = BundleBridgeSource(Some(() => new DistributedCSRIO()))
  val backend = LazyModule(new Backend(backendParams))

  val memBlock = LazyModule(new MemBlock)

  memBlock.inner.frontendBridge.icache_node := frontend.inner.icache.clientNode
  memBlock.inner.frontendBridge.instr_uncache_node := frontend.inner.instrUncache.clientNode
  if (icacheCtrlEnabled) {
    frontend.inner.icache.ctrlUnitOpt.get.node := memBlock.inner.frontendBridge.icachectrl_node
  }
}

class XSCore()(implicit p: config.Parameters) extends XSCoreBase
  with HasXSDts
{
  lazy val module = new XSCoreImp(this)
}

class XSCoreImp(outer: XSCoreBase) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasSoCParameter {
  val io = IO(new Bundle {
    val hartId = Input(UInt(hartIdLen.W))
    val msiInfo = Input(ValidIO(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W)))
    val msiAck = Output(Bool())
    val clintTime = Input(ValidIO(UInt(64.W)))
    val reset_vector = Input(UInt(PAddrBits.W))
    val cpu_halt = Output(Bool())
    val l2_flush_done = Input(Bool())
    val l2_flush_en = Output(Bool())
    val power_down_en = Output(Bool())
    val cpu_critical_error = Output(Bool())
    val resetInFrontend = Output(Bool())
    val traceCoreInterface = new TraceCoreInterface
    val l2PfCtrl = Output(new PrefetchCtrlFromCore)
    val perfEvents = Input(Vec(numPCntHc * coreParams.L2NBanks + 1, new PerfEvent))
    val beu_errors = Output(new XSL1BusErrors())
    val l2_hint = Input(Valid(new L2ToL1Hint()))
    val l2_tlb_req = Flipped(new TlbRequestIO(nRespDups = 2))
    val l2_pmp_resp = new PMPRespBundle
    val l2PfqBusy = Input(Bool())
    val debugTopDown = new Bundle {
      val robTrueCommit = Output(UInt(64.W))
      val robHeadPaddr = Valid(UInt(PAddrBits.W))
      val l2MissMatch = Input(Bool())
      val l3MissMatch = Input(Bool())
    }
    val topDownInfo = Input(new Bundle {
      val l2Miss = Bool()
      val l3Miss = Bool()
    })
    val dft = Option.when(hasDFT)(Input(new SramBroadcastBundle))
    val dft_reset = Option.when(hasDFT)(Input(new DFTResetSignals()))
  })

  dontTouch(io.l2_flush_done)
  dontTouch(io.l2_flush_en)
  dontTouch(io.power_down_en)

  println(s"FPGAPlatform:${env.FPGAPlatform} EnableDebug:${env.EnableDebug}")

  val frontend = outer.frontend.module
  val backend = outer.backend.module
  val memBlock = outer.memBlock.module

  frontend.io.hartId := memBlock.io.inner_hartId
  frontend.io.reset_vector := memBlock.io.inner_reset_vector
  frontend.io.softPrefetch <> memBlock.io.ifetchPrefetch
  frontend.io.backend <> backend.io.frontend
  frontend.io.sfence <> backend.io.frontendSfence
  frontend.io.tlbCsr <> backend.io.frontendTlbCsr
  frontend.io.csrCtrl <> backend.io.frontendCsrCtrl
  frontend.io.fencei <> backend.io.fenceio.fencei

  backend.io.fromTop := memBlock.io.mem_to_ooo.topToBackendBypass

  require(backend.io.mem.stIn.length == memBlock.io.mem_to_ooo.updateLFST.length)
  backend.io.mem.stIn.zip(memBlock.io.mem_to_ooo.updateLFST).foreach { case (sink, source) =>
    sink := source
  }
  backend.io.mem.memoryViolation := memBlock.io.mem_to_ooo.memoryViolation
  backend.io.mem.lsqEnqIO <> memBlock.io.ooo_to_mem.enqLsq
  backend.io.mem.sqDeq := memBlock.io.mem_to_ooo.sqDeq
  backend.io.mem.lqDeq := memBlock.io.mem_to_ooo.lqDeq
  backend.io.mem.sqDeqPtr := memBlock.io.mem_to_ooo.sqDeqPtr
  backend.io.mem.lqDeqPtr := memBlock.io.mem_to_ooo.lqDeqPtr
  backend.io.mem.lqCancelCnt := memBlock.io.mem_to_ooo.lqCancelCnt
  backend.io.mem.sqCancelCnt := memBlock.io.mem_to_ooo.sqCancelCnt
  backend.io.mem.stIssuePtr := memBlock.io.mem_to_ooo.stIssuePtr
  backend.io.mem.ldaIqFeedback := memBlock.io.mem_to_ooo.ldaIqFeedback
  backend.io.mem.staIqFeedback := memBlock.io.mem_to_ooo.staIqFeedback
  backend.io.mem.hyuIqFeedback := memBlock.io.mem_to_ooo.hyuIqFeedback
  backend.io.mem.vstuIqFeedback := memBlock.io.mem_to_ooo.vstuIqFeedback
  backend.io.mem.vlduIqFeedback := memBlock.io.mem_to_ooo.vlduIqFeedback
  backend.io.mem.ldCancel := memBlock.io.mem_to_ooo.ldCancel
  backend.io.mem.wakeup.zip(memBlock.io.mem_to_ooo.wakeup).map{ case (sink, source) => {
    sink.valid := source.valid
    connectSamePort(sink.bits, source.bits)
  }}
  backend.io.mem.writebackLda <> memBlock.io.mem_to_ooo.writebackLda
  backend.io.mem.writebackSta <> memBlock.io.mem_to_ooo.writebackSta
  backend.io.mem.writebackHyuLda <> memBlock.io.mem_to_ooo.writebackHyuLda
  backend.io.mem.writebackHyuSta <> memBlock.io.mem_to_ooo.writebackHyuSta
  backend.io.mem.writebackStd <> memBlock.io.mem_to_ooo.writebackStd
  backend.io.mem.writebackVldu <> memBlock.io.mem_to_ooo.writebackVldu
  backend.io.mem.robLsqIO.mmio := memBlock.io.mem_to_ooo.lsqio.mmio
  backend.io.mem.robLsqIO.uop := memBlock.io.mem_to_ooo.lsqio.uop

  backend.io.mem.exceptionAddr.vaddr  := memBlock.io.mem_to_ooo.lsqio.vaddr
  backend.io.mem.exceptionAddr.gpaddr := memBlock.io.mem_to_ooo.lsqio.gpaddr
  backend.io.mem.exceptionAddr.isForVSnonLeafPTE := memBlock.io.mem_to_ooo.lsqio.isForVSnonLeafPTE
  backend.io.mem.debugLS := memBlock.io.debug_ls
  backend.io.mem.lsTopdownInfo := memBlock.io.mem_to_ooo.lsTopdownInfo
  backend.io.mem.lqCanAccept := memBlock.io.mem_to_ooo.lsqio.lqCanAccept
  backend.io.mem.sqCanAccept := memBlock.io.mem_to_ooo.lsqio.sqCanAccept
  backend.io.fenceio.sbuffer.sbIsEmpty := memBlock.io.mem_to_ooo.sbIsEmpty

  backend.io.perf.frontendInfo := frontend.io.frontendInfo
  backend.io.perf.memInfo := memBlock.io.memInfo
  backend.io.perf.perfEventsFrontend := frontend.io_perf
  backend.io.perf.perfEventsLsu := memBlock.io_perf
  backend.io.perf.perfEventsHc := memBlock.io.inner_hc_perfEvents
  backend.io.perf.perfEventsBackend := DontCare
  backend.io.perf.retiredInstr := DontCare
  backend.io.perf.ctrlInfo := DontCare

  backend.io.mem.storeDebugInfo <> memBlock.io.mem_to_ooo.storeDebugInfo

  // top -> memBlock
  memBlock.io.fromTopToBackend.clintTime := io.clintTime
  memBlock.io.fromTopToBackend.msiInfo := io.msiInfo
  memBlock.io.hartId := io.hartId
  memBlock.io.l2_flush_done := io.l2_flush_done
  memBlock.io.outer_reset_vector := io.reset_vector
  memBlock.io.outer_hc_perfEvents := io.perfEvents
  // frontend -> memBlock
  memBlock.io.inner_beu_errors_icache <> frontend.io.error.bits.toL1BusErrorUnitInfo(frontend.io.error.valid)
  memBlock.io.ooo_to_mem.backendToTopBypass := backend.io.toTop
  memBlock.io.ooo_to_mem.issueLda <> backend.io.mem.issueLda
  memBlock.io.ooo_to_mem.issueSta <> backend.io.mem.issueSta
  memBlock.io.ooo_to_mem.issueStd <> backend.io.mem.issueStd
  memBlock.io.ooo_to_mem.issueHya <> backend.io.mem.issueHylda
  backend.io.mem.issueHysta.foreach(_.ready := false.B) // this fake port should not be used
  memBlock.io.ooo_to_mem.issueVldu <> backend.io.mem.issueVldu

  // By default, instructions do not have exceptions when they enter the function units.
  memBlock.io.ooo_to_mem.issueUops.map(_.bits.uop.clearExceptions())
  memBlock.io.ooo_to_mem.storePc := backend.io.mem.storePcRead
  memBlock.io.ooo_to_mem.hybridPc := backend.io.mem.hyuPcRead
  memBlock.io.ooo_to_mem.flushSb := backend.io.fenceio.sbuffer.flushSb
  memBlock.io.ooo_to_mem.loadFastMatch := 0.U.asTypeOf(memBlock.io.ooo_to_mem.loadFastMatch)
  memBlock.io.ooo_to_mem.loadFastImm := 0.U.asTypeOf(memBlock.io.ooo_to_mem.loadFastImm)
  memBlock.io.ooo_to_mem.loadFastFuOpType := 0.U.asTypeOf(memBlock.io.ooo_to_mem.loadFastFuOpType)

  memBlock.io.ooo_to_mem.sfence <> backend.io.mem.sfence

  memBlock.io.redirect := backend.io.mem.redirect
  memBlock.io.ooo_to_mem.csrCtrl := backend.io.mem.csrCtrl
  memBlock.io.ooo_to_mem.tlbCsr := backend.io.mem.tlbCsr
  memBlock.io.ooo_to_mem.lsqio.lcommit          := backend.io.mem.robLsqIO.lcommit
  memBlock.io.ooo_to_mem.lsqio.scommit          := backend.io.mem.robLsqIO.scommit
  memBlock.io.ooo_to_mem.lsqio.commit           := backend.io.mem.robLsqIO.commit
  memBlock.io.ooo_to_mem.lsqio.pendingPtr       := backend.io.mem.robLsqIO.pendingPtr
  memBlock.io.ooo_to_mem.lsqio.pendingPtrNext   := backend.io.mem.robLsqIO.pendingPtrNext
  memBlock.io.ooo_to_mem.isStoreException       := backend.io.mem.isStoreException
  memBlock.io.ooo_to_mem.isVlsException         := backend.io.mem.isVlsException

  memBlock.io.fetch_to_mem.itlb <> frontend.io.ptw
  memBlock.io.l2_hint.valid := io.l2_hint.valid
  memBlock.io.l2_hint.bits.sourceId := io.l2_hint.bits.sourceId
  memBlock.io.l2_tlb_req <> io.l2_tlb_req
  memBlock.io.l2_pmp_resp <> io.l2_pmp_resp
  memBlock.io.l2_hint.bits.isKeyword := io.l2_hint.bits.isKeyword
  memBlock.io.l2PfqBusy := io.l2PfqBusy

  // if l2 prefetcher use stream prefetch, it should be placed in XSCore

  // top-down info
  memBlock.io.debugTopDown.robHeadVaddr := backend.io.debugTopDown.fromRob.robHeadVaddr
  frontend.io.debugTopDown.robHeadVaddr.bits := backend.io.debugTopDown.fromRob.robHeadVaddr.bits
  frontend.io.debugTopDown.robHeadVaddr.valid := backend.io.debugTopDown.fromRob.robHeadVaddr.valid
  io.debugTopDown.robHeadPaddr := backend.io.debugTopDown.fromRob.robHeadPaddr
  io.debugTopDown.robTrueCommit := backend.io.debugRolling.robTrueCommit
  backend.io.debugTopDown.fromCore.l2MissMatch := io.debugTopDown.l2MissMatch
  backend.io.debugTopDown.fromCore.l3MissMatch := io.debugTopDown.l3MissMatch
  backend.io.debugTopDown.fromCore.fromMem := memBlock.io.debugTopDown.toCore
  memBlock.io.debugRolling := backend.io.debugRolling

  io.cpu_halt := memBlock.io.outer_cpu_halt
  io.l2_flush_en := memBlock.io.outer_l2_flush_en
  io.power_down_en := memBlock.io.outer_power_down_en
  io.cpu_critical_error := memBlock.io.outer_cpu_critical_error
  io.msiAck := memBlock.io.outer_msi_ack
  io.beu_errors.icache <> memBlock.io.outer_beu_errors_icache
  io.beu_errors.dcache <> memBlock.io.dcacheError.bits.toL1BusErrorUnitInfo(memBlock.io.dcacheError.valid)
  io.beu_errors.uncache <> memBlock.io.uncacheError
  io.beu_errors.l2 <> DontCare
  io.l2PfCtrl := backend.io.mem.csrCtrl.pf_ctrl.toL2PrefetchCtrl()

  memBlock.io.resetInFrontendBypass.fromFrontend := frontend.io.resetInFrontend
  io.resetInFrontend := memBlock.io.resetInFrontendBypass.toL2Top
  memBlock.io.traceCoreInterfaceBypass.fromBackend <> backend.io.traceCoreInterface
  io.traceCoreInterface <> memBlock.io.traceCoreInterfaceBypass.toL2Top
  memBlock.io.wfi <> backend.io.mem.wfi
  memBlock.io.topDownInfo.fromL2Top.l2Miss := io.topDownInfo.l2Miss
  memBlock.io.topDownInfo.fromL2Top.l3Miss := io.topDownInfo.l3Miss
  memBlock.io.topDownInfo.toBackend.noUopsIssued := backend.io.topDownInfo.noUopsIssued
  backend.io.topDownInfo.lqEmpty := memBlock.io.topDownInfo.toBackend.lqEmpty
  backend.io.topDownInfo.sqEmpty := memBlock.io.topDownInfo.toBackend.sqEmpty
  backend.io.topDownInfo.l1Miss := memBlock.io.topDownInfo.toBackend.l1Miss
  backend.io.topDownInfo.l2TopMiss.l2Miss := memBlock.io.topDownInfo.toBackend.l2TopMiss.l2Miss
  backend.io.topDownInfo.l2TopMiss.l3Miss := memBlock.io.topDownInfo.toBackend.l2TopMiss.l3Miss


  if (debugOpts.ResetGen) {
    backend.reset := memBlock.io.reset_backend
    frontend.reset := backend.io.frontendReset
  }

  memBlock.io.dft.zip(io.dft).foreach({ case (a, b) => a := b })
  memBlock.io.dft_reset.zip(io.dft_reset).foreach({ case (a, b) => a := b })
  frontend.io.dft.zip(memBlock.io.dft_frnt).foreach({ case (a, b) => a := b })
  frontend.io.dft_reset.zip(memBlock.io.dft_reset_frnt).foreach({ case (a, b) => a := b })
  backend.io.dft.zip(memBlock.io.dft_bcknd).foreach({ case (a, b) => a := b })
  backend.io.dft_reset.zip(memBlock.io.dft_reset_bcknd).foreach({ case (a, b) => a := b })
}
