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
import device.MsiInfoBundle
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.HasFPUParameters
import system.HasSoCParameter
import utils._
import utility._
import xiangshan.frontend._
import xiangshan.backend._
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.trace.TraceCoreInterface
import xiangshan.mem._
import xiangshan.mem.L1PrefetchFuzzer
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
    val msiInfo = Input(ValidIO(new MsiInfoBundle))
    val clintTime = Input(ValidIO(UInt(64.W)))
    val reset_vector = Input(UInt(PAddrBits.W))
    val cpu_halt = Output(Bool())
    val cpu_critical_error = Output(Bool())
    val resetInFrontend = Output(Bool())
    val traceCoreInterface = new TraceCoreInterface
    val l2_pf_enable = Output(Bool())
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
  })

  println(s"FPGAPlatform:${env.FPGAPlatform} EnableDebug:${env.EnableDebug}")

  val frontend = outer.frontend.module
  val backend = outer.backend.module
  val memBlock = outer.memBlock.module

  frontend.io.hartId := memBlock.io.bypass.innerHartId
  frontend.io.reset_vector := memBlock.io.bypass.innerResetVector
  frontend.io.softPrefetch <> memBlock.io.toFrontend.ifetchPrefetch
  frontend.io.backend <> backend.io.frontend
  frontend.io.sfence <> backend.io.frontendSfence
  frontend.io.tlbCsr <> backend.io.frontendTlbCsr
  frontend.io.csrCtrl <> backend.io.frontendCsrCtrl
  frontend.io.fencei <> backend.io.fenceio.fencei

  backend.io.fromTop := memBlock.io.toBackend.topToBackendBypass

  require(backend.io.mem.stIn.length == memBlock.io.toBackend.stIssue.length)
  backend.io.mem.stIn.zip(memBlock.io.toBackend.stIssue).foreach { case (sink, source) =>
    sink.valid := source.valid
    sink.bits := 0.U.asTypeOf(sink.bits)
    sink.bits.robIdx := source.bits.uop.robIdx
    sink.bits.ssid := source.bits.uop.ssid
    sink.bits.storeSetHit := source.bits.uop.storeSetHit
    // The other signals have not been used
  }
  backend.io.mem.memoryViolation := memBlock.io.toBackend.memoryViolation
  backend.io.mem.lsqEnqIO <> memBlock.io.fromBackend.enqLsq
  backend.io.mem.sqDeq := memBlock.io.toBackend.sqDeq
  backend.io.mem.lqDeq := memBlock.io.toBackend.lqDeq
  backend.io.mem.sqDeqPtr := memBlock.io.toBackend.sqDeqPtr
  backend.io.mem.lqDeqPtr := memBlock.io.toBackend.lqDeqPtr
  backend.io.mem.lqCancelCnt := memBlock.io.toBackend.lqCancelCnt
  backend.io.mem.sqCancelCnt := memBlock.io.toBackend.sqCancelCnt
  backend.io.mem.otherFastWakeup := memBlock.io.toBackend.otherFastWakeup
  backend.io.mem.stIssuePtr := memBlock.io.toBackend.stIssuePtr
  backend.io.mem.ldaIqFeedback := memBlock.io.toBackend.ldaIqFeedback
  backend.io.mem.staIqFeedback := memBlock.io.toBackend.staIqFeedback
  backend.io.mem.hyuIqFeedback := memBlock.io.toBackend.hyuIqFeedback
  backend.io.mem.vstuIqFeedback := memBlock.io.toBackend.vstuIqFeedback
  backend.io.mem.vlduIqFeedback := memBlock.io.toBackend.vlduIqFeedback
  backend.io.mem.ldCancel := memBlock.io.toBackend.ldCancel
  backend.io.mem.wakeup := memBlock.io.toBackend.wakeup
  backend.io.mem.writebackLda <> memBlock.io.toBackend.writebackLda
  backend.io.mem.writebackSta <> memBlock.io.toBackend.writebackSta
  backend.io.mem.writebackHyuLda <> memBlock.io.toBackend.writebackHyuLda
  backend.io.mem.writebackHyuSta <> memBlock.io.toBackend.writebackHyuSta
  backend.io.mem.writebackStd <> memBlock.io.toBackend.writebackStd
  backend.io.mem.writebackVldu <> memBlock.io.toBackend.writebackVldu
  backend.io.mem.robLsqIO.mmio := memBlock.io.toBackend.lsqio.mmio
  backend.io.mem.robLsqIO.uop := memBlock.io.toBackend.lsqio.uop

  // memblock error exception writeback, 1 cycle after normal writeback
  backend.io.mem.s3_delayed_load_error.foreach(_ := false.B)

  backend.io.mem.exceptionAddr.vaddr  := memBlock.io.toBackend.lsqio.vaddr
  backend.io.mem.exceptionAddr.gpaddr := memBlock.io.toBackend.lsqio.gpaddr
  backend.io.mem.exceptionAddr.isForVSnonLeafPTE := memBlock.io.toBackend.lsqio.isForVSnonLeafPTE
  backend.io.mem.debugLS := memBlock.io.debugLS
  backend.io.mem.lsTopdownInfo := memBlock.io.toBackend.lsTopdownInfo
  backend.io.mem.lqCanAccept := memBlock.io.toBackend.lsqio.lqCanAccept
  backend.io.mem.sqCanAccept := memBlock.io.toBackend.lsqio.sqCanAccept
  backend.io.fenceio.sbuffer.sbIsEmpty := memBlock.io.toBackend.sbIsEmpty

  backend.io.perf.frontendInfo := frontend.io.frontendInfo
  backend.io.perf.memInfo := memBlock.io.memInfo
  backend.io.perf.perfEventsFrontend := frontend.io_perf
  backend.io.perf.perfEventsLsu := memBlock.io_perf
  backend.io.perf.perfEventsHc := memBlock.io.bypass.innerHcPerfEvents
  backend.io.perf.perfEventsBackend := DontCare
  backend.io.perf.retiredInstr := DontCare
  backend.io.perf.ctrlInfo := DontCare

  backend.io.mem.storeDebugInfo <> memBlock.io.toBackend.storeDebugInfo

  // top -> memBlock
  memBlock.io.bypass.fromTopToBackend.clintTime := io.clintTime
  memBlock.io.bypass.fromTopToBackend.msiInfo := io.msiInfo
  memBlock.io.fromCtrl.hartId := io.hartId
  memBlock.io.bypass.outerResetVector := io.reset_vector
  memBlock.io.bypass.outerHcPerfEvents := io.perfEvents
  // frontend -> memBlock
  memBlock.io.bypass.innerBeuErrorsICache <> frontend.io.error.bits.toL1BusErrorUnitInfo(frontend.io.error.valid)
  memBlock.io.bypass.innerL2PfEnable := backend.io.csrCustomCtrl.l2_pf_enable
  memBlock.io.bypass.fromBackendToTop := backend.io.toTop
  memBlock.io.fromBackend.issueLda <> backend.io.mem.issueLda
  memBlock.io.fromBackend.issueSta <> backend.io.mem.issueSta
  memBlock.io.fromBackend.issueStd <> backend.io.mem.issueStd
  memBlock.io.fromBackend.issueHya <> backend.io.mem.issueHylda
  backend.io.mem.issueHysta.foreach(_.ready := false.B) // this fake port should not be used
  memBlock.io.fromBackend.issueVldu <> backend.io.mem.issueVldu

  // By default, instructions do not have exceptions when they enter the function units.
  memBlock.io.fromBackend.issueUops.map(_.bits.uop.clearExceptions())
  memBlock.io.fromBackend.storePc := backend.io.mem.storePcRead
  memBlock.io.fromBackend.hybridPc := backend.io.mem.hyuPcRead
  memBlock.io.fromBackend.flushSb := backend.io.fenceio.sbuffer.flushSb
  memBlock.io.fromBackend.sfence <> backend.io.mem.sfence

  memBlock.io.fromCtrl.redirect := backend.io.mem.redirect
  memBlock.io.fromCtrl.csr := backend.io.mem.csrCtrl
  memBlock.io.fromBackend.tlbCsr := backend.io.mem.tlbCsr
  memBlock.io.fromBackend.robLsqIO.lcommit          := backend.io.mem.robLsqIO.lcommit
  memBlock.io.fromBackend.robLsqIO.scommit          := backend.io.mem.robLsqIO.scommit
  memBlock.io.fromBackend.robLsqIO.pendingMMIOld    := backend.io.mem.robLsqIO.pendingMMIOld
  memBlock.io.fromBackend.robLsqIO.pendingld        := backend.io.mem.robLsqIO.pendingld
  memBlock.io.fromBackend.robLsqIO.pendingst        := backend.io.mem.robLsqIO.pendingst
  memBlock.io.fromBackend.robLsqIO.pendingVst       := backend.io.mem.robLsqIO.pendingVst
  memBlock.io.fromBackend.robLsqIO.commit           := backend.io.mem.robLsqIO.commit
  memBlock.io.fromBackend.robLsqIO.pendingPtr       := backend.io.mem.robLsqIO.pendingPtr
  memBlock.io.fromBackend.robLsqIO.pendingPtrNext   := backend.io.mem.robLsqIO.pendingPtrNext
  memBlock.io.fromBackend.isStoreException       := backend.io.mem.isStoreException
  memBlock.io.fromBackend.isVlsException         := backend.io.mem.isVlsException

  memBlock.io.fromFrontend.itlb <> frontend.io.ptw
  memBlock.io.l2Hint.valid := io.l2_hint.valid
  memBlock.io.l2Hint.bits.sourceId := io.l2_hint.bits.sourceId
  memBlock.io.l2Hint.bits.isKeyword := io.l2_hint.bits.isKeyword
  memBlock.io.l2TlbReq <> io.l2_tlb_req
  memBlock.io.l2PmpResp <> io.l2_pmp_resp
  memBlock.io.l2PfqBusy := io.l2PfqBusy

  // if l2 prefetcher use stream prefetch, it should be placed in XSCore

  // top-down info
  memBlock.io.debugTopDown.robHeadVaddr := backend.io.debugTopDown.fromRob.robHeadVaddr
  frontend.io.debugTopDown.robHeadVaddr := backend.io.debugTopDown.fromRob.robHeadVaddr
  io.debugTopDown.robHeadPaddr := backend.io.debugTopDown.fromRob.robHeadPaddr
  io.debugTopDown.robTrueCommit := backend.io.debugRolling.robTrueCommit
  backend.io.debugTopDown.fromCore.l2MissMatch := io.debugTopDown.l2MissMatch
  backend.io.debugTopDown.fromCore.l3MissMatch := io.debugTopDown.l3MissMatch
  backend.io.debugTopDown.fromCore.fromMem := memBlock.io.debugTopDown.toCore
  memBlock.io.debugRolling := backend.io.debugRolling

  io.cpu_halt := memBlock.io.bypass.outerCpuHalt
  io.cpu_critical_error := memBlock.io.bypass.outerCpuCriticalError
  io.beu_errors.icache <> memBlock.io.bypass.outerBeuErrorsICache
  io.beu_errors.dcache <> memBlock.io.error.bits.toL1BusErrorUnitInfo(memBlock.io.error.valid)
  io.beu_errors.l2 <> DontCare
  io.l2_pf_enable := memBlock.io.bypass.outerL2PfEnable

  memBlock.io.bypass.resetInFrontendBypass.fromFrontend := frontend.io.resetInFrontend
  io.resetInFrontend := memBlock.io.bypass.resetInFrontendBypass.toL2Top
  memBlock.io.bypass.traceCoreInterfaceBypass.fromBackend <> backend.io.traceCoreInterface
  io.traceCoreInterface <> memBlock.io.bypass.traceCoreInterfaceBypass.toL2Top


  if (debugOpts.ResetGen) {
    backend.reset := memBlock.io.bypass.resetBackend
    frontend.reset := backend.io.frontendReset
  }
}
