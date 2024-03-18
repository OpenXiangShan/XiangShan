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
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.HasFPUParameters
import system.HasSoCParameter
import utils._
import utility._
import xiangshan.backend._
import xiangshan.cache.mmu._
import xiangshan.frontend._
import xiangshan.mem.L1PrefetchFuzzer

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

  memBlock.frontendBridge.icache_node := frontend.icache.clientNode
  memBlock.frontendBridge.instr_uncache_node := frontend.instrUncache.clientNode
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
    val hartId = Input(UInt(64.W))
    val reset_vector = Input(UInt(PAddrBits.W))
    val cpu_halt = Output(Bool())
    val l2_pf_enable = Output(Bool())
    val perfEvents = Input(Vec(numPCntHc * coreParams.L2NBanks, new PerfEvent))
    val beu_errors = Output(new XSL1BusErrors())
    val l2_hint = Input(Valid(new L2ToL1Hint()))
    val l2PfqBusy = Input(Bool())
    val debugTopDown = new Bundle {
      val robHeadPaddr = Valid(UInt(PAddrBits.W))
      val l2MissMatch = Input(Bool())
      val l3MissMatch = Input(Bool())
    }
  })

  println(s"FPGAPlatform:${env.FPGAPlatform} EnableDebug:${env.EnableDebug}")

  val frontend = outer.frontend.module
  val backend = outer.backend.module
  val memBlock = outer.memBlock.module

  frontend.io.hartId := memBlock.io.inner_hartId
  frontend.io.reset_vector := memBlock.io.inner_reset_vector
  frontend.io.backend <> backend.io.frontend
  frontend.io.sfence <> backend.io.frontendSfence
  frontend.io.tlbCsr <> backend.io.frontendTlbCsr
  frontend.io.csrCtrl <> backend.io.frontendCsrCtrl
  frontend.io.fencei <> backend.io.fenceio.fencei

  backend.io.fromTop.hartId := memBlock.io.inner_hartId
  backend.io.fromTop.externalInterrupt := memBlock.io.externalInterrupt

  backend.io.frontendCsrDistributedUpdate := frontend.io.csrUpdate

  require(backend.io.mem.stIn.length == memBlock.io.mem_to_ooo.stIn.length)
  backend.io.mem.stIn.zip(memBlock.io.mem_to_ooo.stIn).foreach { case (sink, source) =>
    sink.valid := source.valid
    sink.bits := 0.U.asTypeOf(sink.bits)
    sink.bits.robIdx := source.bits.uop.robIdx
    sink.bits.ssid := source.bits.uop.ssid
    sink.bits.storeSetHit := source.bits.uop.storeSetHit
    // The other signals have not been used
  }
  backend.io.mem.memoryViolation <> memBlock.io.mem_to_ooo.memoryViolation
  backend.io.mem.lsqEnqIO <> memBlock.io.ooo_to_mem.enqLsq
  backend.io.mem.sqDeq := memBlock.io.mem_to_ooo.sqDeq
  backend.io.mem.lqDeq := memBlock.io.mem_to_ooo.lqDeq
  backend.io.mem.sqDeqPtr := memBlock.io.mem_to_ooo.sqDeqPtr
  backend.io.mem.lqDeqPtr := memBlock.io.mem_to_ooo.lqDeqPtr
  backend.io.mem.lqCancelCnt := memBlock.io.mem_to_ooo.lqCancelCnt
  backend.io.mem.sqCancelCnt := memBlock.io.mem_to_ooo.sqCancelCnt
  backend.io.mem.otherFastWakeup := memBlock.io.mem_to_ooo.otherFastWakeup
  backend.io.mem.stIssuePtr := memBlock.io.mem_to_ooo.stIssuePtr
  backend.io.mem.ldaIqFeedback <> memBlock.io.mem_to_ooo.ldaIqFeedback
  backend.io.mem.staIqFeedback <> memBlock.io.mem_to_ooo.staIqFeedback
  backend.io.mem.hyuIqFeedback <> memBlock.io.mem_to_ooo.hyuIqFeedback
  backend.io.mem.ldCancel <> memBlock.io.mem_to_ooo.ldCancel
  backend.io.mem.wakeup <> memBlock.io.mem_to_ooo.wakeup
  backend.io.mem.writebackLda <> memBlock.io.mem_to_ooo.writebackLda
  backend.io.mem.writebackSta <> memBlock.io.mem_to_ooo.writebackSta
  backend.io.mem.writebackHyuLda <> memBlock.io.mem_to_ooo.writebackHyuLda
  backend.io.mem.writebackHyuSta <> memBlock.io.mem_to_ooo.writebackHyuSta
  backend.io.mem.writebackStd <> memBlock.io.mem_to_ooo.writebackStd
  backend.io.mem.writebackVldu <> memBlock.io.mem_to_ooo.writebackVldu
  backend.io.mem.robLsqIO.mmio := memBlock.io.mem_to_ooo.lsqio.mmio
  backend.io.mem.robLsqIO.uop := memBlock.io.mem_to_ooo.lsqio.uop

  // memblock error exception writeback, 1 cycle after normal writeback
  backend.io.mem.s3_delayed_load_error <> memBlock.io.mem_to_ooo.s3_delayed_load_error

  backend.io.mem.exceptionVAddr := memBlock.io.mem_to_ooo.lsqio.vaddr
  backend.io.mem.csrDistributedUpdate := memBlock.io.mem_to_ooo.csrUpdate
  backend.io.mem.debugLS := memBlock.io.debug_ls
  backend.io.mem.lsTopdownInfo := memBlock.io.mem_to_ooo.lsTopdownInfo
  backend.io.mem.lqCanAccept := memBlock.io.mem_to_ooo.lsqio.lqCanAccept
  backend.io.mem.sqCanAccept := memBlock.io.mem_to_ooo.lsqio.sqCanAccept
  backend.io.fenceio.sbuffer.sbIsEmpty := memBlock.io.mem_to_ooo.sbIsEmpty
  // Todo: remove it
  backend.io.fenceio.disableSfence := DontCare

  backend.io.perf.frontendInfo := frontend.io.frontendInfo
  backend.io.perf.memInfo := memBlock.io.memInfo
  backend.io.perf.perfEventsFrontend := frontend.getPerf
  backend.io.perf.perfEventsLsu := memBlock.getPerf
  backend.io.perf.perfEventsHc := io.perfEvents
  backend.io.perf.perfEventsCtrl := DontCare
  backend.io.perf.retiredInstr := DontCare
  backend.io.perf.ctrlInfo := DontCare

  // top -> memBlock
  memBlock.io.hartId := io.hartId
  memBlock.io.outer_reset_vector := io.reset_vector
  // frontend -> memBlock
  memBlock.io.inner_beu_errors_icache <> frontend.io.error.toL1BusErrorUnitInfo()
  memBlock.io.inner_l2_pf_enable := backend.io.csrCustomCtrl.l2_pf_enable
  memBlock.io.inner_cpu_halt := backend.io.toTop.cpuHalted
  memBlock.io.ooo_to_mem.issueLda <> backend.io.mem.issueLda
  memBlock.io.ooo_to_mem.issueSta <> backend.io.mem.issueSta
  memBlock.io.ooo_to_mem.issueStd <> backend.io.mem.issueStd
  memBlock.io.ooo_to_mem.issueHya <> backend.io.mem.issueHylda
  backend.io.mem.issueHysta.map(_.ready := false.B) // this fake port should not be used
  memBlock.io.ooo_to_mem.issueVldu <> backend.io.mem.issueVldu

  // By default, instructions do not have exceptions when they enter the function units.
  memBlock.io.ooo_to_mem.issueUops.map(_.bits.uop.clearExceptions())
  memBlock.io.ooo_to_mem.loadPc := backend.io.mem.loadPcRead
  memBlock.io.ooo_to_mem.storePc := backend.io.mem.storePcRead
  memBlock.io.ooo_to_mem.hybridPc := backend.io.mem.hyuPcRead
  memBlock.io.ooo_to_mem.flushSb := backend.io.fenceio.sbuffer.flushSb
  memBlock.io.ooo_to_mem.loadFastMatch := 0.U.asTypeOf(memBlock.io.ooo_to_mem.loadFastMatch)
  memBlock.io.ooo_to_mem.loadFastImm := 0.U.asTypeOf(memBlock.io.ooo_to_mem.loadFastImm)
  memBlock.io.ooo_to_mem.loadFastFuOpType := 0.U.asTypeOf(memBlock.io.ooo_to_mem.loadFastFuOpType)

  memBlock.io.ooo_to_mem.sfence <> backend.io.mem.sfence

  memBlock.io.redirect <> backend.io.mem.redirect
  memBlock.io.ooo_to_mem.csrCtrl <> backend.io.mem.csrCtrl
  memBlock.io.ooo_to_mem.tlbCsr <> backend.io.mem.tlbCsr
  memBlock.io.ooo_to_mem.lsqio.lcommit        := backend.io.mem.robLsqIO.lcommit
  memBlock.io.ooo_to_mem.lsqio.scommit        := backend.io.mem.robLsqIO.scommit
  memBlock.io.ooo_to_mem.lsqio.pendingld      := backend.io.mem.robLsqIO.pendingld
  memBlock.io.ooo_to_mem.lsqio.pendingst      := backend.io.mem.robLsqIO.pendingst
  memBlock.io.ooo_to_mem.lsqio.commit         := backend.io.mem.robLsqIO.commit
  memBlock.io.ooo_to_mem.lsqio.pendingPtr     := backend.io.mem.robLsqIO.pendingPtr
  memBlock.io.ooo_to_mem.lsqio.pendingPtrNext := backend.io.mem.robLsqIO.pendingPtrNext
  memBlock.io.ooo_to_mem.isStoreException     := backend.io.mem.isStoreException
  memBlock.io.ooo_to_mem.isVlsException       := backend.io.mem.isVlsException

  memBlock.io.fetch_to_mem.itlb <> frontend.io.ptw
  memBlock.io.l2_hint.valid := io.l2_hint.valid
  memBlock.io.l2_hint.bits.sourceId := io.l2_hint.bits.sourceId
  memBlock.io.l2_hint.bits.isKeyword := io.l2_hint.bits.isKeyword
  memBlock.io.l2PfqBusy := io.l2PfqBusy

  // if l2 prefetcher use stream prefetch, it should be placed in XSCore

  // top-down info
  memBlock.io.debugTopDown.robHeadVaddr := backend.io.debugTopDown.fromRob.robHeadVaddr
  frontend.io.debugTopDown.robHeadVaddr := backend.io.debugTopDown.fromRob.robHeadVaddr
  io.debugTopDown.robHeadPaddr := backend.io.debugTopDown.fromRob.robHeadPaddr
  backend.io.debugTopDown.fromCore.l2MissMatch := io.debugTopDown.l2MissMatch
  backend.io.debugTopDown.fromCore.l3MissMatch := io.debugTopDown.l3MissMatch
  backend.io.debugTopDown.fromCore.fromMem := memBlock.io.debugTopDown.toCore
  memBlock.io.debugRolling := backend.io.debugRolling

  io.cpu_halt := memBlock.io.outer_cpu_halt
  io.beu_errors.icache <> memBlock.io.outer_beu_errors_icache
  io.beu_errors.dcache <> memBlock.io.error.toL1BusErrorUnitInfo()
  io.beu_errors.l2 <> DontCare
  io.l2_pf_enable := memBlock.io.outer_l2_pf_enable
  // Modules are reset one by one
  val resetTree = ResetGenNode(
    Seq(
      ModuleNode(memBlock),
      ResetGenNode(Seq(
        ModuleNode(backend),
        ResetGenNode(Seq(
          ResetGenNode(Seq(
            ModuleNode(frontend)
          ))
        ))
      ))
    )
  )

  // ResetGen(resetTree, reset, !debugOpts.FPGAPlatform)
  if (debugOpts.FPGAPlatform) {
    frontend.reset := memBlock.reset_io_frontend
    backend.reset := memBlock.reset_io_backend
  }
}
