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
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
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
  // interrupt sinks
  val clint_int_sink = IntSinkNode(IntSinkPortSimple(1, 2))
  val debug_int_sink = IntSinkNode(IntSinkPortSimple(1, 1))
  val plic_int_sink = IntSinkNode(IntSinkPortSimple(2, 1))
  // outer facing nodes
  val frontend = LazyModule(new Frontend())
  val csrOut = BundleBridgeSource(Some(() => new DistributedCSRIO()))
  val backend = LazyModule(new Backend(backendParams))

  val memBlock = LazyModule(new MemBlock)
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

  val fenceio = backend.io.fenceio
  fenceio.disableSfence := DontCare

  frontend.io.hartId  := io.hartId
  frontend.io.backend <> backend.io.frontend
  frontend.io.sfence <> backend.io.frontendSfence
  frontend.io.tlbCsr <> backend.io.frontendTlbCsr
  frontend.io.csrCtrl <> backend.io.frontendCsrCtrl
  frontend.io.fencei <> fenceio.fencei

  backend.io.fromTop.hartId := io.hartId
  backend.io.fromTop.externalInterrupt.msip := outer.clint_int_sink.in.head._1(0)
  backend.io.fromTop.externalInterrupt.mtip := outer.clint_int_sink.in.head._1(1)
  backend.io.fromTop.externalInterrupt.meip := outer.plic_int_sink.in.head._1(0)
  backend.io.fromTop.externalInterrupt.seip := outer.plic_int_sink.in.last._1(0)
  backend.io.fromTop.externalInterrupt.debug := outer.debug_int_sink.in.head._1(0)

  backend.io.frontendCsrDistributedUpdate := frontend.io.csrUpdate

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
  backend.io.mem.lqCancelCnt := memBlock.io.mem_to_ooo.lqCancelCnt
  backend.io.mem.sqCancelCnt := memBlock.io.mem_to_ooo.sqCancelCnt
  backend.io.mem.otherFastWakeup := memBlock.io.mem_to_ooo.otherFastWakeup
  backend.io.mem.stIssuePtr := memBlock.io.mem_to_ooo.stIssuePtr
  backend.io.mem.ldaIqFeedback <> memBlock.io.mem_to_ooo.ldaIqFeedback
  backend.io.mem.staIqFeedback <> memBlock.io.mem_to_ooo.staIqFeedback
  backend.io.mem.hyuIqFeedback <> memBlock.io.mem_to_ooo.hyuIqFeedback
  backend.io.mem.ldCancel <> memBlock.io.mem_to_ooo.ldCancel
  backend.io.mem.writebackLda <> memBlock.io.mem_to_ooo.writebackLda
  backend.io.mem.writebackSta <> memBlock.io.mem_to_ooo.writebackSta
  backend.io.mem.writebackHyuLda <> memBlock.io.mem_to_ooo.writebackHyuLda
  backend.io.mem.writebackHyuSta <> memBlock.io.mem_to_ooo.writebackHyuSta
  backend.io.mem.writebackStd <> memBlock.io.mem_to_ooo.writebackStd
  backend.io.mem.writebackVlda <> memBlock.io.mem_to_ooo.writebackVlda
  backend.io.mem.robLsqIO.mmio := memBlock.io.mem_to_ooo.lsqio.mmio
  backend.io.mem.robLsqIO.uop := memBlock.io.mem_to_ooo.lsqio.uop

  frontend.io.reset_vector := io.reset_vector

  io.cpu_halt := backend.io.toTop.cpuHalted

  // memblock error exception writeback, 1 cycle after normal writeback
  backend.io.mem.s3_delayed_load_error <> memBlock.io.mem_to_ooo.s3_delayed_load_error

  io.beu_errors.icache <> frontend.io.error.toL1BusErrorUnitInfo()
  io.beu_errors.dcache <> memBlock.io.error.toL1BusErrorUnitInfo()
  io.beu_errors.l2 <> DontCare

  memBlock.io.hartId := io.hartId
  memBlock.io.ooo_to_mem.issueLda <> backend.io.mem.issueLda
  memBlock.io.ooo_to_mem.issueSta <> backend.io.mem.issueSta
  memBlock.io.ooo_to_mem.issueStd <> backend.io.mem.issueStd
  memBlock.io.ooo_to_mem.issueHya <> backend.io.mem.issueHylda
  backend.io.mem.issueHysta.map(_.ready := false.B)
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

  backend.io.mem.exceptionVAddr := memBlock.io.mem_to_ooo.lsqio.vaddr
  backend.io.mem.csrDistributedUpdate := memBlock.io.mem_to_ooo.csrUpdate
  backend.io.mem.debugLS := memBlock.io.debug_ls
  backend.io.mem.lsTopdownInfo := memBlock.io.mem_to_ooo.lsTopdownInfo
  backend.io.mem.lqCanAccept := memBlock.io.mem_to_ooo.lsqio.lqCanAccept
  backend.io.mem.sqCanAccept := memBlock.io.mem_to_ooo.lsqio.sqCanAccept
  backend.io.fenceio.sbuffer.sbIsEmpty := memBlock.io.mem_to_ooo.sbIsEmpty

  backend.io.perf.frontendInfo := frontend.io.frontendInfo
  backend.io.perf.memInfo := memBlock.io.memInfo
  backend.io.perf.perfEventsFrontend := frontend.getPerf
  backend.io.perf.perfEventsLsu := memBlock.getPerf
  backend.io.perf.perfEventsHc := io.perfEvents
  backend.io.perf.perfEventsCtrl := DontCare
  backend.io.perf.retiredInstr := DontCare
  backend.io.perf.ctrlInfo := DontCare


  memBlock.io.ooo_to_mem.sfence <> backend.io.mem.sfence

  memBlock.io.redirect <> backend.io.mem.redirect
  memBlock.io.ooo_to_mem.csrCtrl <> backend.io.mem.csrCtrl
  memBlock.io.ooo_to_mem.tlbCsr <> backend.io.mem.tlbCsr
  memBlock.io.ooo_to_mem.lsqio.lcommit    := backend.io.mem.robLsqIO.lcommit
  memBlock.io.ooo_to_mem.lsqio.scommit    := backend.io.mem.robLsqIO.scommit
  memBlock.io.ooo_to_mem.lsqio.pendingld  := backend.io.mem.robLsqIO.pendingld
  memBlock.io.ooo_to_mem.lsqio.pendingst  := backend.io.mem.robLsqIO.pendingst
  memBlock.io.ooo_to_mem.lsqio.commit     := backend.io.mem.robLsqIO.commit
  memBlock.io.ooo_to_mem.lsqio.pendingPtr := backend.io.mem.robLsqIO.pendingPtr
  memBlock.io.ooo_to_mem.isStore          := backend.io.mem.isStoreException

  memBlock.io.fetch_to_mem.itlb <> frontend.io.ptw
  memBlock.io.l2_hint.valid := io.l2_hint.valid
  memBlock.io.l2_hint.bits.sourceId := io.l2_hint.bits.sourceId
  memBlock.io.l2PfqBusy := io.l2PfqBusy
  memBlock.io.int2vlsu <> DontCare
  memBlock.io.vec2vlsu <> DontCare
  memBlock.io.vlsu2vec <> DontCare
  memBlock.io.vlsu2int <> DontCare
  memBlock.io.vlsu2ctrl <> DontCare

  // TODO: Connect us when implemented
  memBlock.io.int2vlsu  <> DontCare
  memBlock.io.vec2vlsu  <> DontCare
  memBlock.io.vlsu2vec  <> DontCare
  memBlock.io.vlsu2int  <> DontCare
  memBlock.io.vlsu2ctrl <> DontCare

  // if l2 prefetcher use stream prefetch, it should be placed in XSCore
  io.l2_pf_enable := backend.io.csrCustomCtrl.l2_pf_enable

  // top-down info
  memBlock.io.debugTopDown.robHeadVaddr := backend.io.debugTopDown.fromRob.robHeadVaddr
  frontend.io.debugTopDown.robHeadVaddr := backend.io.debugTopDown.fromRob.robHeadVaddr
  io.debugTopDown.robHeadPaddr := backend.io.debugTopDown.fromRob.robHeadPaddr
  backend.io.debugTopDown.fromCore.l2MissMatch := io.debugTopDown.l2MissMatch
  backend.io.debugTopDown.fromCore.l3MissMatch := io.debugTopDown.l3MissMatch
  backend.io.debugTopDown.fromCore.fromMem := memBlock.io.debugTopDown.toCore
  memBlock.io.debugRolling := backend.io.debugRolling

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

  ResetGen(resetTree, reset, !debugOpts.FPGAPlatform)

}
