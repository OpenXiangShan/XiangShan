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
import xiangshan.backend.exu.{ExuConfig, Wb2Ctrl, WbArbiterWrapper}
import xiangshan.frontend._
import xiangshan.mem.L1PrefetchFuzzer

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

class WritebackSourceParams(
  var exuConfigs: Seq[Seq[ExuConfig]] = Seq()
 ) {
  def length: Int = exuConfigs.length
  def ++(that: WritebackSourceParams): WritebackSourceParams = {
    new WritebackSourceParams(exuConfigs ++ that.exuConfigs)
  }
}

trait HasWritebackSource {
  val writebackSourceParams: Seq[WritebackSourceParams]
  final def writebackSource(sourceMod: HasWritebackSourceImp): Seq[Seq[Valid[ExuOutput]]] = {
    require(sourceMod.writebackSource.isDefined, "should not use Valid[ExuOutput]")
    val source = sourceMod.writebackSource.get
    require(source.length == writebackSourceParams.length, "length mismatch between sources")
    for ((s, p) <- source.zip(writebackSourceParams)) {
      require(s.length == p.length, "params do not match with the exuOutput")
    }
    source
  }
  final def writebackSource1(sourceMod: HasWritebackSourceImp): Seq[Seq[DecoupledIO[ExuOutput]]] = {
    require(sourceMod.writebackSource1.isDefined, "should not use DecoupledIO[ExuOutput]")
    val source = sourceMod.writebackSource1.get
    require(source.length == writebackSourceParams.length, "length mismatch between sources")
    for ((s, p) <- source.zip(writebackSourceParams)) {
      require(s.length == p.length, "params do not match with the exuOutput")
    }
    source
  }
  val writebackSourceImp: HasWritebackSourceImp
}

trait HasWritebackSourceImp {
  def writebackSource: Option[Seq[Seq[Valid[ExuOutput]]]] = None
  def writebackSource1: Option[Seq[Seq[DecoupledIO[ExuOutput]]]] = None
}

trait HasWritebackSink {
  // Caches all sources. The selected source will be the one with smallest length.
  var writebackSinks = ListBuffer.empty[(Seq[HasWritebackSource], Seq[Int])]
  def addWritebackSink(source: Seq[HasWritebackSource], index: Option[Seq[Int]] = None): HasWritebackSink = {
    val realIndex = if (index.isDefined) index.get else Seq.fill(source.length)(0)
    writebackSinks += ((source, realIndex))
    this
  }

  def writebackSinksParams: Seq[WritebackSourceParams] = {
    writebackSinks.map{ case (s, i) => s.zip(i).map(x => x._1.writebackSourceParams(x._2)).reduce(_ ++ _) }.toSeq
  }
  final def writebackSinksMod(
     thisMod: Option[HasWritebackSource] = None,
     thisModImp: Option[HasWritebackSourceImp] = None
   ): Seq[Seq[HasWritebackSourceImp]] = {
    require(thisMod.isDefined == thisModImp.isDefined)
    writebackSinks.map(_._1.map(source =>
      if (thisMod.isDefined && source == thisMod.get) thisModImp.get else source.writebackSourceImp)
    ).toSeq
  }
  final def writebackSinksImp(
    thisMod: Option[HasWritebackSource] = None,
    thisModImp: Option[HasWritebackSourceImp] = None
  ): Seq[Seq[ValidIO[ExuOutput]]] = {
    val sourceMod = writebackSinksMod(thisMod, thisModImp)
    writebackSinks.zip(sourceMod).map{ case ((s, i), m) =>
      s.zip(i).zip(m).flatMap(x => x._1._1.writebackSource(x._2)(x._1._2))
    }.toSeq
  }
  def selWritebackSinks(func: WritebackSourceParams => Int): Int = {
    writebackSinksParams.zipWithIndex.minBy(params => func(params._1))._2
  }
  def generateWritebackIO(
    thisMod: Option[HasWritebackSource] = None,
    thisModImp: Option[HasWritebackSourceImp] = None
   ): Unit
}

abstract class XSBundle(implicit val p: Parameters) extends Bundle
  with HasXSParameter

abstract class XSCoreBase()(implicit p: config.Parameters) extends LazyModule
  with HasXSParameter with HasExuWbHelper
{
  override def shouldBeInlined: Boolean = false
  // outer facing nodes
  val frontend = LazyModule(new Frontend())
  val csrOut = BundleBridgeSource(Some(() => new DistributedCSRIO()))

  val memBlock = LazyModule(new MemBlock()(p.alter((site, here, up) => {
    case XSCoreParamsKey => up(XSCoreParamsKey).copy(
      IssQueSize = IssQueSize * 2 // exuBlocks.head.scheduler.getMemRsEntries
    )
  })))

  val backend = LazyModule(new Backend(memBlock)(p))

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

  private val frontend = outer.frontend.module
  private val backend = outer.backend.module
  private val memBlock = outer.memBlock.module

  frontend.io.hartId  := memBlock.io.inner_hartId
  backend.io.hartId := memBlock.io.inner_hartId
  memBlock.io.hartId := io.hartId
  memBlock.io.outer_reset_vector := io.reset_vector
  frontend.io.reset_vector := memBlock.io.inner_reset_vector

  memBlock.io.inner_cpu_halt := backend.io.cpu_halt
  io.cpu_halt := memBlock.io.outer_cpu_halt

  backend.io.memBlock.writeback <> memBlock.io.mem_to_ooo.writeback

  // memblock error exception writeback, 1 cycle after normal writeback
  backend.io.memBlock.s3_delayed_load_error <> memBlock.io.s3_delayed_load_error

  memBlock.io.inner_beu_errors_icache <> frontend.io.error.toL1BusErrorUnitInfo()
  io.beu_errors.icache <> memBlock.io.outer_beu_errors_icache
  io.beu_errors.dcache <> memBlock.io.error.toL1BusErrorUnitInfo()
  io.beu_errors.l2 <> DontCare

  frontend.io.backend <> backend.io.frontend.frontend2Ctrl
  frontend.io.sfence <> backend.io.frontend.sfence
  frontend.io.tlbCsr <> backend.io.frontend.tlbCsr
  frontend.io.csrCtrl <> backend.io.frontend.csrCtrl
  frontend.io.fencei := backend.io.frontend.fencei

  backend.io.memBlock.stIn <> memBlock.io.mem_to_ooo.stIn
  backend.io.memBlock.memoryViolation <> memBlock.io.mem_to_ooo.memoryViolation
  backend.io.memBlock.enqLsq <> memBlock.io.ooo_to_mem.enqLsq
  backend.io.memBlock.lcommit := memBlock.io.mem_to_ooo.lqDeq
  backend.io.memBlock.scommit := memBlock.io.mem_to_ooo.sqDeq
  backend.io.memBlock.lqCancelCnt := memBlock.io.mem_to_ooo.lqCancelCnt
  backend.io.memBlock.sqCancelCnt := memBlock.io.mem_to_ooo.sqCancelCnt
  backend.io.memBlock.otherFastWakeup <> memBlock.io.mem_to_ooo.otherFastWakeup
  backend.io.memBlock.stIssuePtr := memBlock.io.mem_to_ooo.stIssuePtr

  memBlock.io.ooo_to_mem.issue <> backend.io.memBlock.issue
  memBlock.io.ooo_to_mem.loadFastMatch <> backend.io.memBlock.loadFastMatch
  memBlock.io.ooo_to_mem.loadFastFuOpType <> backend.io.memBlock.loadFastFuOpType
  memBlock.io.ooo_to_mem.loadFastImm <> backend.io.memBlock.loadFastImm
  memBlock.io.ooo_to_mem.loadPc <> backend.io.memBlock.loadPc
  memBlock.io.ooo_to_mem.storePc <> backend.io.memBlock.storePc

  backend.io.perf <> DontCare
  backend.io.perf.memInfo <> memBlock.io.memInfo
  backend.io.perf.frontendInfo <> frontend.io.frontendInfo

  backend.io.externalInterrupt := memBlock.io.externalInterrupt

  backend.io.distributedUpdate(0).w.valid := memBlock.io.mem_to_ooo.csrUpdate.w.valid
  backend.io.distributedUpdate(0).w.bits := memBlock.io.mem_to_ooo.csrUpdate.w.bits
  backend.io.distributedUpdate(1).w.valid := frontend.io.csrUpdate.w.valid
  backend.io.distributedUpdate(1).w.bits := frontend.io.csrUpdate.w.bits

  backend.io.memBlock.sfence <> memBlock.io.ooo_to_mem.sfence
  memBlock.io.fetch_to_mem.itlb <> frontend.io.ptw
  memBlock.io.ooo_to_mem.flushSb := backend.io.memBlock.fenceToSbuffer.flushSb
  backend.io.memBlock.fenceToSbuffer.sbIsEmpty := memBlock.io.mem_to_ooo.sbIsEmpty

  memBlock.io.redirect <> backend.io.memBlock.redirect
  memBlock.io.rsfeedback <> backend.io.memBlock.rsfeedback
  memBlock.io.ooo_to_mem.csrCtrl <> backend.io.memBlock.csrCtrl
  memBlock.io.ooo_to_mem.tlbCsr <> backend.io.memBlock.tlbCsr

  memBlock.io.ooo_to_mem.lsqio.lcommit    := backend.io.memBlock.lsqio.rob.lcommit
  memBlock.io.ooo_to_mem.lsqio.scommit    := backend.io.memBlock.lsqio.rob.scommit
  memBlock.io.ooo_to_mem.lsqio.pendingld  := backend.io.memBlock.lsqio.rob.pendingld
  memBlock.io.ooo_to_mem.lsqio.pendingst  := backend.io.memBlock.lsqio.rob.pendingst
  memBlock.io.ooo_to_mem.lsqio.commit     := backend.io.memBlock.lsqio.rob.commit
  memBlock.io.ooo_to_mem.lsqio.pendingPtr := backend.io.memBlock.lsqio.rob.pendingPtr

  backend.io.memBlock.lsqio.exceptionAddr.vaddr := memBlock.io.mem_to_ooo.lsqio.vaddr
  backend.io.memBlock.lsqio.rob.mmio      := memBlock.io.mem_to_ooo.lsqio.mmio
  backend.io.memBlock.lsqio.rob.uop       := memBlock.io.mem_to_ooo.lsqio.uop
  backend.io.memBlock.lsqio.lqCanAccept   := memBlock.io.mem_to_ooo.lsqio.lqCanAccept
  backend.io.memBlock.lsqio.sqCanAccept   := memBlock.io.mem_to_ooo.lsqio.sqCanAccept

  memBlock.io.ooo_to_mem.isStore := backend.io.memBlock.lsqio.exceptionAddr.isStore
  memBlock.io.debug_ls <> backend.io.memBlock.debug_ls
  memBlock.io.mem_to_ooo.lsTopdownInfo <> backend.io.memBlock.lsTopdownInfo

  memBlock.io.l2_hint.valid := io.l2_hint.valid
  memBlock.io.l2_hint.bits.sourceId := io.l2_hint.bits.sourceId
  memBlock.io.l2PfqBusy := io.l2PfqBusy
  memBlock.io.int2vlsu <> DontCare
  memBlock.io.vec2vlsu <> DontCare
  memBlock.io.vlsu2vec <> DontCare
  memBlock.io.vlsu2int <> DontCare
  memBlock.io.vlsu2ctrl <> DontCare

  // if l2 prefetcher use stream prefetch, it should be placed in XSCore
  memBlock.io.inner_l2_pf_enable := backend.io.l2_pf_enable
  io.l2_pf_enable := memBlock.io.outer_l2_pf_enable

  // top-down info
  memBlock.io.debugTopDown.robHeadVaddr := backend.io.debugTopDown.fromRob.robHeadVaddr
  frontend.io.debugTopDown.robHeadVaddr := backend.io.debugTopDown.fromRob.robHeadVaddr
  io.debugTopDown.robHeadPaddr := backend.io.debugTopDown.fromRob.robHeadPaddr
  backend.io.debugTopDown.fromCore.l2MissMatch := io.debugTopDown.l2MissMatch
  backend.io.debugTopDown.fromCore.l3MissMatch := io.debugTopDown.l3MissMatch
  backend.io.debugTopDown.fromCore.fromMem := memBlock.io.debugTopDown.toCore
  memBlock.io.debugRolling := backend.io.debugRolling

  // Modules are reset one by one
  // val resetTree = ResetGenNode(
  //   Seq(
  //     ModuleNode(memBlock),
  //     ResetGenNode(Seq(
  //       ModuleNode(exuBlocks.head),
  //       ResetGenNode(
  //         exuBlocks.tail.map(m => ModuleNode(m)) :+ ModuleNode(outer.wbArbiter.module)
  //       ),
  //       ResetGenNode(Seq(
  //         ModuleNode(ctrlBlock),
  //         ResetGenNode(Seq(
  //           ModuleNode(frontend)
  //         ))
  //       ))
  //     ))
  //   )
  // )

  // ResetGen(resetTree, reset, !debugOpts.FPGAPlatform)
  if (debugOpts.FPGAPlatform) {
    frontend.reset := memBlock.reset_io_frontend
    backend.reset := memBlock.reset_io_backend
  }

}
