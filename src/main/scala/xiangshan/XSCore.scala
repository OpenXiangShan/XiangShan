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

import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
import freechips.rocketchip.tile.HasFPUParameters
import huancun.debug.TLLogger
import huancun.utils.{ModuleNode, ResetGen, ResetGenNode}
import system.HasSoCParameter
import utils._
import xiangshan.backend._
import xiangshan.backend.exu.{ExuConfig, Wb2Ctrl, WbArbiterWrapper}
import xiangshan.frontend._

import scala.collection.mutable.ListBuffer

abstract class XSModule(implicit val p: Parameters) extends MultiIOModule
  with HasXSParameter
  with HasFPUParameters {
  def io: Record
}

//remove this trait after impl module logic
trait NeedImpl {
  this: RawModule =>
  override protected def IO[T <: Data](iodef: T): T = {
    println(s"[Warn]: (${this.name}) please reomve 'NeedImpl' after implement this module")
    val io = chisel3.experimental.IO(iodef)
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
    writebackSinks.map{ case (s, i) => s.zip(i).map(x => x._1.writebackSourceParams(x._2)).reduce(_ ++ _) }
  }
  final def writebackSinksMod(
     thisMod: Option[HasWritebackSource] = None,
     thisModImp: Option[HasWritebackSourceImp] = None
   ): Seq[Seq[HasWritebackSourceImp]] = {
    require(thisMod.isDefined == thisModImp.isDefined)
    writebackSinks.map(_._1.map(source =>
      if (thisMod.isDefined && source == thisMod.get) thisModImp.get else source.writebackSourceImp)
    )
  }
  final def writebackSinksImp(
    thisMod: Option[HasWritebackSource] = None,
    thisModImp: Option[HasWritebackSourceImp] = None
  ): Seq[Seq[ValidIO[ExuOutput]]] = {
    val sourceMod = writebackSinksMod(thisMod, thisModImp)
    writebackSinks.zip(sourceMod).map{ case ((s, i), m) =>
      s.zip(i).zip(m).flatMap(x => x._1._1.writebackSource(x._2)(x._1._2))
    }
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
  // interrupt sinks
  val clint_int_sink = IntSinkNode(IntSinkPortSimple(1, 2))
  val debug_int_sink = IntSinkNode(IntSinkPortSimple(1, 1))
  val plic_int_sink = IntSinkNode(IntSinkPortSimple(2, 1))
  // outer facing nodes
  val frontend = LazyModule(new Frontend())
  val csrOut = BundleBridgeSource(Some(() => new DistributedCSRIO()))
  val backendTop = LazyModule(new BackendTop)

  val memBlock = LazyModule(new MemBlock()(p.alter((site, here, up) => {
    case XSCoreParamsKey => up(XSCoreParamsKey).copy(
      IssQueSize = backendTop.exuBlocks.head.scheduler.memRsEntries.max
    )
  })))
  //port-merge
  memBlock.l1i_to_l2_buf_node :=
    frontend.icache.clientNode

  memBlock.i_mmio_port :=
    frontend.instrUncache.clientNode
}

class ResetAgent extends Module{
  //temp empty module for port merge
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
    val cpu_halt = Output(Bool())
    val l2_pf_enable = Output(Bool())
    val perfEvents = Input(Vec(numPCntHc * coreParams.L2NBanks, new PerfEvent))
    val beu_errors = Output(new XSL1BusErrors())
  })

  println(s"FPGAPlatform:${env.FPGAPlatform} EnableDebug:${env.EnableDebug}")

  val frontend = outer.frontend.module
  val backendTop = outer.backendTop.module
  val memBlock = outer.memBlock.module

  memBlock.io.hartId := io.hartId
  frontend.io.hartId  := memBlock.io.hartId_out_frontend
  backendTop.io.hartId := memBlock.io.hartId_out_backend

  io.cpu_halt := memBlock.io.cpu_halt_out

  // memblock error exception writeback, 1 cycle after normal writeback
  backendTop.io.s3_delayed_load_error <> memBlock.io.s3_delayed_load_error

  //  io.beu_errors.icache <> frontend.io.error.toL1BusErrorUnitInfo()
  //  io.beu_errors.dcache <> memBlock.io.error.toL1BusErrorUnitInfo()

  memBlock.io.beu_errors.icache <> frontend.io.error.toL1BusErrorUnitInfo()
  memBlock.io.beu_errors.dcache <> memBlock.io.error.toL1BusErrorUnitInfo()
  io.beu_errors <> memBlock.io.beu_errors

  val csrioIn = backendTop.io.csrio
  val fenceio = backendTop.io.fenceio

  frontend.io.backend <> backendTop.io.frontend
  frontend.io.sfence <> fenceio.sfence
  frontend.io.tlbCsr <> csrioIn.tlb
  frontend.io.csrCtrl <> csrioIn.customCtrl
  frontend.io.fencei := fenceio.fencei

  backendTop.io.stIn <> memBlock.io.stIn
  backendTop.io.memoryViolation <> memBlock.io.memoryViolation
  backendTop.io.enqLsq <> memBlock.io.enqLsq
  backendTop.io.sqDeq := memBlock.io.sqDeq
  backendTop.io.lqCancelCnt := memBlock.io.lqCancelCnt
  backendTop.io.sqCancelCnt := memBlock.io.sqCancelCnt

  backendTop.io.otherFastWakeup <> memBlock.io.otherFastWakeup

  // return load pc at load s2
  memBlock.io.loadPc <> VecInit(backendTop.io.ld_pc_read.map(_.data))
  memBlock.io.issue <> backendTop.io.issue

  // By default, instructions do not have exceptions when they enter the function units.
  memBlock.io.issue.map(_.bits.uop.clearExceptions())
  backendTop.io.loadFastMatch <> memBlock.io.loadFastMatch
  backendTop.io.loadFastImm <> memBlock.io.loadFastImm
  backendTop.io.stIssuePtr <> memBlock.io.stIssuePtr
  backendTop.io.stIn := memBlock.io.stIn

  csrioIn.hartId <> memBlock.io.hartId_out_backend
  csrioIn.perf <> DontCare
  csrioIn.perf.memInfo <> memBlock.io.memInfo
  csrioIn.perf.frontendInfo <> frontend.io.frontendInfo

  csrioIn.perf.perfEventsFrontend <> frontend.getPerf
  csrioIn.perf.perfEventsLsu      <> memBlock.getPerf
  memBlock.io.perfEventsHc_in     <> io.perfEvents
  csrioIn.perf.perfEventsHc       <> memBlock.io.perfEventsHc_out

  csrioIn.memExceptionVAddr <> memBlock.io.lsqio.exceptionAddr.vaddr

  csrioIn.externalInterrupt.msip := outer.clint_int_sink.in.head._1(0)
  csrioIn.externalInterrupt.mtip := outer.clint_int_sink.in.head._1(1)
  csrioIn.externalInterrupt.meip := outer.plic_int_sink.in.head._1(0)
  csrioIn.externalInterrupt.seip := outer.plic_int_sink.in.last._1(0)
  csrioIn.externalInterrupt.debug := outer.debug_int_sink.in.head._1(0)

  csrioIn.distributedUpdate(0).w.valid := memBlock.io.csrUpdate.w.valid
  csrioIn.distributedUpdate(0).w.bits := memBlock.io.csrUpdate.w.bits
  csrioIn.distributedUpdate(1).w.valid := frontend.io.csrUpdate.w.valid
  csrioIn.distributedUpdate(1).w.bits := frontend.io.csrUpdate.w.bits

  fenceio.sfence <> memBlock.io.sfence
  fenceio.sbuffer <> memBlock.io.fenceToSbuffer

  memBlock.io.redirect <> backendTop.io.redirect
  memBlock.io.rsfeedback <> backendTop.io.rsfeedback
  memBlock.io.csrCtrl <> csrioIn.customCtrl
  memBlock.io.tlbCsr <> csrioIn.tlb
  memBlock.io.itlb <> frontend.io.ptw
  memBlock.io.lsqio.rob <> backendTop.io.robio.lsq
  memBlock.io.lsqio.exceptionAddr.isStore := CommitType.lsInstIsStore(backendTop.io.robio.exception.bits.uop.ctrl.commitType)

  // if l2 prefetcher use stream prefetch, it should be placed in XSCore
  io.l2_pf_enable := csrioIn.customCtrl.l2_pf_enable

  // Modules are reset one by one
//  val resetTree = ResetGenNode(
//    Seq(
//      ModuleNode(memBlock),
//      ResetGenNode(Seq(
//        ModuleNode(backend_rst_agent),
//        ResetGenNode(Seq(
//          ResetGenNode(Seq(
//            ModuleNode(frontend_rst_agent)
//          ))
//        ))
//      ))
//    )
//  )

  frontend.reset := memBlock.reset_sink_frontend
  backendTop.reset := memBlock.reset_sink_backend

  backendTop.io.writeback <> memBlock.io.writeback
}
