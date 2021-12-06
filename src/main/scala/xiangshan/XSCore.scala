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

import chisel3._
import chisel3.util._
import xiangshan.backend._
import xiangshan.backend.fu.HasExceptionNO
import xiangshan.backend.exu.WbArbiterWrapper
import xiangshan.frontend._
import xiangshan.cache.mmu._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import freechips.rocketchip.interrupts.{IntSinkNode, IntSinkPortSimple}
import freechips.rocketchip.tile.HasFPUParameters
import system.HasSoCParameter
import utils._

abstract class XSModule(implicit val p: Parameters) extends MultiIOModule
  with HasXSParameter
  with HasExceptionNO
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

abstract class XSBundle(implicit val p: Parameters) extends Bundle
  with HasXSParameter

abstract class XSCoreBase()(implicit p: config.Parameters) extends LazyModule
  with HasXSParameter with HasExuWbMappingHelper
{
  // interrupt sinks
  val clint_int_sink = IntSinkNode(IntSinkPortSimple(1, 2))
  val debug_int_sink = IntSinkNode(IntSinkPortSimple(1, 1))
  val plic_int_sink = IntSinkNode(IntSinkPortSimple(2, 1))
  // outer facing nodes
  val frontend = LazyModule(new Frontend())
  val ptw = LazyModule(new PTWWrapper())
  val csrOut = BundleBridgeSource(Some(() => new DistributedCSRIO()))

  val wbArbiter = LazyModule(new WbArbiterWrapper(exuConfigs, NRIntWritePorts, NRFpWritePorts))
  val intWbPorts = wbArbiter.intWbPorts
  val fpWbPorts = wbArbiter.fpWbPorts

  // TODO: better RS organization
  // generate rs according to number of function units
  require(exuParameters.JmpCnt == 1)
  require(exuParameters.MduCnt <= exuParameters.AluCnt && exuParameters.MduCnt > 0)
  require(exuParameters.FmiscCnt <= exuParameters.FmacCnt && exuParameters.FmiscCnt > 0)
  require(exuParameters.LduCnt == 2 && exuParameters.StuCnt == 2)

  // one RS every 2 MDUs
  val schedulePorts = Seq(
    // exuCfg, numDeq, intFastWakeupTarget, fpFastWakeupTarget
    Seq(
      (AluExeUnitCfg, exuParameters.AluCnt, Seq(AluExeUnitCfg, MulDivExeUnitCfg, JumpCSRExeUnitCfg, LdExeUnitCfg, StaExeUnitCfg), Seq()),
      (MulDivExeUnitCfg, exuParameters.MduCnt, Seq(AluExeUnitCfg, MulDivExeUnitCfg), Seq()),
      (JumpCSRExeUnitCfg, 1, Seq(), Seq()),
      (LdExeUnitCfg, exuParameters.LduCnt, Seq(AluExeUnitCfg, LdExeUnitCfg), Seq()),
      (StaExeUnitCfg, exuParameters.StuCnt, Seq(), Seq()),
      (StdExeUnitCfg, exuParameters.StuCnt, Seq(), Seq())
    ),
    Seq(
      (FmacExeUnitCfg, exuParameters.FmacCnt, Seq(), Seq(FmacExeUnitCfg, FmiscExeUnitCfg)),
      (FmiscExeUnitCfg, exuParameters.FmiscCnt, Seq(), Seq())
    )
  )

  // should do outer fast wakeup ports here
  val otherFastPorts = schedulePorts.zipWithIndex.map { case (sche, i) =>
    val otherCfg = schedulePorts.zipWithIndex.filter(_._2 != i).map(_._1).reduce(_ ++ _)
    val outerPorts = sche.map(cfg => {
      // exe units from this scheduler need fastUops from exeunits
      val outerWakeupInSche = sche.filter(_._1.wakeupFromExu)
      val intraIntScheOuter = outerWakeupInSche.filter(_._3.contains(cfg._1)).map(_._1)
      val intraFpScheOuter = outerWakeupInSche.filter(_._4.contains(cfg._1)).map(_._1)
      // exe units from other schedulers need fastUop from outside
      val otherIntSource = otherCfg.filter(_._3.contains(cfg._1)).map(_._1)
      val otherFpSource = otherCfg.filter(_._4.contains(cfg._1)).map(_._1)
      val intSource = findInWbPorts(intWbPorts, intraIntScheOuter ++ otherIntSource)
      val fpSource = findInWbPorts(fpWbPorts, intraFpScheOuter ++ otherFpSource)
      getFastWakeupIndex(cfg._1, intSource, fpSource, intWbPorts.length).sorted
    })
    println(s"inter-scheduler wakeup sources for $i: $outerPorts")
    outerPorts
  }

  // allow mdu and fmisc to have 2*numDeq enqueue ports
  val intDpPorts = (0 until exuParameters.AluCnt).map(i => {
    if (i < exuParameters.JmpCnt) Seq((0, i), (1, i), (2, i))
    else if (i < 2 * exuParameters.MduCnt) Seq((0, i), (1, i))
    else Seq((0, i))
  })
  val lsDpPorts = Seq(
    Seq((3, 0)),
    Seq((3, 1)),
    Seq((4, 0)),
    Seq((4, 1))
  ) ++ (0 until exuParameters.StuCnt).map(i => Seq((5, i)))
  val fpDpPorts = (0 until exuParameters.FmacCnt).map(i => {
    if (i < 2 * exuParameters.FmiscCnt) Seq((0, i), (1, i))
    else Seq((0, i))
  })

  val dispatchPorts = Seq(intDpPorts ++ lsDpPorts, fpDpPorts)

  val outIntRfReadPorts = Seq(0, 0)
  val outFpRfReadPorts = Seq(0, 2)
  val hasIntRf = Seq(true, false)
  val hasFpRf = Seq(false, true)
  val exuBlocks = schedulePorts.zip(dispatchPorts).zip(otherFastPorts).zipWithIndex.map {
    case (((sche, disp), other), i) =>
      LazyModule(new ExuBlock(sche, disp, intWbPorts, fpWbPorts, other, outIntRfReadPorts(i), outFpRfReadPorts(i), hasIntRf(i), hasFpRf(i)))
  }

  val memBlock = LazyModule(new MemBlock()(p.alter((site, here, up) => {
    case XSCoreParamsKey => up(XSCoreParamsKey).copy(
      IssQueSize = exuBlocks.head.scheduler.memRsEntries.max
    )
  })))
}

class XSCore()(implicit p: config.Parameters) extends XSCoreBase
  with HasXSDts
{
  lazy val module = new XSCoreImp(this)
}

class XSCoreImp(outer: XSCoreBase) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasSoCParameter
  with HasExeBlockHelper {
  val io = IO(new Bundle {
    val hartId = Input(UInt(64.W))
    val l2_pf_enable = Output(Bool())
    val perfEvents = Vec(numPCntHc * coreParams.L2NBanks,(Input(UInt(6.W))))
    val beu_errors = Output(new XSL1BusErrors())
  })

  println(s"FPGAPlatform:${env.FPGAPlatform} EnableDebug:${env.EnableDebug}")

  val ctrlBlock = Module(new CtrlBlock)

  val frontend = outer.frontend.module
  val memBlock = outer.memBlock.module
  val ptw = outer.ptw.module
  val exuBlocks = outer.exuBlocks.map(_.module)


  ctrlBlock.io.hartId := io.hartId
  exuBlocks.foreach(_.io.hartId := io.hartId)
  memBlock.io.hartId := io.hartId
  outer.wbArbiter.module.io.hartId := io.hartId


  outer.wbArbiter.module.io.redirect <> ctrlBlock.io.redirect
  val allWriteback = exuBlocks.flatMap(_.io.fuWriteback) ++ memBlock.io.writeback
  require(exuConfigs.length == allWriteback.length, s"${exuConfigs.length} != ${allWriteback.length}")
  outer.wbArbiter.module.io.in <> allWriteback
  val rfWriteback = outer.wbArbiter.module.io.out

  io.beu_errors.icache <> frontend.io.error
  io.beu_errors.dcache <> memBlock.io.error

  require(exuBlocks.count(_.fuConfigs.map(_._1).contains(JumpCSRExeUnitCfg)) == 1)
  val csrFenceMod = exuBlocks.filter(_.fuConfigs.map(_._1).contains(JumpCSRExeUnitCfg)).head
  val csrioIn = csrFenceMod.io.fuExtra.csrio.get
  val fenceio = csrFenceMod.io.fuExtra.fenceio.get

  frontend.io.backend <> ctrlBlock.io.frontend
  frontend.io.sfence <> fenceio.sfence
  frontend.io.tlbCsr <> csrioIn.tlb
  frontend.io.csrCtrl <> csrioIn.customCtrl
  frontend.io.fencei := fenceio.fencei

  ctrlBlock.io.csrCtrl <> csrioIn.customCtrl
  val redirectBlocks = exuBlocks.reverse.filter(_.fuConfigs.map(_._1).map(_.hasRedirect).reduce(_ || _))
  ctrlBlock.io.exuRedirect <> redirectBlocks.flatMap(_.io.fuExtra.exuRedirect)
  ctrlBlock.io.stIn <> memBlock.io.stIn
  ctrlBlock.io.stOut <> memBlock.io.stOut
  ctrlBlock.io.memoryViolation <> memBlock.io.memoryViolation
  exuBlocks.head.io.scheExtra.enqLsq.get <> memBlock.io.enqLsq
  val intWriteback = rfWriteback.take(outer.wbArbiter.intArbiter.numOutPorts)
  val fpWriteback = rfWriteback.drop(outer.wbArbiter.intArbiter.numOutPorts)
  val fpWritbackNoLoad = fpWriteback.zip(outer.wbArbiter.fpWbPorts).filterNot(_._2.contains(LdExeUnitCfg)).map(_._1)
  require(fpWritbackNoLoad.length + intWriteback.length == ctrlBlock.io.writeback.length)
  ctrlBlock.io.writeback <> VecInit(intWriteback ++ fpWritbackNoLoad)
  // Load Int and Fp have the same bits and we can re-use the write-back ports
  for (i <- 0 until exuParameters.LduCnt) {
    ctrlBlock.io.writeback(exuParameters.AluCnt + i).valid := memBlock.io.writeback(i).valid
  }

  val allFastUop = exuBlocks.flatMap(b => b.io.fastUopOut.dropRight(b.numOutFu)) ++ memBlock.io.otherFastWakeup
  require(allFastUop.length == exuConfigs.length, s"${allFastUop.length} != ${exuConfigs.length}")
  val intFastUop = allFastUop.zip(exuConfigs).filter(_._2.writeIntRf).map(_._1)
  val fpFastUop = allFastUop.zip(exuConfigs).filter(_._2.writeFpRf).map(_._1)
  val intFastUop1 = outer.wbArbiter.intConnections.map(c => intFastUop(c.head))
  val fpFastUop1 = outer.wbArbiter.fpConnections.map(c => fpFastUop(c.head))
  val allFastUop1 = intFastUop1 ++ fpFastUop1

  ctrlBlock.io.dispatch <> exuBlocks.flatMap(_.io.in)

  exuBlocks(0).io.scheExtra.fpRfReadIn.get <> exuBlocks(1).io.scheExtra.fpRfReadOut.get
  exuBlocks(0).io.scheExtra.fpStateReadIn.get <> exuBlocks(1).io.scheExtra.fpStateReadOut.get

  memBlock.io.issue <> exuBlocks(0).io.issue.get
  // By default, instructions do not have exceptions when they enter the function units.
  memBlock.io.issue.map(_.bits.uop.clearExceptions())
  exuBlocks(0).io.scheExtra.loadFastMatch.get <> memBlock.io.loadFastMatch

  val stdIssue = exuBlocks(0).io.issue.get.takeRight(exuParameters.StuCnt)
  exuBlocks.map(_.io).foreach { exu =>
    exu.redirect <> ctrlBlock.io.redirect
    exu.allocPregs <> ctrlBlock.io.allocPregs
    exu.rfWriteback <> rfWriteback
    exu.fastUopIn <> allFastUop1
    exu.scheExtra.jumpPc <> ctrlBlock.io.jumpPc
    exu.scheExtra.jalr_target <> ctrlBlock.io.jalr_target
    exu.scheExtra.stIssuePtr <> memBlock.io.stIssuePtr
    exu.scheExtra.debug_fp_rat <> ctrlBlock.io.debug_fp_rat
    exu.scheExtra.debug_int_rat <> ctrlBlock.io.debug_int_rat
    exu.scheExtra.memWaitUpdateReq.staIssue.zip(memBlock.io.stIn).foreach{case (sink, src) => {
      sink.bits := src.bits
      sink.valid := src.valid
    }}
    exu.scheExtra.memWaitUpdateReq.stdIssue.zip(stdIssue).foreach{case (sink, src) => {
      sink.valid := src.valid
      sink.bits := src.bits
    }}
  }
  XSPerfHistogram("fastIn_count", PopCount(allFastUop1.map(_.valid)), true.B, 0, allFastUop1.length, 1)
  XSPerfHistogram("wakeup_count", PopCount(rfWriteback.map(_.valid)), true.B, 0, rfWriteback.length, 1)

  // TODO: connect rsPerf
  val rsPerf = VecInit(exuBlocks.flatMap(_.io.scheExtra.perf))
  val rs_perf = Wire(new PerfEventsBundle(rsPerf.length))
  val rs_cnt = rs_perf.length
  for (i <- 0 until rs_cnt){
    rs_perf.perf_events(i).incr_step := rsPerf(i).asUInt
  }
  dontTouch(rsPerf)
  exuBlocks(0).perfinfo.perfEvents <> ctrlBlock.perfinfo.perfEventsEu0
  exuBlocks(1).perfinfo.perfEvents <> ctrlBlock.perfinfo.perfEventsEu1
  memBlock.perfinfo.perfEventsPTW <> ptw.perfinfo.perfEvents
  ctrlBlock.perfinfo.perfEventsRs := rs_perf

  csrioIn.hartId <> io.hartId
  csrioIn.perf <> DontCare
  csrioIn.perf.retiredInstr <> ctrlBlock.io.robio.toCSR.perfinfo.retiredInstr
  csrioIn.perf.ctrlInfo <> ctrlBlock.io.perfInfo.ctrlInfo
  csrioIn.perf.memInfo <> memBlock.io.memInfo
  csrioIn.perf.frontendInfo <> frontend.io.frontendInfo

  csrioIn.perf.perfEventsFrontend <> frontend.perfinfo.perfEvents
  csrioIn.perf.perfEventsCtrl     <> ctrlBlock.perfinfo.perfEvents
  csrioIn.perf.perfEventsLsu      <> memBlock.perfinfo.perfEvents
  csrioIn.perf.perfEventsHc       <> io.perfEvents

  csrioIn.fpu.fflags <> ctrlBlock.io.robio.toCSR.fflags
  csrioIn.fpu.isIllegal := false.B
  csrioIn.fpu.dirty_fs <> ctrlBlock.io.robio.toCSR.dirty_fs
  csrioIn.fpu.frm <> exuBlocks(1).io.fuExtra.frm.get
  csrioIn.exception <> ctrlBlock.io.robio.exception
  csrioIn.isXRet <> ctrlBlock.io.robio.toCSR.isXRet
  csrioIn.trapTarget <> ctrlBlock.io.robio.toCSR.trapTarget
  csrioIn.interrupt <> ctrlBlock.io.robio.toCSR.intrBitSet
  csrioIn.memExceptionVAddr <> memBlock.io.lsqio.exceptionAddr.vaddr

  csrioIn.externalInterrupt.msip := outer.clint_int_sink.in.head._1(0)
  csrioIn.externalInterrupt.mtip := outer.clint_int_sink.in.head._1(1)
  csrioIn.externalInterrupt.meip := outer.plic_int_sink.in.head._1(0)
  csrioIn.externalInterrupt.seip := outer.plic_int_sink.in.last._1(0)
  csrioIn.externalInterrupt.debug := outer.debug_int_sink.in.head._1(0)

  csrioIn.distributedUpdate <> memBlock.io.csrUpdate // TODO

  fenceio.sfence <> memBlock.io.sfence
  fenceio.sbuffer <> memBlock.io.fenceToSbuffer

  memBlock.io.redirect <> ctrlBlock.io.redirect
  memBlock.io.rsfeedback <> exuBlocks(0).io.scheExtra.feedback.get
  memBlock.io.csrCtrl <> csrioIn.customCtrl
  memBlock.io.tlbCsr <> csrioIn.tlb
  memBlock.io.lsqio.rob <> ctrlBlock.io.robio.lsq
  memBlock.io.lsqio.exceptionAddr.isStore := CommitType.lsInstIsStore(ctrlBlock.io.robio.exception.bits.uop.ctrl.commitType)

  val itlbRepeater1 = PTWRepeater(frontend.io.ptw, fenceio.sfence, csrioIn.tlb)
  val itlbRepeater2 = PTWRepeater(itlbRepeater1.io.ptw, ptw.io.tlb(0), fenceio.sfence, csrioIn.tlb)
  val dtlbRepeater1  = PTWFilter(memBlock.io.ptw, fenceio.sfence, csrioIn.tlb, l2tlbParams.filterSize)
  val dtlbRepeater2  = PTWRepeaterNB(passReady = false, dtlbRepeater1.io.ptw, ptw.io.tlb(1), fenceio.sfence, csrioIn.tlb)
  ptw.io.sfence <> fenceio.sfence
  ptw.io.csr.tlb <> csrioIn.tlb
  ptw.io.csr.distribute_csr <> csrioIn.customCtrl.distribute_csr

  // if l2 prefetcher use stream prefetch, it should be placed in XSCore
  io.l2_pf_enable := csrioIn.customCtrl.l2_pf_enable

  // Modules are reset one by one
  // reset --> SYNC ----> SYNC ------> SYNC -----> SYNC -----> SYNC ---
  //                  |          |            |           |           |
  //                  v          v            v           v           v
  //                 PTW  {MemBlock, dtlb}  ExuBlocks  CtrlBlock  {Frontend, itlb}
  val resetChain = Seq(
    Seq(memBlock, dtlbRepeater1, dtlbRepeater2),
    Seq(exuBlocks.head),
    // Note: arbiters don't actually have reset ports
    exuBlocks.tail ++ Seq(outer.wbArbiter.module),
    Seq(ctrlBlock),
    Seq(ptw),
    Seq(frontend, itlbRepeater1, itlbRepeater2)
  )
  ResetGen(resetChain, reset.asBool, !debugOpts.FPGAPlatform)
}
