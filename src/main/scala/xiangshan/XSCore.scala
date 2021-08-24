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
import xiangshan.backend.exu.{ExuConfig, Wb}
import xiangshan.frontend._
import xiangshan.cache.mmu._
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.HasFPUParameters
import system.{HasSoCParameter, L1CacheErrorInfo, SoCParamsKey}
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

case class EnviromentParameters
(
  FPGAPlatform: Boolean = true,
  EnableDebug: Boolean = false,
  EnablePerfDebug: Boolean = true,
  DualCore: Boolean = false
)

abstract class XSCoreBase()(implicit p: config.Parameters) extends LazyModule
  with HasXSParameter with HasExuWbMappingHelper
{
  // outer facing nodes
  val frontend = LazyModule(new Frontend())
  val ptw = LazyModule(new PTWWrapper())

  val intConfigs = exuConfigs.filter(_.writeIntRf)
  val intArbiter = LazyModule(new Wb(intConfigs, NRIntWritePorts, isFp = false))
  val intWbPorts = intArbiter.allConnections.map(c => c.map(intConfigs(_)))
  val numIntWbPorts = intWbPorts.length

  val fpConfigs = exuConfigs.filter(_.writeFpRf)
  val fpArbiter = LazyModule(new Wb(fpConfigs, NRFpWritePorts, isFp = true))
  val fpWbPorts = fpArbiter.allConnections.map(c => c.map(fpConfigs(_)))
  val numFpWbPorts = fpWbPorts.length

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
      (AluExeUnitCfg, exuParameters.AluCnt, Seq(AluExeUnitCfg, MulDivExeUnitCfg, JumpCSRExeUnitCfg, LdExeUnitCfg, StExeUnitCfg), Seq())
    ),
    Seq(
      (MulDivExeUnitCfg, exuParameters.MduCnt, Seq(AluExeUnitCfg, MulDivExeUnitCfg, JumpCSRExeUnitCfg, LdExeUnitCfg, StExeUnitCfg), Seq()),
      (JumpCSRExeUnitCfg, 1, Seq(), Seq())
    ),
    Seq(
      (FmacExeUnitCfg, exuParameters.FmacCnt, Seq(), Seq(FmacExeUnitCfg, FmiscExeUnitCfg)),
      (FmiscExeUnitCfg, exuParameters.FmiscCnt, Seq(), Seq())
    ),
    Seq(
      (LdExeUnitCfg, exuParameters.LduCnt, Seq(AluExeUnitCfg, LdExeUnitCfg), Seq()),
      (StExeUnitCfg, exuParameters.StuCnt, Seq(), Seq())
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
      getFastWakeupIndex(cfg._1, intSource, fpSource, numIntWbPorts).sorted
    })
    println(s"inter-scheduler wakeup sources for $i: $outerPorts")
    outerPorts
  }

  // allow mdu and fmisc to have 2*numDeq enqueue ports
  val intDpPorts = (0 until exuParameters.AluCnt).map(i => Seq((0, i)))
  val int1DpPorts = (0 until 2*exuParameters.MduCnt).map(i => {
    if (i < exuParameters.JmpCnt) Seq((0, i), (1, i))
    else Seq((0, i))
  })
  val fpDpPorts = (0 until exuParameters.FmacCnt).map(i => {
    if (i < 2*exuParameters.FmiscCnt) Seq((0, i), (1, i))
    else Seq((1, i))
  })
  val lsDpPorts = Seq(
    Seq((0, 0)),
    Seq((0, 1)),
    Seq((1, 0)),
    Seq((1, 1))
  )
  val dispatchPorts = Seq(intDpPorts, int1DpPorts, fpDpPorts, lsDpPorts)

  val exuBlocks = schedulePorts.zip(dispatchPorts).zip(otherFastPorts).reverse.drop(1).reverseMap { case ((sche, disp), other) =>
    LazyModule(new ExuBlock(sche, disp, intWbPorts, fpWbPorts, other))
  }
  
  val memScheduler = LazyModule(new Scheduler(schedulePorts.last, dispatchPorts.last, intWbPorts, fpWbPorts, otherFastPorts.last))
  val memBlock = LazyModule(new MemBlock()(p.alter((site, here, up) => {
    case XSCoreParamsKey => up(XSCoreParamsKey).copy(
      IssQueSize = memScheduler.memRsEntries.max
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
    val externalInterrupt = new ExternalInterruptIO
    val l2_pf_enable = Output(Bool())
    val l1plus_error, icache_error, dcache_error = Output(new L1CacheErrorInfo)
  })

  println(s"FPGAPlatform:${env.FPGAPlatform} EnableDebug:${env.EnableDebug}")
  AddressSpace.checkMemmap()
  AddressSpace.printMemmap()

  val ctrlBlock = Module(new CtrlBlock)

  val frontend = outer.frontend.module
  val memBlock = outer.memBlock.module
  val ptw = outer.ptw.module
  val exuBlocks = outer.exuBlocks.map(_.module)
  val memScheduler = outer.memScheduler.module

  val allWriteback = exuBlocks.map(_.io.fuWriteback).fold(Seq())(_ ++ _) ++ memBlock.io.writeback

  val intWriteback = allWriteback.zip(exuConfigs).filter(_._2.writeIntRf).map(_._1)
  // set default value for ready
  exuBlocks.foreach(_.io.fuWriteback.foreach(_.ready := true.B))
  memBlock.io.writeback.foreach(_.ready := true.B)

  val intArbiter = outer.intArbiter.module
  intArbiter.io.in.zip(intWriteback).foreach { case (arb, wb) =>
    arb.valid := wb.valid && !wb.bits.uop.ctrl.fpWen
    arb.bits := wb.bits
    when (arb.valid) {
      wb.ready := arb.ready
    }
  }

  val fpArbiter = outer.fpArbiter.module
  val fpWriteback = allWriteback.zip(exuConfigs).filter(_._2.writeFpRf).map(_._1)
  fpArbiter.io.in.zip(fpWriteback).foreach{ case (arb, wb) =>
    arb.valid := wb.valid && wb.bits.uop.ctrl.fpWen
    arb.bits := wb.bits
    when (arb.valid) {
      wb.ready := arb.ready
    }
  }

  val rfWriteback = VecInit(intArbiter.io.out ++ fpArbiter.io.out)

  io.l1plus_error <> DontCare
  io.icache_error <> frontend.io.error
  io.dcache_error <> memBlock.io.error

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
  ctrlBlock.io.exuRedirect <> redirectBlocks.map(_.io.fuExtra.exuRedirect).fold(Seq())(_ ++ _)
  ctrlBlock.io.stIn <> memBlock.io.stIn
  ctrlBlock.io.stOut <> memBlock.io.stOut
  ctrlBlock.io.memoryViolation <> memBlock.io.memoryViolation
  ctrlBlock.io.enqLsq <> memBlock.io.enqLsq
  ctrlBlock.io.writeback <> rfWriteback

  val allFastUop = exuBlocks.map(_.io.fastUopOut).fold(Seq())(_ ++ _) ++ memBlock.io.otherFastWakeup
  val intFastUop = allFastUop.zip(exuConfigs).filter(_._2.writeIntRf).map(_._1)
  val fpFastUop = allFastUop.zip(exuConfigs).filter(_._2.writeFpRf).map(_._1)
  val intFastUop1 = outer.intArbiter.allConnections.map(c => intFastUop(c.head))
  val fpFastUop1 = outer.fpArbiter.allConnections.map(c => fpFastUop(c.head))
  val allFastUop1 = intFastUop1 ++ fpFastUop1

  ctrlBlock.io.enqIQ <> exuBlocks(0).io.allocate ++ exuBlocks(2).io.allocate ++ memScheduler.io.allocate
  for (i <- 0 until exuParameters.AluCnt) {
    val rsIn = VecInit(Seq(exuBlocks(0).io.allocate(i), exuBlocks(1).io.allocate(i)))
    val func1 = (op: MicroOp) => outer.exuBlocks(0).scheduler.canAccept(op.ctrl.fuType)
    val func2 = (op: MicroOp) => outer.exuBlocks(1).scheduler.canAccept(op.ctrl.fuType)
    val arbiterOut = DispatchArbiter(ctrlBlock.io.enqIQ(i), Seq(func1, func2))
    rsIn <> arbiterOut
  }
  memScheduler.io.redirect <> ctrlBlock.io.redirect
  memScheduler.io.flush <> ctrlBlock.io.flush
  memScheduler.io.issue <> memBlock.io.issue
  memScheduler.io.writeback <> rfWriteback
  memScheduler.io.fastUopIn <> allFastUop1
  memScheduler.io.extra.jumpPc <> ctrlBlock.io.jumpPc
  memScheduler.io.extra.jalr_target <> ctrlBlock.io.jalr_target
  memScheduler.io.extra.stIssuePtr <> memBlock.io.stIssuePtr
  memScheduler.io.extra.debug_int_rat <> ctrlBlock.io.debug_int_rat
  memScheduler.io.extra.debug_fp_rat <> ctrlBlock.io.debug_fp_rat

  exuBlocks.map(_.io).foreach { exu =>
    exu.redirect <> ctrlBlock.io.redirect
    exu.flush <> ctrlBlock.io.flush
    exu.rfWriteback <> rfWriteback
    exu.fastUopIn <> allFastUop1
    exu.scheExtra.jumpPc <> ctrlBlock.io.jumpPc
    exu.scheExtra.jalr_target <> ctrlBlock.io.jalr_target
    exu.scheExtra.stIssuePtr <> memBlock.io.stIssuePtr
    exu.scheExtra.debug_fp_rat <> ctrlBlock.io.debug_fp_rat
    exu.scheExtra.debug_int_rat <> ctrlBlock.io.debug_int_rat
  }

  csrioIn.hartId <> io.hartId
  csrioIn.perf <> DontCare
  csrioIn.perf.retiredInstr <> ctrlBlock.io.roqio.toCSR.perfinfo.retiredInstr
  // csrioIn.perf.bpuInfo <> DontCare // TODO: reassign this
  csrioIn.perf.ctrlInfo <> ctrlBlock.io.perfInfo.ctrlInfo
  csrioIn.perf.memInfo <> memBlock.io.memInfo
  csrioIn.perf.frontendInfo <> frontend.io.frontendInfo

  csrioIn.fpu.fflags <> ctrlBlock.io.roqio.toCSR.fflags
  csrioIn.fpu.isIllegal := false.B
  csrioIn.fpu.dirty_fs <> ctrlBlock.io.roqio.toCSR.dirty_fs
  csrioIn.fpu.frm <> exuBlocks(2).io.fuExtra.frm.get
  csrioIn.exception <> ctrlBlock.io.roqio.exception
  csrioIn.isXRet <> ctrlBlock.io.roqio.toCSR.isXRet
  csrioIn.trapTarget <> ctrlBlock.io.roqio.toCSR.trapTarget
  csrioIn.interrupt <> ctrlBlock.io.roqio.toCSR.intrBitSet
  csrioIn.memExceptionVAddr <> memBlock.io.lsqio.exceptionAddr.vaddr
  csrioIn.externalInterrupt <> io.externalInterrupt

  fenceio.sfence <> memBlock.io.sfence
  fenceio.sbuffer <> memBlock.io.fenceToSbuffer

  memBlock.io.redirect <> ctrlBlock.io.redirect
  memBlock.io.flush <> ctrlBlock.io.flush
  memBlock.io.replay <> memScheduler.io.extra.feedback.get.map(_.replay)
  memBlock.io.rsIdx <> memScheduler.io.extra.feedback.get.map(_.rsIdx)
  memBlock.io.isFirstIssue <> memScheduler.io.extra.feedback.get.map(_.isFirstIssue)
  memBlock.io.stData <> memScheduler.stData
  memBlock.io.csrCtrl <> csrioIn.customCtrl
  memBlock.io.tlbCsr <> csrioIn.tlb
  memBlock.io.lsqio.roq <> ctrlBlock.io.roqio.lsq
  memBlock.io.lsqio.exceptionAddr.lsIdx.lqIdx := ctrlBlock.io.roqio.exception.bits.uop.lqIdx
  memBlock.io.lsqio.exceptionAddr.lsIdx.sqIdx := ctrlBlock.io.roqio.exception.bits.uop.sqIdx
  memBlock.io.lsqio.exceptionAddr.isStore := CommitType.lsInstIsStore(ctrlBlock.io.roqio.exception.bits.uop.ctrl.commitType)

  val itlbRepeater = Module(new PTWRepeater(2))
  val dtlbRepeater = if (usePTWRepeater) {
    Module(new PTWRepeater(LoadPipelineWidth + StorePipelineWidth))
  } else {
    Module(new PTWFilter(LoadPipelineWidth + StorePipelineWidth, PtwMissQueueSize))
  }
  itlbRepeater.io.tlb <> frontend.io.ptw
  dtlbRepeater.io.tlb <> memBlock.io.ptw
  itlbRepeater.io.sfence <> fenceio.sfence
  dtlbRepeater.io.sfence <> fenceio.sfence
  ptw.io.tlb(0) <> itlbRepeater.io.ptw
  ptw.io.tlb(1) <> dtlbRepeater.io.ptw
  ptw.io.sfence <> fenceio.sfence
  ptw.io.csr <> csrioIn.tlb

  // if l2 prefetcher use stream prefetch, it should be placed in XSCore
  assert(l2PrefetcherParameters._type == "bop")
  io.l2_pf_enable := csrioIn.customCtrl.l2_pf_enable

  val ptw_reset_gen = Module(new ResetGen(2, !debugOpts.FPGAPlatform))
  ptw.reset := ptw_reset_gen.io.out
  itlbRepeater.reset := ptw_reset_gen.io.out
  dtlbRepeater.reset := ptw_reset_gen.io.out

  val memBlock_reset_gen = Module(new ResetGen(3, !debugOpts.FPGAPlatform))
  memBlock.reset := memBlock_reset_gen.io.out

  val exuBlock_reset_gen = Module(new ResetGen(4, !debugOpts.FPGAPlatform))
  exuBlocks.foreach(_.reset := exuBlock_reset_gen.io.out)

  val ctrlBlock_reset_gen = Module(new ResetGen(6, !debugOpts.FPGAPlatform))
  ctrlBlock.reset := ctrlBlock_reset_gen.io.out

  val frontend_reset_gen = Module(new ResetGen(7, !debugOpts.FPGAPlatform))
  frontend.reset := frontend_reset_gen.io.out
}
