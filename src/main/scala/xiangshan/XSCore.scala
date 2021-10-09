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
import xiangshan.backend.exu.{ExuConfig, WbArbiter}
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
  val intArbiter = LazyModule(new WbArbiter(intConfigs, NRIntWritePorts, isFp = false))
  val intWbPorts = intArbiter.allConnections.map(c => c.map(intConfigs(_)))
  val numIntWbPorts = intWbPorts.length

  val fpConfigs = exuConfigs.filter(_.writeFpRf)
  val fpArbiter = LazyModule(new WbArbiter(fpConfigs, NRFpWritePorts, isFp = true))
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
      getFastWakeupIndex(cfg._1, intSource, fpSource, numIntWbPorts).sorted
    })
    println(s"inter-scheduler wakeup sources for $i: $outerPorts")
    outerPorts
  }

  // allow mdu and fmisc to have 2*numDeq enqueue ports
  val intDpPorts = (0 until exuParameters.AluCnt).map(i => {
    if (i < exuParameters.JmpCnt) Seq((0, i), (1, i), (2, i))
    else if (i < exuParameters.MduCnt) Seq((0, i), (1, i))
    else Seq((0, i))
  })
  val lsDpPorts = Seq(
    Seq((3, 0)),
    Seq((3, 1)),
    Seq((4, 0)),
    Seq((4, 1))
  ) ++ (0 until exuParameters.StuCnt).map(i => Seq((5, i)))
  val fpDpPorts = (0 until exuParameters.FmacCnt).map(i => {
    if (i < exuParameters.FmiscCnt) Seq((0, i), (1, i))
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

  val allWriteback = exuBlocks.flatMap(_.io.fuWriteback) ++ memBlock.io.writeback

  val intWriteback = allWriteback.zip(exuConfigs).filter(_._2.writeIntRf).map(_._1)
  require(exuConfigs.length == allWriteback.length, s"${exuConfigs.length} != ${allWriteback.length}")

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
  ctrlBlock.io.exuRedirect <> redirectBlocks.flatMap(_.io.fuExtra.exuRedirect)
  ctrlBlock.io.stIn <> memBlock.io.stIn
  ctrlBlock.io.stOut <> memBlock.io.stOut
  ctrlBlock.io.memoryViolation <> memBlock.io.memoryViolation
  ctrlBlock.io.enqLsq <> memBlock.io.enqLsq
  ctrlBlock.io.writeback <> rfWriteback

  val allFastUop = exuBlocks.flatMap(b => b.io.fastUopOut.dropRight(b.numOutFu)) ++ memBlock.io.otherFastWakeup
  require(allFastUop.length == exuConfigs.length, s"${allFastUop.length} != ${exuConfigs.length}")
  val intFastUop = allFastUop.zip(exuConfigs).filter(_._2.writeIntRf).map(_._1)
  val fpFastUop = allFastUop.zip(exuConfigs).filter(_._2.writeFpRf).map(_._1)
  val intFastUop1 = outer.intArbiter.allConnections.map(c => intFastUop(c.head))
  val fpFastUop1 = outer.fpArbiter.allConnections.map(c => fpFastUop(c.head))
  val allFastUop1 = intFastUop1 ++ fpFastUop1

  ctrlBlock.io.dispatch <> exuBlocks.flatMap(_.io.in)

  exuBlocks(0).io.scheExtra.fpRfReadIn.get <> exuBlocks(1).io.scheExtra.fpRfReadOut.get
  exuBlocks(0).io.scheExtra.fpStateReadIn.get <> exuBlocks(1).io.scheExtra.fpStateReadOut.get

  memBlock.io.issue <> exuBlocks(0).io.issue.get
  // By default, instructions do not have exceptions when they enter the function units.
  memBlock.io.issue.map(_.bits.uop.clearExceptions())
  exuBlocks(0).io.scheExtra.loadFastMatch.get <> memBlock.io.loadFastMatch

  exuBlocks.map(_.io).foreach { exu =>
    exu.redirect <> ctrlBlock.io.redirect
    exu.flush <> ctrlBlock.io.flush
    exu.allocPregs <> ctrlBlock.io.allocPregs
    exu.rfWriteback <> rfWriteback
    exu.fastUopIn <> allFastUop1
    exu.scheExtra.jumpPc <> ctrlBlock.io.jumpPc
    exu.scheExtra.jalr_target <> ctrlBlock.io.jalr_target
    exu.scheExtra.stIssuePtr <> memBlock.io.stIssuePtr
    exu.scheExtra.debug_fp_rat <> ctrlBlock.io.debug_fp_rat
    exu.scheExtra.debug_int_rat <> ctrlBlock.io.debug_int_rat
  }
  XSPerfHistogram("fastIn_count", PopCount(allFastUop1.map(_.valid)), true.B, 0, allFastUop1.length, 1)
  XSPerfHistogram("wakeup_count", PopCount(rfWriteback.map(_.valid)), true.B, 0, rfWriteback.length, 1)

  csrioIn.hartId <> io.hartId
  csrioIn.perf <> DontCare
  csrioIn.perf.retiredInstr <> ctrlBlock.io.robio.toCSR.perfinfo.retiredInstr
  csrioIn.perf.ctrlInfo <> ctrlBlock.io.perfInfo.ctrlInfo
  csrioIn.perf.memInfo <> memBlock.io.memInfo
  csrioIn.perf.frontendInfo <> frontend.io.frontendInfo

  csrioIn.fpu.fflags <> ctrlBlock.io.robio.toCSR.fflags
  csrioIn.fpu.isIllegal := false.B
  csrioIn.fpu.dirty_fs <> ctrlBlock.io.robio.toCSR.dirty_fs
  csrioIn.fpu.frm <> exuBlocks(1).io.fuExtra.frm.get
  csrioIn.exception <> ctrlBlock.io.robio.exception
  csrioIn.isXRet <> ctrlBlock.io.robio.toCSR.isXRet
  csrioIn.trapTarget <> ctrlBlock.io.robio.toCSR.trapTarget
  csrioIn.interrupt <> ctrlBlock.io.robio.toCSR.intrBitSet
  csrioIn.memExceptionVAddr <> memBlock.io.lsqio.exceptionAddr.vaddr
  csrioIn.externalInterrupt <> io.externalInterrupt

  fenceio.sfence <> memBlock.io.sfence
  fenceio.sbuffer <> memBlock.io.fenceToSbuffer

  memBlock.io.redirect <> ctrlBlock.io.redirect
  memBlock.io.flush <> ctrlBlock.io.flush
  memBlock.io.replay <> exuBlocks(0).io.scheExtra.feedback.get.map(_.replay)
  memBlock.io.rsIdx <> exuBlocks(0).io.scheExtra.feedback.get.map(_.rsIdx)
  memBlock.io.isFirstIssue <> exuBlocks(0).io.scheExtra.feedback.get.map(_.isFirstIssue)
  memBlock.io.csrCtrl <> csrioIn.customCtrl
  memBlock.io.tlbCsr <> csrioIn.tlb
  memBlock.io.lsqio.rob <> ctrlBlock.io.robio.lsq
  memBlock.io.lsqio.exceptionAddr.lsIdx.lqIdx := ctrlBlock.io.robio.exception.bits.uop.lqIdx
  memBlock.io.lsqio.exceptionAddr.lsIdx.sqIdx := ctrlBlock.io.robio.exception.bits.uop.sqIdx
  memBlock.io.lsqio.exceptionAddr.isStore := CommitType.lsInstIsStore(ctrlBlock.io.robio.exception.bits.uop.ctrl.commitType)

  val itlbRepeater = Module(new PTWRepeater(2))
  val dtlbRepeater = Module(new PTWFilter(LoadPipelineWidth + StorePipelineWidth, l2tlbParams.missQueueSize - 1))
  itlbRepeater.io.tlb <> frontend.io.ptw
  dtlbRepeater.io.tlb <> memBlock.io.ptw
  itlbRepeater.io.sfence <> fenceio.sfence
  dtlbRepeater.io.sfence <> fenceio.sfence
  ptw.io.tlb(0) <> itlbRepeater.io.ptw
  ptw.io.tlb(1) <> dtlbRepeater.io.ptw
  ptw.io.sfence <> fenceio.sfence
  ptw.io.csr <> csrioIn.tlb

  // if l2 prefetcher use stream prefetch, it should be placed in XSCore
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
