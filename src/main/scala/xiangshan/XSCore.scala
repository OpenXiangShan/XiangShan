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
import xiangshan.backend.exu.Wb
import xiangshan.frontend._
import xiangshan.cache.mmu._
import xiangshan.cache.L1plusCacheWrapper
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
  with HasXSParameter
{
  // outer facing nodes
  val frontend = LazyModule(new Frontend())
  val l1pluscache = LazyModule(new L1plusCacheWrapper())
  val ptw = LazyModule(new PTWWrapper())

  val intConfigs = exuConfigs.filter(_.writeIntRf)
  val intArbiter = LazyModule(new Wb(intConfigs, NRIntWritePorts, isFp = false))
  println(intArbiter.allConnections)

  val fpConfigs = exuConfigs.filter(_.writeFpRf)
  val fpArbiter = LazyModule(new Wb(fpConfigs, NRFpWritePorts, isFp = true))
  println(fpArbiter.allConnections)

  // TODO: better RS organization
  // generate rs according to number of function units
  require(exuParameters.JmpCnt == 1)
  require(exuParameters.MduCnt <= exuParameters.AluCnt && exuParameters.MduCnt > 0)
  require(exuParameters.FmiscCnt <= exuParameters.FmacCnt && exuParameters.FmiscCnt > 0)
  require(exuParameters.LduCnt == 2 && exuParameters.StuCnt == 2)

  // one RS every 2 MDUs
  val schedulePorts = Seq(
    // exuCfg, numDeq, intFastWakeupTarget, fpFastWakeupTarget
    Seq((AluExeUnitCfg, exuParameters.AluCnt, Seq(AluExeUnitCfg, MulDivExeUnitCfg, JumpCSRExeUnitCfg, LdExeUnitCfg, StExeUnitCfg), Seq()),
    (MulDivExeUnitCfg, exuParameters.MduCnt, Seq(AluExeUnitCfg, MulDivExeUnitCfg, JumpCSRExeUnitCfg, LdExeUnitCfg, StExeUnitCfg), Seq()),
    (JumpCSRExeUnitCfg, 1, Seq(), Seq())),
    Seq((FmacExeUnitCfg, exuParameters.FmacCnt, Seq(), Seq(FmacExeUnitCfg, FmiscExeUnitCfg)),
    (FmiscExeUnitCfg, exuParameters.FmiscCnt, Seq(), Seq(FmacExeUnitCfg, FmiscExeUnitCfg))),
    Seq((LdExeUnitCfg, exuParameters.LduCnt, Seq(AluExeUnitCfg, LdExeUnitCfg), Seq()),
    (StExeUnitCfg, exuParameters.StuCnt, Seq(), Seq()))
  )
  // should do outer fast wakeup ports here

  // allow mdu and fmisc to have 2*numDeq enqueue ports
  val intDpPorts = (0 until exuParameters.AluCnt).map(i => {
    if (i < exuParameters.JmpCnt) Seq((0, i), (1, i), (2, i))
    else if (i < 2*exuParameters.MduCnt) Seq((0, i), (1, i))
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
  val dispatchPorts = Seq(intDpPorts, fpDpPorts, lsDpPorts)

  val scheduler = schedulePorts.zip(dispatchPorts).map{ case (sche, disp) => {
    val mappedSchedulePorts = sche.map(port => {
      val intWakeup = port._3.flatMap(cfg => sche.zipWithIndex.filter(_._1._1 == cfg).map(_._2))
      val fpWakeup = port._4.flatMap(cfg => sche.zipWithIndex.filter(_._1._1 == cfg).map(_._2))
      (port._1, port._2, intWakeup, fpWakeup)
    })

    LazyModule(new Scheduler(mappedSchedulePorts, disp))
  }}
  scheduler.zipWithIndex.foreach{ case (sche, i) => {
    val otherSche = scheduler.zipWithIndex.filter(_._2 != i).map(_._1)
    val numInt = otherSche.map(_.intRfWritePorts).sum
    val numFp = otherSche.map(_.fpRfWritePorts).sum
    sche.addIntWritebackPorts(numInt).addFpWritebackPorts(numFp)
  }}

  val memBlock = LazyModule(new MemBlock()(p.alter((site, here, up) => {
    case XSCoreParamsKey => up(XSCoreParamsKey).copy(
      IssQueSize = scheduler(2).memRsEntries.max
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

  // to fast wake up fp, mem rs
  val intBlockFastWakeUp = intExuConfigs.filter(_.hasCertainLatency)
  val intBlockSlowWakeUp = intExuConfigs.filter(_.hasUncertainlatency)

  val ctrlBlock = Module(new CtrlBlock)

  val intExuCfgs = Seq(
    (AluExeUnitCfg, exuParameters.AluCnt),
    (MulDivExeUnitCfg, exuParameters.MduCnt),
    (JumpCSRExeUnitCfg, exuParameters.JmpCnt)
  )
  val integerBlock = Module(new ExuBlock(intExuCfgs))
  val fpExuCfgs = Seq(
    (FmacExeUnitCfg, exuParameters.FmacCnt),
    (FmiscExeUnitCfg, exuParameters.FmiscCnt)
  )
  val floatBlock = Module(new ExuBlock(fpExuCfgs))

  val frontend = outer.frontend.module
  val memBlock = outer.memBlock.module
  val l1pluscache = outer.l1pluscache.module
  val ptw = outer.ptw.module
  val scheduler = outer.scheduler.map(_.module)

  val allWriteback = integerBlock.io.writeback ++ floatBlock.io.writeback ++ memBlock.io.writeback


  val intWriteback = allWriteback.zip(exuConfigs).filter(_._2.writeIntRf).map(_._1)
  // set default value for ready
  integerBlock.io.writeback.foreach(_.ready := true.B)
  floatBlock.io.writeback.foreach(_.ready := true.B)
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

  io.l1plus_error <> l1pluscache.io.error
  io.icache_error <> frontend.io.error
  io.dcache_error <> memBlock.io.error

  frontend.io.backend <> ctrlBlock.io.frontend
  frontend.io.sfence <> integerBlock.io.fenceio.get.sfence
  frontend.io.tlbCsr <> integerBlock.io.csrio.get.tlb
  frontend.io.csrCtrl <> integerBlock.io.csrio.get.customCtrl

  frontend.io.icacheMemAcq <> l1pluscache.io.req
  l1pluscache.io.resp <> frontend.io.icacheMemGrant
  l1pluscache.io.flush := frontend.io.l1plusFlush
  frontend.io.fencei := integerBlock.io.fenceio.get.fencei

  ctrlBlock.io.csrCtrl <> integerBlock.io.csrio.get.customCtrl
  ctrlBlock.io.exuRedirect <> integerBlock.io.exuRedirect
  ctrlBlock.io.stIn <> memBlock.io.stIn
  ctrlBlock.io.stOut <> memBlock.io.stOut
  ctrlBlock.io.memoryViolation <> memBlock.io.memoryViolation
  ctrlBlock.io.enqLsq <> memBlock.io.enqLsq
  ctrlBlock.io.writeback <> VecInit(intArbiter.io.out ++ fpArbiter.io.out)

  scheduler.foreach(_.io.redirect <> ctrlBlock.io.redirect)
  scheduler.foreach(_.io.flush <> ctrlBlock.io.flush)
  ctrlBlock.io.enqIQ <> scheduler(0).io.allocate ++ scheduler(1).io.allocate ++ scheduler(2).io.allocate
  scheduler(0).io.issue <> integerBlock.io.issue
  scheduler(1).io.issue <> floatBlock.io.issue
  scheduler(2).io.issue <> memBlock.io.issue
  scheduler(0).io.writeback <> intArbiter.io.out ++ fpArbiter.io.out
  scheduler(1).io.writeback <> intArbiter.io.out ++ fpArbiter.io.out
  scheduler(2).io.writeback <> intArbiter.io.out ++ fpArbiter.io.out


//  scheduler(0).io.otherFastWakeup.get <> memBlock.io.otherFastWakeup
  scheduler(2).io.fastUopIn.get <> memBlock.io.otherFastWakeup
  scheduler(0).io.jumpPc <> ctrlBlock.io.jumpPc
  scheduler(0).io.jalr_target <> ctrlBlock.io.jalr_target
  scheduler(0).io.stIssuePtr <> memBlock.io.stIssuePtr
  scheduler(1).io.debug_fp_rat <> ctrlBlock.io.debug_fp_rat
  scheduler(0).io.debug_int_rat <> ctrlBlock.io.debug_int_rat
  ctrlBlock.io.readIntRf <> scheduler(0).readIntRf ++ scheduler(2).readIntRf
  ctrlBlock.io.readFpRf <> scheduler(1).readFpRf ++ scheduler(2).readFpRf

  integerBlock.io.redirect <> ctrlBlock.io.redirect
  integerBlock.io.flush <> ctrlBlock.io.flush
  integerBlock.io.csrio.get.hartId <> io.hartId
  integerBlock.io.csrio.get.perf <> DontCare
  integerBlock.io.csrio.get.perf.retiredInstr <> ctrlBlock.io.roqio.toCSR.perfinfo.retiredInstr
  integerBlock.io.csrio.get.perf.bpuInfo <> ctrlBlock.io.perfInfo.bpuInfo
  integerBlock.io.csrio.get.perf.ctrlInfo <> ctrlBlock.io.perfInfo.ctrlInfo
  integerBlock.io.csrio.get.perf.memInfo <> memBlock.io.memInfo
  integerBlock.io.csrio.get.perf.frontendInfo <> frontend.io.frontendInfo

  integerBlock.io.csrio.get.fpu.fflags <> ctrlBlock.io.roqio.toCSR.fflags
  integerBlock.io.csrio.get.fpu.isIllegal := false.B
  integerBlock.io.csrio.get.fpu.dirty_fs <> ctrlBlock.io.roqio.toCSR.dirty_fs
  integerBlock.io.csrio.get.fpu.frm <> floatBlock.io.frm.get
  integerBlock.io.csrio.get.exception <> ctrlBlock.io.roqio.exception
  integerBlock.io.csrio.get.isXRet <> ctrlBlock.io.roqio.toCSR.isXRet
  integerBlock.io.csrio.get.trapTarget <> ctrlBlock.io.roqio.toCSR.trapTarget
  integerBlock.io.csrio.get.interrupt <> ctrlBlock.io.roqio.toCSR.intrBitSet
  integerBlock.io.csrio.get.memExceptionVAddr <> memBlock.io.lsqio.exceptionAddr.vaddr
  integerBlock.io.csrio.get.externalInterrupt <> io.externalInterrupt

  floatBlock.io.redirect <> ctrlBlock.io.redirect
  floatBlock.io.flush <> ctrlBlock.io.flush

  integerBlock.io.fenceio.get.sfence <> memBlock.io.sfence
  integerBlock.io.fenceio.get.sbuffer <> memBlock.io.fenceToSbuffer

  memBlock.io.redirect <> ctrlBlock.io.redirect
  memBlock.io.flush <> ctrlBlock.io.flush
  memBlock.io.replay <> scheduler(2).io.feedback.get.map(_.replay)
  memBlock.io.rsIdx <> scheduler(2).io.feedback.get.map(_.rsIdx)
  memBlock.io.isFirstIssue <> scheduler(2).io.feedback.get.map(_.isFirstIssue)
  memBlock.io.stData <> scheduler(2).stData
  memBlock.io.csrCtrl <> integerBlock.io.csrio.get.customCtrl
  memBlock.io.tlbCsr <> integerBlock.io.csrio.get.tlb
  memBlock.io.lsqio.roq <> ctrlBlock.io.roqio.lsq
  memBlock.io.lsqio.exceptionAddr.lsIdx.lqIdx := ctrlBlock.io.roqio.exception.bits.uop.lqIdx
  memBlock.io.lsqio.exceptionAddr.lsIdx.sqIdx := ctrlBlock.io.roqio.exception.bits.uop.sqIdx
  memBlock.io.lsqio.exceptionAddr.isStore := CommitType.lsInstIsStore(ctrlBlock.io.roqio.exception.bits.uop.ctrl.commitType)

  val itlbRepeater = Module(new PTWRepeater())
  val dtlbRepeater = if (usePTWRepeater) {
    Module(new PTWRepeater(LoadPipelineWidth + StorePipelineWidth))
  } else {
    Module(new PTWFilter(LoadPipelineWidth + StorePipelineWidth, PtwMissQueueSize))
  }
  itlbRepeater.io.tlb <> frontend.io.ptw
  dtlbRepeater.io.tlb <> memBlock.io.ptw
  itlbRepeater.io.sfence <> integerBlock.io.fenceio.get.sfence
  dtlbRepeater.io.sfence <> integerBlock.io.fenceio.get.sfence
  ptw.io.tlb(0) <> itlbRepeater.io.ptw
  ptw.io.tlb(1) <> dtlbRepeater.io.ptw
  ptw.io.sfence <> integerBlock.io.fenceio.get.sfence
  ptw.io.csr <> integerBlock.io.csrio.get.tlb

  // if l2 prefetcher use stream prefetch, it should be placed in XSCore
  assert(l2PrefetcherParameters._type == "bop")
  io.l2_pf_enable := integerBlock.io.csrio.get.customCtrl.l2_pf_enable

  val l1plus_reset_gen = Module(new ResetGen(1, !debugOpts.FPGAPlatform))
  l1pluscache.reset := l1plus_reset_gen.io.out

  val ptw_reset_gen = Module(new ResetGen(2, !debugOpts.FPGAPlatform))
  ptw.reset := ptw_reset_gen.io.out
  itlbRepeater.reset := ptw_reset_gen.io.out
  dtlbRepeater.reset := ptw_reset_gen.io.out

  val memBlock_reset_gen = Module(new ResetGen(3, !debugOpts.FPGAPlatform))
  memBlock.reset := memBlock_reset_gen.io.out

  val intBlock_reset_gen = Module(new ResetGen(4, !debugOpts.FPGAPlatform))
  integerBlock.reset := intBlock_reset_gen.io.out

  val fpBlock_reset_gen = Module(new ResetGen(5, !debugOpts.FPGAPlatform))
  floatBlock.reset := fpBlock_reset_gen.io.out

  val ctrlBlock_reset_gen = Module(new ResetGen(6, !debugOpts.FPGAPlatform))
  ctrlBlock.reset := ctrlBlock_reset_gen.io.out

  val frontend_reset_gen = Module(new ResetGen(7, !debugOpts.FPGAPlatform))
  frontend.reset := frontend_reset_gen.io.out
}
