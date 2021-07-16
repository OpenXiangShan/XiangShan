/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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
import xiangshan.cache.{L1plusCacheWrapper, PTWWrapper, PTWRepeater, PTWFilter}
import chipsalliance.rocketchip.config
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.HasFPUParameters
import system.{HasSoCParameter, L1CacheErrorInfo}
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
  val memBlock = LazyModule(new MemBlock)

  // TODO: better RS organization
  // generate rs according to number of function units
  require(exuParameters.JmpCnt == 1)
  require(exuParameters.MduCnt <= exuParameters.AluCnt && exuParameters.MduCnt > 0)
  require(exuParameters.FmiscCnt <= exuParameters.FmacCnt && exuParameters.FmiscCnt > 0)
  require(exuParameters.LduCnt == 2 && exuParameters.StuCnt == 2)
  // one RS every 2 MDUs
  val schedulePorts = Seq(
    // exuCfg, numDeq, intFastWakeupTarget, fpFastWakeupTarget
    (AluExeUnitCfg, exuParameters.AluCnt, Seq(0, 1, 2, 5, 6, 7, 8), Seq()),
    (MulDivExeUnitCfg, exuParameters.MduCnt, Seq(0, 1, 2, 5, 6, 7, 8), Seq()),
    (JumpExeUnitCfg, 1, Seq(), Seq()),
    (FmacExeUnitCfg, exuParameters.FmacCnt, Seq(), Seq(3, 4)),
    (FmiscExeUnitCfg, exuParameters.FmiscCnt, Seq(), Seq(3, 4)),
    (LdExeUnitCfg, 1, Seq(0, 5, 6), Seq()),
    (LdExeUnitCfg, 1, Seq(0, 5, 6), Seq()),
    (StExeUnitCfg, 1, Seq(), Seq()),
    (StExeUnitCfg, 1, Seq(), Seq())
  )
  // allow mdu and fmisc to have 2*numDeq enqueue ports
  val intDpPorts = (0 until exuParameters.AluCnt).map(i => {
    if (i < exuParameters.JmpCnt) Seq((0, i), (1, i), (2, i))
    else if (i < 2*exuParameters.MduCnt) Seq((0, i), (1, i))
    else Seq((0, i))
  })
  val fpDpPorts = (0 until exuParameters.FmacCnt).map(i => {
    if (i < 2*exuParameters.FmiscCnt) Seq((3, i), (4, i))
    else Seq((4, i))
  })
  val lsDpPorts = Seq(
    Seq((5, 0)),
    Seq((6, 0)),
    Seq((7, 0)),
    Seq((8, 0))
  )
  val dispatchPorts = intDpPorts ++ fpDpPorts ++ lsDpPorts

  val scheduler = LazyModule(new Scheduler(schedulePorts, dispatchPorts))

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

  val integerBlock = Module(new IntegerBlock)
  val floatBlock = Module(new FloatBlock)

  val frontend = outer.frontend.module
  val memBlock = outer.memBlock.module
  val l1pluscache = outer.l1pluscache.module
  val ptw = outer.ptw.module
  val scheduler = outer.scheduler.module

  val allWriteback = integerBlock.io.writeback ++ floatBlock.io.writeback ++ memBlock.io.writeback
  val intConfigs = exuConfigs.filter(_.writeIntRf)
  val intArbiter = Module(new Wb(intConfigs, NRIntWritePorts, isFp = false))
  val intWriteback = allWriteback.zip(exuConfigs).filter(_._2.writeIntRf).map(_._1)
  // set default value for ready
  integerBlock.io.writeback.map(_.ready := true.B)
  floatBlock.io.writeback.map(_.ready := true.B)
  memBlock.io.writeback.map(_.ready := true.B)
  intArbiter.io.in.zip(intWriteback).foreach { case (arb, wb) =>
    arb.valid := wb.valid && !wb.bits.uop.ctrl.fpWen
    arb.bits := wb.bits
    when (arb.valid) {
      wb.ready := arb.ready
    }
  }

  val fpArbiter = Module(new Wb(exuConfigs.filter(_.writeFpRf), NRFpWritePorts, isFp = true))
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
  frontend.io.sfence <> integerBlock.io.fenceio.sfence
  frontend.io.tlbCsr <> integerBlock.io.csrio.tlb
  frontend.io.csrCtrl <> integerBlock.io.csrio.customCtrl

  frontend.io.icacheMemAcq <> l1pluscache.io.req
  l1pluscache.io.resp <> frontend.io.icacheMemGrant
  l1pluscache.io.flush := frontend.io.l1plusFlush
  frontend.io.fencei := integerBlock.io.fenceio.fencei

  ctrlBlock.io.csrCtrl <> integerBlock.io.csrio.customCtrl
  ctrlBlock.io.exuRedirect <> integerBlock.io.exuRedirect
  ctrlBlock.io.stIn <> memBlock.io.stIn
  ctrlBlock.io.stOut <> memBlock.io.stOut
  ctrlBlock.io.memoryViolation <> memBlock.io.memoryViolation
  ctrlBlock.io.enqLsq <> memBlock.io.enqLsq
  // TODO
  ctrlBlock.io.writeback <> VecInit(intArbiter.io.out ++ fpArbiter.io.out)

  scheduler.io.redirect <> ctrlBlock.io.redirect
  scheduler.io.flush <> ctrlBlock.io.flush
  scheduler.io.allocate <> ctrlBlock.io.enqIQ
  scheduler.io.issue <> integerBlock.io.issue ++ floatBlock.io.issue ++ memBlock.io.issue
  // TODO arbiter
  scheduler.io.writeback <> VecInit(intArbiter.io.out ++ fpArbiter.io.out)

  scheduler.io.replay <> memBlock.io.replay
  scheduler.io.rsIdx <> memBlock.io.rsIdx
  scheduler.io.isFirstIssue <> memBlock.io.isFirstIssue
  scheduler.io.stData <> memBlock.io.stData
  scheduler.io.otherFastWakeup <> memBlock.io.otherFastWakeup
  scheduler.io.jumpPc <> ctrlBlock.io.jumpPc
  scheduler.io.jalr_target <> ctrlBlock.io.jalr_target
  scheduler.io.stIssuePtr <> memBlock.io.stIssuePtr
  scheduler.io.debug_fp_rat <> ctrlBlock.io.debug_fp_rat
  scheduler.io.debug_int_rat <> ctrlBlock.io.debug_int_rat
  scheduler.io.readIntRf <> ctrlBlock.io.readIntRf
  scheduler.io.readFpRf <> ctrlBlock.io.readFpRf

  integerBlock.io.redirect <> ctrlBlock.io.redirect
  integerBlock.io.flush <> ctrlBlock.io.flush
  integerBlock.io.csrio.hartId <> io.hartId
  integerBlock.io.csrio.perf <> DontCare
  integerBlock.io.csrio.perf.retiredInstr <> ctrlBlock.io.roqio.toCSR.perfinfo.retiredInstr
  integerBlock.io.csrio.perf.ctrlInfo <> ctrlBlock.io.perfInfo.ctrlInfo
  integerBlock.io.csrio.perf.memInfo <> memBlock.io.memInfo
  integerBlock.io.csrio.perf.frontendInfo <> frontend.io.frontendInfo

  integerBlock.io.csrio.fpu.fflags <> ctrlBlock.io.roqio.toCSR.fflags
  integerBlock.io.csrio.fpu.isIllegal := false.B
  integerBlock.io.csrio.fpu.dirty_fs <> ctrlBlock.io.roqio.toCSR.dirty_fs
  integerBlock.io.csrio.fpu.frm <> floatBlock.io.frm
  integerBlock.io.csrio.exception <> ctrlBlock.io.roqio.exception
  integerBlock.io.csrio.isXRet <> ctrlBlock.io.roqio.toCSR.isXRet
  integerBlock.io.csrio.trapTarget <> ctrlBlock.io.roqio.toCSR.trapTarget
  integerBlock.io.csrio.interrupt <> ctrlBlock.io.roqio.toCSR.intrBitSet
  integerBlock.io.csrio.memExceptionVAddr <> memBlock.io.lsqio.exceptionAddr.vaddr
  integerBlock.io.csrio.externalInterrupt <> io.externalInterrupt

  floatBlock.io.redirect <> ctrlBlock.io.redirect
  floatBlock.io.flush <> ctrlBlock.io.flush

  integerBlock.io.fenceio.sfence <> memBlock.io.sfence
  integerBlock.io.fenceio.sbuffer <> memBlock.io.fenceToSbuffer

  memBlock.io.redirect <> ctrlBlock.io.redirect
  memBlock.io.flush <> ctrlBlock.io.flush
  memBlock.io.csrCtrl <> integerBlock.io.csrio.customCtrl
  memBlock.io.tlbCsr <> integerBlock.io.csrio.tlb
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
  itlbRepeater.io.sfence <> integerBlock.io.fenceio.sfence
  dtlbRepeater.io.sfence <> integerBlock.io.fenceio.sfence
  ptw.io.tlb(0) <> itlbRepeater.io.ptw
  ptw.io.tlb(1) <> dtlbRepeater.io.ptw
  ptw.io.sfence <> integerBlock.io.fenceio.sfence
  ptw.io.csr <> integerBlock.io.csrio.tlb

  // if l2 prefetcher use stream prefetch, it should be placed in XSCore
  assert(l2PrefetcherParameters._type == "bop")
  io.l2_pf_enable := integerBlock.io.csrio.customCtrl.l2_pf_enable

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
