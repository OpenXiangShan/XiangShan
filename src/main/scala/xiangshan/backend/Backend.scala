
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

package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils._
import utility._
import xiangshan._
import xiangshan.backend.exu._
import xiangshan.backend.fu._
import xiangshan.backend.rob._
import xiangshan.mem._


class FakeMemBlockWbSource()(implicit p: Parameters) extends LazyModule
  with HasXSParameter with HasWritebackSource {
  lazy val module = new FakeMemBlockWbSourceImp(this)

  override val writebackSourceParams: Seq[WritebackSourceParams] = {
    val params = new WritebackSourceParams
    params.exuConfigs = (loadExuConfigs ++ storeExuConfigs).map(cfg => Seq(cfg))
    Seq(params)
  }
  override lazy val writebackSourceImp: HasWritebackSourceImp = module
}

class FakeMemBlockWbSourceImp(outer: FakeMemBlockWbSource) extends LazyModuleImp(outer)
  with HasXSParameter
  with HasWritebackSourceImp
{
  val io = IO(new Bundle() {
    val in = Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, Flipped(DecoupledIO(new ExuOutput)))
    val out = Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, DecoupledIO(new ExuOutput))
  })
  override def writebackSource1: Option[Seq[Seq[DecoupledIO[ExuOutput]]]] = Some(Seq(io.out))
  io.out <> io.in
}

// Merge CtrlBlock, exuBlocks, wbArbiter, wb2Ctrl, etc into 1 module
class Backend(memWbSource: HasWritebackSource)(implicit p: Parameters) extends LazyModule 
  with HasXSParameter
  with HasExuWbHelper
{
  val wbArbiter = LazyModule(new WbArbiterWrapper(exuConfigs, NRIntWritePorts, NRFpWritePorts))
  val intWbPorts = wbArbiter.intWbPorts
  val fpWbPorts = wbArbiter.fpWbPorts

  // TODO: better RS organization
  // generate rs according to number of function units
  require(exuParameters.JmpCnt == 1)
  require(exuParameters.MduCnt <= exuParameters.AluCnt && exuParameters.MduCnt > 0)
  require(exuParameters.FmiscCnt <= exuParameters.FmacCnt && exuParameters.FmiscCnt > 0)
  require(exuParameters.LduCnt == exuParameters.StuCnt) // TODO: remove this limitation

  // one RS every 2 MDUs
  val schedulePorts = Seq(
    // exuCfg, numDeq, intFastWakeupTarget, fpFastWakeupTarget
    Seq(
      (AluExeUnitCfg, exuParameters.AluCnt, Seq(AluExeUnitCfg, LdExeUnitCfg, StaExeUnitCfg), Seq()),
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
  val lsDpPorts = (0 until exuParameters.LduCnt).map(i => Seq((3, i))) ++
                  (0 until exuParameters.StuCnt).map(i => Seq((4, i))) ++
                  (0 until exuParameters.StuCnt).map(i => Seq((5, i)))
  val fpDpPorts = (0 until exuParameters.FmacCnt).map(i => {
    if (i < 2 * exuParameters.FmiscCnt) Seq((0, i), (1, i))
    else Seq((0, i))
  })

  val dispatchPorts = Seq(intDpPorts ++ lsDpPorts, fpDpPorts)

  val outIntRfReadPorts = Seq(0, 0)
  val outFpRfReadPorts = Seq(0, StorePipelineWidth)
  val hasIntRf = Seq(true, false)
  val hasFpRf = Seq(false, true)

  val exuBlocks = schedulePorts.zip(dispatchPorts).zip(otherFastPorts).zipWithIndex.map {
    case (((sche, disp), other), i) =>
      LazyModule(new ExuBlock(sche, disp, intWbPorts, fpWbPorts, other, outIntRfReadPorts(i), outFpRfReadPorts(i), hasIntRf(i), hasFpRf(i)))
  }

  val fakeMemBlockWbSource = LazyModule(new FakeMemBlockWbSource())

  val wb2Ctrl = LazyModule(new Wb2Ctrl(exuConfigs))
  wb2Ctrl.addWritebackSink(exuBlocks :+ fakeMemBlockWbSource)
  val dpExuConfigs = exuBlocks.flatMap(_.scheduler.dispatch2.map(_.configs))
  val ctrlBlock = LazyModule(new CtrlBlock(dpExuConfigs))
  val writebackSources = Seq(Seq(wb2Ctrl), Seq(wbArbiter))
  writebackSources.foreach(s => ctrlBlock.addWritebackSink(s))

  lazy val module = new BackendImp(this)
}

class BackendImp(outer: Backend)(implicit p: Parameters) extends LazyModuleImp(outer)
  with HasXSParameter
{
  val io = IO(new Bundle() {
    val hartId = Input(UInt(64.W))
    val cpu_halt = Output(Bool())

    val memBlock = new Bundle() { // TODO: use class
      val redirect = ValidIO(new Redirect)
      val issue = Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, DecoupledIO(new ExuInput))
      val loadFastMatch = Vec(exuParameters.LduCnt, Output(UInt(exuParameters.LduCnt.W)))
      val loadFastImm = Vec(exuParameters.LduCnt, Output(UInt(12.W)))
      val rsfeedback = Vec(exuParameters.LsExuCnt, Flipped(new MemRSFeedbackIO))
      val loadPc = Vec(exuParameters.LduCnt, Output(UInt(VAddrBits.W)))
      val storePC = Vec(exuParameters.LduCnt, Output(UInt(VAddrBits.W)))
      val stIssuePtr = Input(new SqPtr())
      val writeback = Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, Flipped(DecoupledIO(new ExuOutput)))
      val s3_delayed_load_error = Vec(exuParameters.LduCnt, Input(Bool()))
      val otherFastWakeup = Vec(exuParameters.LduCnt + 2 * exuParameters.StuCnt, Flipped(ValidIO(new MicroOp)))
      val stIn = Vec(exuParameters.StuCnt, Flipped(ValidIO(new ExuInput)))
      val memoryViolation = Flipped(ValidIO(new Redirect))
      val sfence = Output(new SfenceBundle)
      val tlbCsr = Output(new TlbCsrBundle)
      val fenceToSbuffer = new FenceToSbuffer
      val enqLsq = Flipped(new LsqEnqIO)
      val lsqio = new Bundle {
        val exceptionAddr = Flipped(new ExceptionAddrIO) // to csr
        val rob = new RobLsqIO // rob to lsq
        val lqCanAccept = Input(Bool())
        val sqCanAccept = Input(Bool())
      }
      val csrCtrl = new CustomCSRCtrlIO
      val lqCancelCnt = Input(UInt(log2Up(VirtualLoadQueueSize + 1).W))
      val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))
      val scommit = Input(UInt(log2Ceil(EnsbufferWidth + 1).W))
      val lcommit = Input(UInt(log2Up(CommitWidth + 1).W))
      val debug_ls = Flipped(new DebugLSIO)
      val lsTopdownInfo = Vec(exuParameters.LduCnt, Input(new LsTopdownInfo))
    }

    val frontend = new Bundle() { // TODO: use class
      val frontend2Ctrl = Flipped(new FrontendToCtrlIO)
      val sfence = Output(new SfenceBundle)
      val tlbCsr = Output(new TlbCsrBundle)
      val csrCtrl = Output(new CustomCSRCtrlIO)
      val fencei = Output(Bool())
    }

    // CSR related
    val perf = Input(new PerfCounterIO)
    val externalInterrupt = new ExternalInterruptIO
    val distributedUpdate = Vec(2, Flipped(new DistributedCSRUpdateReq))

    val l2_pf_enable = Output(Bool())
  })

  val ctrlBlock = outer.ctrlBlock.module
  val wb2Ctrl = outer.wb2Ctrl.module
  val exuBlocks = outer.exuBlocks.map(_.module)
  val wbArbiter = outer.wbArbiter.module

  val mem = io.memBlock
  val frontend = io.frontend

  outer.fakeMemBlockWbSource.module.io.in <> mem.writeback

  ctrlBlock.io.hartId := io.hartId
  exuBlocks.foreach(_.io.hartId := io.hartId)
  wbArbiter.io.hartId := io.hartId

  io.cpu_halt := ctrlBlock.io.cpu_halt

  wbArbiter.io.redirect <> ctrlBlock.io.redirect

  val allWriteback = exuBlocks.flatMap(_.io.fuWriteback) ++ outer.fakeMemBlockWbSource.module.io.out
  require(exuConfigs.length == allWriteback.length, s"${exuConfigs.length} != ${allWriteback.length}")
  wbArbiter.io.in <> allWriteback
  val rfWriteback = wbArbiter.io.out

  // memblock error exception writeback, 1 cycle after normal writeback
  wb2Ctrl.io.s3_delayed_load_error <> mem.s3_delayed_load_error

  wb2Ctrl.io.redirect <> ctrlBlock.io.redirect
  outer.wb2Ctrl.generateWritebackIO()

  require(exuBlocks.count(_.fuConfigs.map(_._1).contains(JumpCSRExeUnitCfg)) == 1)
  val csrFenceMod = exuBlocks.filter(_.fuConfigs.map(_._1).contains(JumpCSRExeUnitCfg)).head
  val csrioIn = csrFenceMod.io.fuExtra.csrio.get
  val fenceio = csrFenceMod.io.fuExtra.fenceio.get

  ctrlBlock.io.frontend <> frontend.frontend2Ctrl
  frontend.sfence <> fenceio.sfence
  frontend.tlbCsr <> csrioIn.tlb
  frontend.csrCtrl <> csrioIn.customCtrl
  frontend.fencei := fenceio.fencei

  ctrlBlock.io.csrCtrl <> csrioIn.customCtrl
  val redirectBlocks = exuBlocks.reverse.filter(_.fuConfigs.map(_._1).map(_.hasRedirect).reduce(_ || _))
  ctrlBlock.io.exuRedirect <> redirectBlocks.flatMap(_.io.fuExtra.exuRedirect)
  ctrlBlock.io.stIn <> mem.stIn
  ctrlBlock.io.memoryViolation <> mem.memoryViolation
  exuBlocks.head.io.scheExtra.enqLsq.get <> mem.enqLsq
  exuBlocks.foreach(b => {
    b.io.scheExtra.lcommit := mem.lcommit
    b.io.scheExtra.scommit := mem.scommit
    b.io.scheExtra.lqCancelCnt := mem.lqCancelCnt
    b.io.scheExtra.sqCancelCnt := mem.sqCancelCnt
  })
  val sourceModules = outer.writebackSources.map(_.map(_.module.asInstanceOf[HasWritebackSourceImp]))
  outer.ctrlBlock.generateWritebackIO()

  val allFastUop = exuBlocks.flatMap(b => b.io.fastUopOut.dropRight(b.numOutFu)) ++ mem.otherFastWakeup
  require(allFastUop.length == exuConfigs.length, s"${allFastUop.length} != ${exuConfigs.length}")
  val intFastUop = allFastUop.zip(exuConfigs).filter(_._2.writeIntRf).map(_._1)
  val fpFastUop = allFastUop.zip(exuConfigs).filter(_._2.writeFpRf).map(_._1)
  val intFastUop1 = outer.wbArbiter.intConnections.map(c => intFastUop(c.head))
  val fpFastUop1 = outer.wbArbiter.fpConnections.map(c => fpFastUop(c.head))
  val allFastUop1 = intFastUop1 ++ fpFastUop1

  ctrlBlock.io.dispatch <> exuBlocks.flatMap(_.io.in)
  ctrlBlock.io.rsReady := exuBlocks.flatMap(_.io.scheExtra.rsReady)
  ctrlBlock.io.enqLsq <> mem.enqLsq
  ctrlBlock.io.lqDeq := mem.lcommit
  ctrlBlock.io.sqDeq := mem.scommit
  ctrlBlock.io.lqCanAccept := mem.lsqio.lqCanAccept
  ctrlBlock.io.sqCanAccept := mem.lsqio.sqCanAccept
  ctrlBlock.io.lqCancelCnt := mem.lqCancelCnt
  ctrlBlock.io.sqCancelCnt := mem.sqCancelCnt
  ctrlBlock.io.robHeadLsIssue := exuBlocks.map(_.io.scheExtra.robHeadLsIssue).reduce(_ || _)

  exuBlocks(0).io.scheExtra.fpRfReadIn.get <> exuBlocks(1).io.scheExtra.fpRfReadOut.get
  exuBlocks(0).io.scheExtra.fpStateReadIn.get <> exuBlocks(1).io.scheExtra.fpStateReadOut.get

  for((c, e) <- ctrlBlock.io.ld_pc_read.zip(exuBlocks(0).io.issue.get)){
    // read load pc at load s0
    c.ptr := e.bits.uop.cf.ftqPtr
    c.offset := e.bits.uop.cf.ftqOffset
  }
  // return load pc at load s2
  mem.loadPc <> VecInit(ctrlBlock.io.ld_pc_read.map(_.data))

  for((c, e) <- ctrlBlock.io.st_pc_read.zip(exuBlocks(0).io.issue.get.drop(exuParameters.LduCnt))){
    // read store pc at store s0
    c.ptr := e.bits.uop.cf.ftqPtr
    c.offset := e.bits.uop.cf.ftqOffset
  }
  // return store pc at load s2
  mem.storePc <> VecInit(ctrlBlock.io.st_pc_read.map(_.data))

  mem.issue <> exuBlocks(0).io.issue.get
  // By default, instructions do not have exceptions when they enter the function units.
  mem.issue.map(_.bits.uop.clearExceptions())
  exuBlocks(0).io.scheExtra.loadFastMatch.get <> mem.loadFastMatch
  exuBlocks(0).io.scheExtra.loadFastImm.get <> mem.loadFastImm

  val stdIssue = exuBlocks(0).io.issue.get.takeRight(exuParameters.StuCnt)
  exuBlocks.map(_.io).foreach { exu =>
    exu.redirect <> ctrlBlock.io.redirect
    exu.allocPregs <> ctrlBlock.io.allocPregs
    exu.rfWriteback <> rfWriteback
    exu.fastUopIn <> allFastUop1
    exu.scheExtra.jumpPc <> ctrlBlock.io.jumpPc
    exu.scheExtra.jalr_target <> ctrlBlock.io.jalr_target
    exu.scheExtra.stIssuePtr <> mem.stIssuePtr
    exu.scheExtra.debug_fp_rat <> ctrlBlock.io.debug_fp_rat
    exu.scheExtra.debug_int_rat <> ctrlBlock.io.debug_int_rat
    exu.scheExtra.robDeqPtr := ctrlBlock.io.robDeqPtr
    exu.scheExtra.memWaitUpdateReq.staIssue.zip(mem.stIn).foreach{case (sink, src) => {
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

  ctrlBlock.perfinfo.perfEventsEu0 := exuBlocks(0).getPerf.dropRight(outer.exuBlocks(0).scheduler.numRs)
  ctrlBlock.perfinfo.perfEventsEu1 := exuBlocks(1).getPerf.dropRight(outer.exuBlocks(1).scheduler.numRs)
  ctrlBlock.perfinfo.perfEventsRs  := outer.exuBlocks.flatMap(b => b.module.getPerf.takeRight(b.scheduler.numRs))

  csrioIn.hartId <> io.hartId

  val perf = WireInit(io.perf) // other perf events are assigned outside the backend
  perf.retiredInstr <> ctrlBlock.io.robio.toCSR.perfinfo.retiredInstr
  perf.ctrlInfo <> ctrlBlock.io.perfInfo.ctrlInfo
  perf.perfEventsCtrl <> ctrlBlock.getPerf
  csrioIn.perf <> perf

  csrioIn.fpu.fflags <> ctrlBlock.io.robio.toCSR.fflags
  csrioIn.fpu.isIllegal := false.B
  csrioIn.fpu.dirty_fs <> ctrlBlock.io.robio.toCSR.dirty_fs
  csrioIn.fpu.frm <> exuBlocks(1).io.fuExtra.frm.get
  csrioIn.exception <> ctrlBlock.io.robio.exception
  csrioIn.isXRet <> ctrlBlock.io.robio.toCSR.isXRet
  csrioIn.trapTarget <> ctrlBlock.io.robio.toCSR.trapTarget
  csrioIn.interrupt <> ctrlBlock.io.robio.toCSR.intrBitSet
  csrioIn.wfi_event <> ctrlBlock.io.robio.toCSR.wfiEvent
  csrioIn.memExceptionVAddr <> mem.lsqio.exceptionAddr.vaddr

  csrioIn.externalInterrupt := io.externalInterrupt

  csrioIn.distributedUpdate := io.distributedUpdate

  mem.sfence <> fenceio.sfence
  mem.fenceToSbuffer <> fenceio.sbuffer

  mem.redirect <> ctrlBlock.io.redirect
  mem.rsfeedback <> exuBlocks(0).io.scheExtra.feedback.get
  mem.csrCtrl <> csrioIn.customCtrl
  mem.tlbCsr <> csrioIn.tlb
  mem.lsqio.rob <> ctrlBlock.io.robio.lsq
  mem.lsqio.exceptionAddr.isStore := CommitType.lsInstIsStore(ctrlBlock.io.robio.exception.bits.uop.ctrl.commitType)
  mem.debug_ls <> ctrlBlock.io.robio.debug_ls
  mem.lsTopdownInfo <> ctrlBlock.io.robio.lsTopdownInfo

  // if l2 prefetcher use stream prefetch, it should be placed in XSCore
  io.l2_pf_enable := csrioIn.customCtrl.l2_pf_enable

  val resetTree = ResetGenNode(
    exuBlocks.tail.map(m => ModuleNode(m))
      :+ ModuleNode(wbArbiter)
      :+ ModuleNode(ctrlBlock)
  )
  ResetGen(resetTree, reset, !p(DebugOptionsKey).FPGAPlatform)
}