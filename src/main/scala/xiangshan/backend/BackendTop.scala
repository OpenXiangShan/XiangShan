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
import huancun.utils.{DFTResetGen, ModuleNode, ResetGen, ResetGenNode}
import xiangshan._
import xiangshan.backend._
import xiangshan.backend.exu.{ExuConfig, Wb2Ctrl, WbArbiterWrapper, FenceIO}
import xiangshan.backend.rob.RobLsqIO
import xiangshan.backend.fu.CSRFileIO
import xiangshan.mem.{LsqEnqIO, SqPtr}
import xiangshan.frontend.FtqRead
import system.HasSoCParameter


class BackendTop(implicit p: Parameters) extends LazyModule 
  with HasWritebackSource
  with HasXSParameter 
  with HasExuWbHelper{

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

  val wb2Ctrl = LazyModule(new Wb2Ctrl(exuConfigs))
  wb2Ctrl.addWritebackSink(exuBlocks :+ this)
  val dpExuConfigs = exuBlocks.flatMap(_.scheduler.dispatch2.map(_.configs))
  val ctrlBlock = LazyModule(new CtrlBlock(dpExuConfigs))
  val writebackSources = Seq(Seq(wb2Ctrl), Seq(wbArbiter))
  writebackSources.foreach(s => ctrlBlock.addWritebackSink(s))

  lazy val module = new BackendTopImp(this)

  override val writebackSourceParams: Seq[WritebackSourceParams] = {
    val params = new WritebackSourceParams
    params.exuConfigs = (loadExuConfigs ++ storeExuConfigs).map(cfg => Seq(cfg))
    Seq(params)
  }
  override lazy val writebackSourceImp: HasWritebackSourceImp = module
}

class BackendTopImp(outer: BackendTop)(implicit p: Parameters) extends LazyModuleImp(outer)
  with HasWritebackSourceImp 
  with HasXSParameter
  with HasSoCParameter{

  val ctrlBlock = outer.ctrlBlock.module
  val wb2Ctrl = outer.wb2Ctrl.module
  val exuBlocks = outer.exuBlocks.map(_.module)

  val updatedP = p.alter((site, here, up) => {
    case XSCoreParamsKey => up(XSCoreParamsKey).copy(
      IssQueSize = outer.exuBlocks.head.scheduler.memRsEntries.max
    )
  })

  val io = IO(new Bundle {
    // XSCore interface
    val hartId = Input(UInt(64.W))
    val cpu_halt = Output(Bool())
    val dfx_reset = Input(new DFTResetGen)
    
    // val perfEvents = Input(Vec(numPCntHc * coreParams.L2NBanks, new PerfEvent))
    // from MemBlock
    val s3_delayed_load_error = Vec(LoadPipelineWidth, Input(Bool())) // Dirty fix of data ecc error timing
    // 
    val csrio   = new CSRFileIO
    val fenceio = new FenceIO
    
    // frontend
    val frontend = Flipped(new FrontendToCtrlIO)
    // memblock
    val stIn = Vec(exuParameters.StuCnt, Flipped(ValidIO(new ExuInput)))
    val memoryViolation = Flipped(ValidIO(new Redirect))
    // 
    val enqLsq = Flipped(new LsqEnqIO)
  
    val sqDeq = Input(UInt(log2Up(CommitWidth + 1).W))
    val lqCancelCnt = Input(UInt(log2Up(LoadQueueSize + 1).W))
    val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))

    // from memBlock
    val otherFastWakeup = Flipped(Vec(exuParameters.LduCnt + 2 * exuParameters.StuCnt, ValidIO(new MicroOp)))

    val ld_pc_read = Vec(exuParameters.LduCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))

    val issue = Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, DecoupledIO(new ExuInput))

    val loadFastMatch = Vec(exuParameters.LduCnt, Output(UInt(exuParameters.LduCnt.W)))
    val loadFastImm   = Vec(exuParameters.LduCnt, Output(UInt(12.W)))

    // from memblock
    val stIssuePtr = Input(new SqPtr())

    val redirect = ValidIO(new Redirect)
    val rsfeedback = Vec(exuParameters.LsExuCnt, Flipped(new MemRSFeedbackIO()(updatedP)))

    val robio = new Bundle {
      // to int block
      val exception = ValidIO(new ExceptionInfo)
      // to mem block
      val lsq = new RobLsqIO
    }

    val writeback = Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, Flipped(DecoupledIO(new ExuOutput)))
  })
  override def writebackSource1: Option[Seq[Seq[DecoupledIO[ExuOutput]]]] = Some(Seq(io.writeback))

  ctrlBlock.io.hartId := io.hartId
  exuBlocks.foreach(_.io.hartId := io.hartId)
  outer.wbArbiter.module.io.hartId := io.hartId

  io.cpu_halt := ctrlBlock.io.cpu_halt

  io.cpu_halt := ctrlBlock.io.cpu_halt

  outer.wbArbiter.module.io.redirect <> ctrlBlock.io.redirect
  val allWriteback = exuBlocks.flatMap(_.io.fuWriteback) ++ io.writeback

  require(exuConfigs.length == allWriteback.length, s"${exuConfigs.length} != ${allWriteback.length}")
  outer.wbArbiter.module.io.in <> allWriteback
  val rfWriteback = outer.wbArbiter.module.io.out

  wb2Ctrl.io.s3_delayed_load_error <> io.s3_delayed_load_error

  wb2Ctrl.io.redirect <> ctrlBlock.io.redirect

  outer.wb2Ctrl.generateWritebackIO(Some(outer), Some(this))

  require(exuBlocks.count(_.fuConfigs.map(_._1).contains(JumpCSRExeUnitCfg)) == 1)
  val csrFenceMod = exuBlocks.filter(_.fuConfigs.map(_._1).contains(JumpCSRExeUnitCfg)).head
  val csrioIn = csrFenceMod.io.fuExtra.csrio.get
  val fenceio = csrFenceMod.io.fuExtra.fenceio.get
  io.csrio <> csrioIn
  io.fenceio <> fenceio

  io.frontend <> ctrlBlock.io.frontend

  ctrlBlock.io.csrCtrl <> csrioIn.customCtrl

  val redirectBlocks = exuBlocks.reverse.filter(_.fuConfigs.map(_._1).map(_.hasRedirect).reduce(_ || _))
  ctrlBlock.io.exuRedirect <> redirectBlocks.flatMap(_.io.fuExtra.exuRedirect)
  ctrlBlock.io.stIn <> io.stIn
  ctrlBlock.io.memoryViolation <> io.memoryViolation
  exuBlocks.head.io.scheExtra.enqLsq.get <> io.enqLsq
  exuBlocks.foreach(b => {
    b.io.scheExtra.lcommit := ctrlBlock.io.robio.lsq.lcommit
    b.io.scheExtra.scommit := io.sqDeq
    b.io.scheExtra.lqCancelCnt := io.lqCancelCnt
    b.io.scheExtra.sqCancelCnt := io.sqCancelCnt
  })

  val sourceModules = outer.writebackSources.map(_.map(_.module.asInstanceOf[HasWritebackSourceImp]))
  outer.ctrlBlock.generateWritebackIO()

  val allFastUop = exuBlocks.flatMap(b => b.io.fastUopOut.dropRight(b.numOutFu)) ++ io.otherFastWakeup
  require(allFastUop.length == exuConfigs.length, s"${allFastUop.length} != ${exuConfigs.length}")
  val intFastUop = allFastUop.zip(exuConfigs).filter(_._2.writeIntRf).map(_._1)
  val fpFastUop = allFastUop.zip(exuConfigs).filter(_._2.writeFpRf).map(_._1)
  val intFastUop1 = outer.wbArbiter.intConnections.map(c => intFastUop(c.head))
  val fpFastUop1 = outer.wbArbiter.fpConnections.map(c => fpFastUop(c.head))
  val allFastUop1 = intFastUop1 ++ fpFastUop1


  ctrlBlock.io.dispatch <> exuBlocks.flatMap(_.io.in)
  ctrlBlock.io.rsReady := exuBlocks.flatMap(_.io.scheExtra.rsReady)
  ctrlBlock.io.enqLsq <> io.enqLsq
  ctrlBlock.io.sqDeq := io.sqDeq
  ctrlBlock.io.lqCancelCnt := io.lqCancelCnt
  ctrlBlock.io.sqCancelCnt := io.sqCancelCnt


  exuBlocks(0).io.scheExtra.fpRfReadIn.get <> exuBlocks(1).io.scheExtra.fpRfReadOut.get
  exuBlocks(0).io.scheExtra.fpStateReadIn.get <> exuBlocks(1).io.scheExtra.fpStateReadOut.get

  for((c, e) <- ctrlBlock.io.ld_pc_read.zip(exuBlocks(0).io.issue.get)){
    // read load pc at load s0
    c.ptr := e.bits.uop.cf.ftqPtr
    c.offset := e.bits.uop.cf.ftqOffset
  }

  io.ld_pc_read <> ctrlBlock.io.ld_pc_read
  io.issue <> exuBlocks(0).io.issue.get

  io.loadFastMatch := exuBlocks(0).io.scheExtra.loadFastMatch.get
  io.loadFastImm   := exuBlocks(0).io.scheExtra.loadFastImm.get

  val stdIssue = exuBlocks(0).io.issue.get.takeRight(exuParameters.StuCnt)
  exuBlocks.map(_.io).foreach { exu =>
    exu.redirect <> ctrlBlock.io.redirect
    exu.allocPregs <> ctrlBlock.io.allocPregs
    exu.rfWriteback <> rfWriteback
    exu.fastUopIn <> allFastUop1
    exu.scheExtra.jumpPc <> ctrlBlock.io.jumpPc
    exu.scheExtra.jalr_target <> ctrlBlock.io.jalr_target
    exu.scheExtra.stIssuePtr <> io.stIssuePtr
    exu.scheExtra.debug_fp_rat <> ctrlBlock.io.debug_fp_rat
    exu.scheExtra.debug_int_rat <> ctrlBlock.io.debug_int_rat
    exu.scheExtra.memWaitUpdateReq.staIssue.zip(io.stIn).foreach{case (sink, src) => {
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

  csrioIn.perf.retiredInstr <> ctrlBlock.io.robio.toCSR.perfinfo.retiredInstr
  csrioIn.perf.ctrlInfo <> ctrlBlock.io.perfInfo.ctrlInfo

  csrioIn.perf.perfEventsCtrl     <> ctrlBlock.getPerf

  csrioIn.fpu.fflags <> ctrlBlock.io.robio.toCSR.fflags
  csrioIn.fpu.isIllegal := false.B
  csrioIn.fpu.dirty_fs <> ctrlBlock.io.robio.toCSR.dirty_fs
  csrioIn.fpu.frm <> exuBlocks(1).io.fuExtra.frm.get
  csrioIn.exception <> ctrlBlock.io.robio.exception
  csrioIn.isXRet <> ctrlBlock.io.robio.toCSR.isXRet
  csrioIn.trapTarget <> ctrlBlock.io.robio.toCSR.trapTarget
  csrioIn.interrupt <> ctrlBlock.io.robio.toCSR.intrBitSet
  csrioIn.wfi_event <> ctrlBlock.io.robio.toCSR.wfiEvent

  io.redirect <> ctrlBlock.io.redirect

  io.rsfeedback <> exuBlocks(0).io.scheExtra.feedback.get
  
  io.robio.exception := ctrlBlock.io.robio.exception
  io.robio.lsq <> ctrlBlock.io.robio.lsq

  // Modules are reset one by one
  val resetTree = ResetGenNode(
    Seq(
      ResetGenNode(Seq(
        ModuleNode(exuBlocks.head),
        ResetGenNode(
          exuBlocks.tail.map(m => ModuleNode(m)) :+ ModuleNode(outer.wbArbiter.module) :+ ModuleNode(wb2Ctrl)
        ),
        ResetGenNode(Seq(
          ModuleNode(ctrlBlock),
        ))
      ))
    )
  )

  ResetGen(resetTree, reset, !debugOpts.FPGAPlatform, Some(io.dfx_reset))
}
