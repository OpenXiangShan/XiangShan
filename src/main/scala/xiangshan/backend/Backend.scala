package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.{PipelineConnect, ZeroExt}
import xiangshan._
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput, FuBusyTableWriteBundle}
import xiangshan.backend.ctrlblock.CtrlBlock
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.datapath.{DataPath, WbDataPath}
import xiangshan.backend.exu.ExuBlock
import xiangshan.backend.fu.vector.Bundles.{VConfig, VType}
import xiangshan.backend.fu.{FenceIO, FenceToSbuffer, PerfCounterIO, FuConfig}
import xiangshan.backend.issue.{Scheduler, IntScheduler, MemScheduler, VfScheduler}
import xiangshan.backend.rob.RobLsqIO
import xiangshan.frontend.{FtqPtr, FtqRead}
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}

class Backend(val params: BackendParams)(implicit p: Parameters) extends LazyModule
  with HasXSParameter {

  for (exuCfg <- params.allExuParams) {
    val fuConfigs = exuCfg.fuConfigs
    val wbPortConfigs = exuCfg.wbPortConfigs
    val immType = exuCfg.immType
    println(s"exu: ${fuConfigs.map(_.name)}, wb: ${wbPortConfigs}, imm: ${immType}")
    require(wbPortConfigs.collectFirst { case x: IntWB => x }.nonEmpty ==
      fuConfigs.map(_.writeIntRf).reduce(_ || _),
      "int wb port has no priority" )
    require(wbPortConfigs.collectFirst { case x: VfWB => x }.nonEmpty ==
      fuConfigs.map(x => x.writeFpRf || x.writeVecRf).reduce(_ || _),
      "vec wb port has no priority" )
  }

  println(s"Function Unit: Alu(${params.AluCnt}), Brh(${params.BrhCnt}), Jmp(${params.JmpCnt}), " +
    s"Ldu(${params.LduCnt}), Sta(${params.StaCnt}), Std(${params.StdCnt})")

  for (cfg <- FuConfig.allConfigs) {
    println(s"[Backend] $cfg")
  }

  val ctrlBlock = LazyModule(new CtrlBlock(params))
  val intScheduler = params.intSchdParams.map(x => LazyModule(new Scheduler(x)))
  val vfScheduler = params.vfSchdParams.map(x => LazyModule(new Scheduler(x)))
  val memScheduler = params.memSchdParams.map(x => LazyModule(new Scheduler(x)))
  val dataPath = LazyModule(new DataPath(params))
  val intExuBlock = params.intSchdParams.map(x => LazyModule(new ExuBlock(x)))
  val vfExuBlock = params.vfSchdParams.map(x => LazyModule(new ExuBlock(x)))

  lazy val module = new BackendImp(this)
}

class BackendImp(override val wrapper: Backend)(implicit p: Parameters) extends LazyModuleImp(wrapper)
  with HasXSParameter{
  implicit private val params = wrapper.params
  val io = IO(new BackendIO()(p, wrapper.params))

  private val ctrlBlock = wrapper.ctrlBlock.module
  private val intScheduler = wrapper.intScheduler.get.module
  private val vfScheduler = wrapper.vfScheduler.get.module
  private val memScheduler = wrapper.memScheduler.get.module
  private val dataPath = wrapper.dataPath.module
  private val intExuBlock = wrapper.intExuBlock.get.module
  private val vfExuBlock = wrapper.vfExuBlock.get.module
  private val wbDataPath = Module(new WbDataPath(params))

  private val (intRespWrite, vfRespWrite, memRespWrite) = (intScheduler.io.wbFuBusyTable.fuBusyTableWrite,
    vfScheduler.io.wbFuBusyTable.fuBusyTableWrite,
    memScheduler.io.wbFuBusyTable.fuBusyTableWrite)
  private val (intRespRead, vfRespRead, memRespRead) = (intScheduler.io.wbFuBusyTable.fuBusyTableRead,
    vfScheduler.io.wbFuBusyTable.fuBusyTableRead,
    memScheduler.io.wbFuBusyTable.fuBusyTableRead)
  private val allRespWrite = (intRespWrite ++ vfRespWrite ++ memRespWrite).flatten
  private val allRespRead = (intRespRead ++ vfRespRead ++ memRespRead).flatten
  wbDataPath.io.fromIntExu.flatten.filter(x => x.bits.params.writeIntRf)
  private val allExuParams = params.allExuParams
  private val respWriteWithParams = allRespWrite.zip(allExuParams)
  println(s"[respWriteWithParams] is ${respWriteWithParams}")
  respWriteWithParams.foreach{ case(l,r) =>
    println(s"FuBusyTableWriteBundle is ${l}, ExeUnitParams is ${r}")
  }
//  require(false)

  private val intWBFuGroup = params.getIntWBExeGroup.map{case(groupId, exeUnit) => (groupId, exeUnit.flatMap(_.fuConfigs))}
  private val intLatencyCertains = intWBFuGroup.map{case (k,v) => (k, v.map(_.latency.latencyVal.nonEmpty).reduce(_ && _))}
  private val intWBFuLatencyMap = intLatencyCertains.map{case (k, latencyCertain) =>
    if (latencyCertain) Some(intWBFuGroup(k).map(y => (y.fuType, y.latency.latencyVal.get)))
    else None
  }.toSeq
  private val intWBFuLatencyValMax = intWBFuLatencyMap.map(latencyMap=> latencyMap.map(x => x.map(_._2).max))

  private val vfWBFuGroup = params.getVfWBExeGroup.map{case(groupId, exeUnit) => (groupId, exeUnit.flatMap(_.fuConfigs))}
  private val vfLatencyCertains = vfWBFuGroup.map{case (k,v) => (k, v.map(_.latency.latencyVal.nonEmpty).reduce(_ && _))}
  val vfWBFuLatencyMap = vfLatencyCertains.map { case (k, latencyCertain) =>
    if (latencyCertain) Some(vfWBFuGroup(k).map(y => (y.fuType, y.latency.latencyVal.get)))
    else None
  }.toSeq
  private val vfWBFuLatencyValMax = vfWBFuLatencyMap.map(latencyMap => latencyMap.map(x => x.map(_._2).max))

  private val intWBFuBusyTable = intWBFuLatencyValMax.map { case y => if (y.getOrElse(0)>0) Some(Reg(UInt(y.getOrElse(1).W))) else None }
  println(s"[intWBFuBusyTable] is ${intWBFuBusyTable.map(x => x) }")
  private val vfWBFuBusyTable = vfWBFuLatencyValMax.map { case y => if (y.getOrElse(0)>0) Some(Reg(UInt(y.getOrElse(1).W))) else None }

  intWBFuBusyTable.map(x => x.map(dontTouch(_)))
  dontTouch(intScheduler.io.wbFuBusyTable)
  dontTouch(vfScheduler.io.wbFuBusyTable)
  dontTouch(memScheduler.io.wbFuBusyTable)

  // intWBFuBusyTable write
  for (i <- 0 until intWBFuGroup.size) {
    if (intWBFuBusyTable(i).nonEmpty){
      val deqIsLatencyNumMask = respWriteWithParams.zipWithIndex.map{ case((r, p), idx) =>
        val resps = p.schdType match {
          case IntScheduler() => Seq(r.deqResp, r.og0Resp, r.og1Resp)
          case MemScheduler() => Seq(r.deqResp, r.og1Resp)
          case VfScheduler() => Seq(r.deqResp, r.og1Resp)
          case _ => null
        }
        val matchI = (p.wbPortConfigs.collectFirst{ case x: IntWB => x }.getOrElse(-1)) == i
        if(matchI){
          Mux(resps(0).valid && resps(0).bits.respType === RSFeedbackType.issueSuccess,
            Cat((0 until intWBFuLatencyValMax(i).getOrElse(0)).map { case num =>
            val latencyNumFuType = p.fuConfigs.filter(_.latency.latencyVal.getOrElse(-1) == num+1).map(_.fuType)
            val isLatencyNum = Cat(latencyNumFuType.map(futype => resps(0).bits.fuType === futype.U)).asUInt().orR() // The latency of the deqResp inst is Num
            isLatencyNum
            }),
            0.U)
        } else 0.U
      }.reduce(_|_)
      val og0IsLatencyNumMask = WireInit(-1.S.asTypeOf(deqIsLatencyNumMask))
      og0IsLatencyNumMask := respWriteWithParams.zipWithIndex.map { case ((r, p), idx) =>
        val resps = p.schdType match {
          case IntScheduler() => Seq(r.deqResp, r.og0Resp, r.og1Resp)
          case MemScheduler() => Seq(r.deqResp, r.og1Resp)
          case VfScheduler() => Seq(r.deqResp, r.og1Resp)
          case _ => null
        }
        val matchI = (p.wbPortConfigs.collectFirst { case x: IntWB => x }.getOrElse(-1)) == i
        if (matchI) {
          Mux(resps(1).valid && resps(1).bits.respType === RSFeedbackType.issueSuccess,
            ~(Cat(Cat((0 until intWBFuLatencyValMax(i).getOrElse(0)).map { case num =>
              val latencyNumFuType = p.fuConfigs.filter(_.latency.latencyVal.getOrElse(-1) == num + 1).map(_.fuType)
              val isLatencyNum = Cat(latencyNumFuType.map(futype => resps(1).bits.fuType === futype.U)).asUInt().orR() // The latency of the deqResp inst is Num
              isLatencyNum
            }), 0.U(1.W))),
            -1.S.asTypeOf(deqIsLatencyNumMask)).asTypeOf(deqIsLatencyNumMask)
        } else -1.S.asTypeOf(deqIsLatencyNumMask)
      }.reduce(_&_)
      val og1IsLatencyNumMask = WireInit(-1.S.asTypeOf(deqIsLatencyNumMask))
      og1IsLatencyNumMask := respWriteWithParams.zipWithIndex.map { case ((r, p), idx) =>
        val resps = p.schdType match {
          case IntScheduler() => Seq(r.deqResp, r.og0Resp, r.og1Resp)
          case MemScheduler() => Seq(r.deqResp, r.og1Resp)
          case VfScheduler() => Seq(r.deqResp, r.og1Resp)
          case _ => null
        }
        val matchI = (p.wbPortConfigs.collectFirst { case x: IntWB => x }.getOrElse(-1)) == i
        if (matchI && resps.length==3) {
          Mux(resps(2).valid && resps(2).bits.respType === RSFeedbackType.issueSuccess,
            ~(Cat(Cat((0 until intWBFuLatencyValMax(i).getOrElse(0)).map { case num =>
              val latencyNumFuType = p.fuConfigs.filter(_.latency.latencyVal.getOrElse(-1) == num + 1).map(_.fuType)
              val isLatencyNum = Cat(latencyNumFuType.map(futype => resps(2).bits.fuType === futype.U)).asUInt().orR() // The latency of the deqResp inst is Num
              isLatencyNum
            }), 0.U(1.W))),
            -1.S.asTypeOf(deqIsLatencyNumMask)).asTypeOf(deqIsLatencyNumMask)
        } else -1.S.asTypeOf(deqIsLatencyNumMask)
      }.reduce(_ & _)
      dontTouch(deqIsLatencyNumMask)
      dontTouch(og0IsLatencyNumMask)
      dontTouch(og1IsLatencyNumMask)
      intWBFuBusyTable(i).get := ((intWBFuBusyTable(i).get << 1.U).asUInt() | deqIsLatencyNumMask) & og0IsLatencyNumMask.asUInt() & og1IsLatencyNumMask.asUInt()
    }
  }
  // intWBFuBusyTable read
  for(i <- 0 until allRespRead.size){
    allRespRead(i) := intWBFuBusyTable.zipWithIndex.map{ case (ele, idx) =>
      val matchI = (allExuParams(i).wbPortConfigs.collectFirst { case x: IntWB => x }.getOrElse(-1)) == idx
      if(ele.nonEmpty && matchI){
        ele.get.asTypeOf(allRespRead(i))
      }else{
        0.U.asTypeOf(allRespRead(i))
      }
    }.reduce(_|_)
  }

  ctrlBlock.io.fromTop.hartId := io.fromTop.hartId
  ctrlBlock.io.frontend <> io.frontend
  ctrlBlock.io.fromWB.wbData <> wbDataPath.io.toCtrlBlock.writeback
  ctrlBlock.io.fromMem.stIn <> io.mem.stIn
  ctrlBlock.io.fromMem.violation <> io.mem.memoryViolation
  ctrlBlock.io.csrCtrl <> intExuBlock.io.csrio.get.customCtrl
  ctrlBlock.io.robio.csr.intrBitSet := intExuBlock.io.csrio.get.interrupt
  ctrlBlock.io.robio.csr.trapTarget := intExuBlock.io.csrio.get.trapTarget
  ctrlBlock.io.robio.csr.isXRet := intExuBlock.io.csrio.get.isXRet
  ctrlBlock.io.robio.csr.wfiEvent := intExuBlock.io.csrio.get.wfi_event
  ctrlBlock.io.robio.lsq <> io.mem.robLsqIO

  intScheduler.io.fromTop.hartId := io.fromTop.hartId
  intScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  intScheduler.io.fromCtrlBlock.pcVec := ctrlBlock.io.toIssueBlock.pcVec
  intScheduler.io.fromCtrlBlock.targetVec := ctrlBlock.io.toIssueBlock.targetVec
  intScheduler.io.fromDispatch.allocPregs <> ctrlBlock.io.toIssueBlock.allocPregs
  intScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.intUops
  intScheduler.io.intWriteBack := wbDataPath.io.toIntPreg
  intScheduler.io.vfWriteBack := 0.U.asTypeOf(intScheduler.io.vfWriteBack)

  memScheduler.io.fromTop.hartId := io.fromTop.hartId
  memScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  memScheduler.io.fromDispatch.allocPregs <> ctrlBlock.io.toIssueBlock.allocPregs
  memScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.memUops
  memScheduler.io.intWriteBack := wbDataPath.io.toIntPreg
  memScheduler.io.vfWriteBack := wbDataPath.io.toVfPreg
  memScheduler.io.fromMem.get.scommit := io.mem.sqDeq
  memScheduler.io.fromMem.get.lcommit := io.mem.lqDeq
  memScheduler.io.fromMem.get.sqCancelCnt := io.mem.sqCancelCnt
  memScheduler.io.fromMem.get.lqCancelCnt := io.mem.lqCancelCnt
  memScheduler.io.fromMem.get.stIssuePtr := io.mem.stIssuePtr
  memScheduler.io.fromMem.get.memWaitUpdateReq.staIssue.zip(io.mem.stIn).foreach { case (sink, source) =>
    sink.valid := source.valid
    sink.bits.uop := 0.U.asTypeOf(sink.bits.uop)
    sink.bits.uop.robIdx := source.bits.robIdx
  }
  io.mem.ldaIqFeedback <> memScheduler.io.fromMem.get.ldaFeedback
  io.mem.staIqFeedback <> memScheduler.io.fromMem.get.staFeedback

  vfScheduler.io.fromTop.hartId := io.fromTop.hartId
  vfScheduler.io.fromCtrlBlock.flush := ctrlBlock.io.toIssueBlock.flush
  vfScheduler.io.fromDispatch.allocPregs <> ctrlBlock.io.toIssueBlock.allocPregs
  vfScheduler.io.fromDispatch.uops <> ctrlBlock.io.toIssueBlock.vfUops
  vfScheduler.io.intWriteBack := 0.U.asTypeOf(vfScheduler.io.intWriteBack)
  vfScheduler.io.vfWriteBack := wbDataPath.io.toVfPreg

  dataPath.io.flush := ctrlBlock.io.toDataPath.flush
  dataPath.io.vconfigReadPort.addr := ctrlBlock.io.toDataPath.vtypeAddr
  val vconfig = dataPath.io.vconfigReadPort.data
  ctrlBlock.io.fromDataPath.vtype := vconfig(7, 0).asTypeOf(new VType)
  for (i <- 0 until dataPath.io.fromIntIQ.length) {
    for (j <- 0 until dataPath.io.fromIntIQ(i).length) {
      PipelineConnect(intScheduler.io.toDataPath(i)(j), dataPath.io.fromIntIQ(i)(j), dataPath.io.fromIntIQ(i)(j).valid,
        intScheduler.io.toDataPath(i)(j).bits.common.robIdx.needFlush(ctrlBlock.io.toDataPath.flush))
      intScheduler.io.fromDataPath(i)(j) := dataPath.io.toIntIQ(i)(j)
    }
  }

  dataPath.io.fromVfIQ <> vfScheduler.io.toDataPath
  vfScheduler.io.fromDataPath := dataPath.io.toVfIQ
  dataPath.io.fromMemIQ <> memScheduler.io.toDataPath
  memScheduler.io.fromDataPath := dataPath.io.toMemIQ

  println(s"[Backend] wbDataPath.io.toIntPreg: ${wbDataPath.io.toIntPreg.size}, dataPath.io.fromIntWb: ${dataPath.io.fromIntWb.size}")
  println(s"[Backend] wbDataPath.io.toVfPreg: ${wbDataPath.io.toVfPreg.size}, dataPath.io.fromFpWb: ${dataPath.io.fromVfWb.size}")
  dataPath.io.fromIntWb := wbDataPath.io.toIntPreg
  dataPath.io.fromVfWb := wbDataPath.io.toVfPreg
  dataPath.io.debugIntRat := ctrlBlock.io.debug_int_rat
  dataPath.io.debugFpRat := ctrlBlock.io.debug_fp_rat
  dataPath.io.debugVecRat := ctrlBlock.io.debug_vec_rat
  dataPath.io.debugVconfigRat := ctrlBlock.io.debug_vconfig_rat

  intExuBlock.io.flush := ctrlBlock.io.toExuBlock.flush
  for (i <- 0 until intExuBlock.io.in.length) {
    for (j <- 0 until intExuBlock.io.in(i).length) {
      PipelineConnect(dataPath.io.toIntExu(i)(j), intExuBlock.io.in(i)(j), intExuBlock.io.in(i)(j).fire,
        Mux(dataPath.io.toIntExu(i)(j).fire,
          dataPath.io.toIntExu(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush),
          intExuBlock.io.in(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush)))
    }
  }

  private val csrio = intExuBlock.io.csrio.get
  csrio.hartId := io.fromTop.hartId
  csrio.perf.retiredInstr <> ctrlBlock.io.robio.csr.perfinfo.retiredInstr
  csrio.perf.ctrlInfo <> ctrlBlock.io.perfInfo.ctrlInfo
  csrio.perf.perfEventsCtrl <> ctrlBlock.getPerf
  csrio.fpu.fflags := ctrlBlock.io.robio.csr.fflags
  csrio.fpu.isIllegal := false.B // Todo: remove it
  csrio.fpu.dirty_fs := ctrlBlock.io.robio.csr.dirty_fs
  csrio.vpu <> 0.U.asTypeOf(csrio.vpu) // Todo

  val debugVconfig = dataPath.io.debugVconfig.asTypeOf(new VConfig)
  val debugVtype = VType.toVtypeStruct(debugVconfig.vtype).asUInt
  val debugVl = debugVconfig.vl
  csrio.vpu.set_vxsat := ctrlBlock.io.robio.csr.vxsat
  csrio.vpu.set_vstart.valid := ctrlBlock.io.robio.csr.vcsrFlag
  csrio.vpu.set_vstart.bits := 0.U
  csrio.vpu.set_vtype.valid := ctrlBlock.io.robio.csr.vcsrFlag
  csrio.vpu.set_vtype.bits := ZeroExt(debugVtype, XLEN)
  csrio.vpu.set_vl.valid := ctrlBlock.io.robio.csr.vcsrFlag
  csrio.vpu.set_vl.bits := ZeroExt(debugVl, XLEN)
  csrio.exception := ctrlBlock.io.robio.exception
  csrio.memExceptionVAddr := io.mem.exceptionVAddr
  csrio.externalInterrupt := io.fromTop.externalInterrupt
  csrio.distributedUpdate(0) := io.mem.csrDistributedUpdate
  csrio.distributedUpdate(1) := io.frontendCsrDistributedUpdate
  csrio.perf <> io.perf
  private val fenceio = intExuBlock.io.fenceio.get
  fenceio.disableSfence := csrio.disableSfence
  io.fenceio <> fenceio

  vfExuBlock.io.flush := ctrlBlock.io.toExuBlock.flush
  for (i <- 0 until vfExuBlock.io.in.size) {
    for (j <- 0 until vfExuBlock.io.in(i).size) {
      PipelineConnect(dataPath.io.toFpExu(i)(j), vfExuBlock.io.in(i)(j), vfExuBlock.io.in(i)(j).fire,
        Mux(dataPath.io.toFpExu(i)(j).fire,
          dataPath.io.toFpExu(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush),
          vfExuBlock.io.in(i)(j).bits.robIdx.needFlush(ctrlBlock.io.toExuBlock.flush)))
    }
  }
  vfExuBlock.io.frm.foreach(_ := csrio.fpu.frm)

  wbDataPath.io.flush := ctrlBlock.io.redirect
  wbDataPath.io.fromTop.hartId := io.fromTop.hartId
  wbDataPath.io.fromIntExu <> intExuBlock.io.out
  wbDataPath.io.fromVfExu <> vfExuBlock.io.out
  wbDataPath.io.fromMemExu.flatten.zip(io.mem.writeBack).foreach { case (sink, source) =>
    sink.valid := source.valid
    source.ready := sink.ready
    sink.bits.data   := source.bits.data
    sink.bits.pdest  := source.bits.uop.pdest
    sink.bits.robIdx := source.bits.uop.robIdx
    sink.bits.intWen.foreach(_ := source.bits.uop.rfWen)
    sink.bits.fpWen.foreach(_ := source.bits.uop.fpWen)
    sink.bits.vecWen.foreach(_ := source.bits.uop.vecWen)
    sink.bits.exceptionVec.foreach(_ := source.bits.uop.exceptionVec)
    sink.bits.flushPipe.foreach(_ := source.bits.uop.flushPipe)
    sink.bits.replay.foreach(_ := source.bits.uop.replayInst)
    sink.bits.debug := source.bits.debug
    sink.bits.debugInfo := 0.U.asTypeOf(sink.bits.debugInfo)
    sink.bits.lqIdx.foreach(_ := source.bits.uop.lqIdx)
    sink.bits.sqIdx.foreach(_ := source.bits.uop.sqIdx)
    sink.bits.ftqIdx.foreach(_ := source.bits.uop.ftqPtr)
    sink.bits.ftqOffset.foreach(_ := source.bits.uop.ftqOffset)
  }

  // to mem
  io.mem.redirect := ctrlBlock.io.redirect
  io.mem.issueUops.zip(dataPath.io.toMemExu.flatten).foreach { case (sink, source) =>
    sink.valid := source.valid
    source.ready := sink.ready
    sink.bits.iqIdx         := source.bits.iqIdx
    sink.bits.isFirstIssue  := source.bits.isFirstIssue
    sink.bits.uop           := 0.U.asTypeOf(sink.bits.uop)
    sink.bits.src           := 0.U.asTypeOf(sink.bits.src)
    sink.bits.src.zip(source.bits.src).foreach { case (l, r) => l := r}
    sink.bits.uop.fuType    := source.bits.fuType
    sink.bits.uop.fuOpType  := source.bits.fuOpType
    sink.bits.uop.imm       := source.bits.imm
    sink.bits.uop.robIdx    := source.bits.robIdx
    sink.bits.uop.pdest     := source.bits.pdest
    sink.bits.uop.rfWen     := source.bits.rfWen.getOrElse(false.B)
    sink.bits.uop.fpWen     := source.bits.fpWen.getOrElse(false.B)
    sink.bits.uop.vecWen    := source.bits.vecWen.getOrElse(false.B)
    sink.bits.uop.flushPipe := source.bits.flushPipe.getOrElse(false.B)
    sink.bits.uop.pc        := source.bits.pc.getOrElse(0.U)
    sink.bits.uop.lqIdx     := source.bits.lqIdx.getOrElse(0.U.asTypeOf(new LqPtr))
    sink.bits.uop.sqIdx     := source.bits.sqIdx.getOrElse(0.U.asTypeOf(new SqPtr))
    sink.bits.uop.ftqPtr    := source.bits.ftqIdx.getOrElse(0.U.asTypeOf(new FtqPtr))
    sink.bits.uop.ftqOffset := source.bits.ftqOffset.getOrElse(0.U)
  }
  io.mem.loadFastMatch := memScheduler.io.toMem.get.loadFastMatch.map(_.fastMatch)
  io.mem.loadFastImm := memScheduler.io.toMem.get.loadFastMatch.map(_.fastImm)
  io.mem.tlbCsr := csrio.tlb
  io.mem.csrCtrl := csrio.customCtrl
  io.mem.sfence := fenceio.sfence
  io.mem.isStoreException := CommitType.lsInstIsStore(ctrlBlock.io.robio.exception.bits.commitType)
  require(io.mem.loadPcRead.size == params.LduCnt)
  io.mem.loadPcRead.zipWithIndex.foreach { case (loadPcRead, i) =>
    loadPcRead.data := ctrlBlock.io.memLdPcRead(i).data
    ctrlBlock.io.memLdPcRead(i).ptr := loadPcRead.ptr
    ctrlBlock.io.memLdPcRead(i).offset := loadPcRead.offset
  }
  // mem io
  io.mem.lsqEnqIO <> memScheduler.io.memIO.get.lsqEnqIO
  io.mem.robLsqIO <> ctrlBlock.io.robio.lsq
  io.mem.toSbuffer <> fenceio.sbuffer

  io.frontendSfence := fenceio.sfence
  io.frontendTlbCsr := csrio.tlb
  io.frontendCsrCtrl := csrio.customCtrl

  io.tlb <> csrio.tlb

  io.csrCustomCtrl := csrio.customCtrl

  dontTouch(memScheduler.io)
  dontTouch(io.mem)
  dontTouch(dataPath.io.toMemExu)
  dontTouch(wbDataPath.io.fromMemExu)
}

class BackendMemIO(implicit p: Parameters, params: BackendParams) extends XSBundle {
  // params alias
  private val LoadQueueSize = VirtualLoadQueueSize
  // In/Out // Todo: split it into one-direction bundle
  val lsqEnqIO = Flipped(new LsqEnqIO)
  val robLsqIO = new RobLsqIO
  val toSbuffer = new FenceToSbuffer
  val ldaIqFeedback = Vec(params.LduCnt, Flipped(new MemRSFeedbackIO))
  val staIqFeedback = Vec(params.StaCnt, Flipped(new MemRSFeedbackIO))
  val loadPcRead = Vec(params.LduCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))

  // Input
  val writeBack = MixedVec(Seq.fill(params.LduCnt + params.StaCnt * 2)(Flipped(DecoupledIO(new MemExuOutput()))) ++ Seq.fill(params.VlduCnt)(Flipped(DecoupledIO(new MemExuOutput(true)))))

  val s3_delayed_load_error = Input(Vec(LoadPipelineWidth, Bool()))
  val stIn = Input(Vec(params.StaCnt, ValidIO(new DynInst())))
  val memoryViolation = Flipped(ValidIO(new Redirect))
  val exceptionVAddr = Input(UInt(VAddrBits.W))
  val sqDeq = Input(UInt(log2Ceil(EnsbufferWidth + 1).W))
  val lqDeq = Input(UInt(log2Up(CommitWidth + 1).W))

  val lqCancelCnt = Input(UInt(log2Up(VirtualLoadQueueSize + 1).W))
  val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))

  val otherFastWakeup = Flipped(Vec(params.LduCnt + 2 * params.StaCnt, ValidIO(new DynInst)))
  val stIssuePtr = Input(new SqPtr())

  val csrDistributedUpdate = Flipped(new DistributedCSRUpdateReq)

  // Output
  val redirect = ValidIO(new Redirect)   // rob flush MemBlock
  val issueUops = MixedVec(Seq.fill(params.LduCnt + params.StaCnt * 2)(DecoupledIO(new MemExuInput())) ++ Seq.fill(params.VlduCnt)(DecoupledIO(new MemExuInput(true))))
  val loadFastMatch = Vec(params.LduCnt, Output(UInt(params.LduCnt.W)))
  val loadFastImm   = Vec(params.LduCnt, Output(UInt(12.W))) // Imm_I

  val tlbCsr = Output(new TlbCsrBundle)
  val csrCtrl = Output(new CustomCSRCtrlIO)
  val sfence = Output(new SfenceBundle)
  val isStoreException = Output(Bool())
}

class BackendIO(implicit p: Parameters, params: BackendParams) extends XSBundle {
  val fromTop = new Bundle {
    val hartId = Input(UInt(8.W))
    val externalInterrupt = new ExternalInterruptIO
  }

  val toTop = new Bundle {
    val cpuHalted = Output(Bool())
  }

  val fenceio = new FenceIO
  // Todo: merge these bundles into BackendFrontendIO
  val frontend = Flipped(new FrontendToCtrlIO)
  val frontendSfence = Output(new SfenceBundle)
  val frontendCsrCtrl = Output(new CustomCSRCtrlIO)
  val frontendTlbCsr = Output(new TlbCsrBundle)
  // distributed csr write
  val frontendCsrDistributedUpdate = Flipped(new DistributedCSRUpdateReq)

  val mem = new BackendMemIO

  val perf = Input(new PerfCounterIO)

  val tlb = Output(new TlbCsrBundle)

  val csrCustomCtrl = Output(new CustomCSRCtrlIO)
}
