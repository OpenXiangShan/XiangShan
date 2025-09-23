package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.HasPerfEvents
import utils.OptionWrapper
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.{BackendMemIO, ExcpModToVprf, PcToDataPathIO, VprfToExcpMod}
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.fu.{CSRFileIO, FenceIO, FuType}
import xiangshan.backend.regfile.{RfWritePortBundle, RfWritePortWithConfig, VlPregParams}
import xiangshan.backend.datapath.WbConfig.V0WB
import xiangshan.backend.exu.ExuBlock
import xiangshan.backend.regcache.RegCacheTagTable
import xiangshan.mem.{LqPtr, LsqEnqCtrl, LsqEnqIO, SqPtr}
import xiangshan.mem.Bundles.MemWaitUpdateReqBundle
import utility._
import xiangshan.backend.fu.vector.Bundles.{VType, Vstart}
import xiangshan.backend.fu.wrapper.{CSRInput, CSRToDecode}
import xiangshan.backend.issue.EntryBundles.RespType


class Region(val params: SchdBlockParams)(implicit p: Parameters) extends XSModule with HasCriticalErrors {
  val io = IO(new RegionIO(params))
  val issueQueues = params.issueBlockParams.map { case iqParam => {
    (if (iqParam.inFpSchd) Module(new IssueQueueFpImp()(p,iqParam))
    else if (iqParam.inIntSchd && !iqParam.isMemAddrIQ) Module(new IssueQueueIntImp()(p,iqParam))
    else if (iqParam.inIntSchd && iqParam.isMemAddrIQ) Module(new IssueQueueMemAddrImp()(p,iqParam))
    else if (iqParam.inVfSchd && !iqParam.isMemAddrIQ) Module(new IssueQueueVfImp()(p,iqParam))
    else Module(new IssueQueueVecMemImp()(p,iqParam))).suggestName("issueQueue" + iqParam.allExuParams.map(_.name).reduce(_ + _) + "_" + iqParam.getIQFuName)
    }
  }
  issueQueues.map(x =>{
    println(s"[Region] iqParam.getIQName = ${x.param.getIQName}")
    println(s"[Region] Class name: ${x.getClass.getSimpleName}")
    println(s"[Region] iqParam.inIntSchd: ${x.param.inIntSchd}")
    println(s"[Region] iqParam.isMemAddrIQ: ${x.param.isMemAddrIQ}")
  }
  )
  println(s"[Region] ${params.schdType}")
  val prefix = if (params.isIntSchd) "int" else if (params.isFpSchd) "fp" else "vec"
  val dataPath = Module(new DataPath()(p, backendParams, params)).suggestName(prefix + "DataPath")
  val bypassNetwork = Module(new BypassNetwork()(p, backendParams)).suggestName(prefix + "BypassNetwork")
  val exuBlock = Module(new ExuBlock()(p, params)).suggestName(prefix + "ExuBlock")
  val wbDataPath = Module(new WbDataPath(backendParams, params)).suggestName(prefix + "WBDataPath")
  val og2ForVector = Option.when(params.isVecSchd)(Module(new Og2ForVector(backendParams)).suggestName(prefix + "OG2ForVector"))
  val wbFuBusyTable = Module(new WbFuBusyTable()(p, backendParams)).suggestName(prefix + "WBFuBusyTable")

  wbFuBusyTable.io.in.intSchdBusyTable := io.intSchdBusyTable
  wbFuBusyTable.io.in.fpSchdBusyTable := io.fpSchdBusyTable
  wbFuBusyTable.io.in.vfSchdBusyTable := io.vfSchdBusyTable
  val og0Cancel = dataPath.io.og0Cancel
  val og1Cancel = dataPath.io.og1Cancel
  val allWakeup = if (params.isIntSchd) issueQueues.flatMap(_.io.wakeupToIQ) ++ io.wakeUpFromFp.get
                  else if (params.isFpSchd) issueQueues.flatMap(_.io.wakeupToIQ) ++ io.wakeUpFromInt.get.filter(_.bits.params.hasLoadExu)
                  else issueQueues.flatMap(_.io.wakeupToIQ)
  val iqWakeUpBundle: Map[Int, ValidIO[IssueQueueIQWakeUpBundle]] = allWakeup.map(x => (x.bits.exuIdx, x)).toMap
  val iqWakeUpBundleDelayed: Map[Int, ValidIO[IssueQueueIQWakeUpBundle]] =
    allWakeup
    .map{ case x =>
      val delayed = Wire(chiselTypeOf(x))
      // TODO: add clock gate use Wen, remove issuequeue wakeupToIQ logic Wen = Wen && valid
      delayed := RegNext(x)
      (x.bits.exuIdx, delayed)
    }.toMap
  // Connect bundles having the same wakeup source
  issueQueues.zipWithIndex.foreach { case (iq, i) =>
    println(s"[Region_${params.schdType}] iq.param.getIQName = ${iq.param.getIQName}")
    println(s"[Region_${params.schdType}] iq.io.wakeupFromIQ.size = ${iq.io.wakeupFromIQ.size}")
    if (params.isIntSchd) println(s"[Region_${params.schdType}] io.wakeUpFromFp.size = ${io.wakeUpFromFp.get.size}")
    if (params.isFpSchd) println(s"[Region_${params.schdType}] io.wakeUpFromInt.filter(_.bits.params.hasLoadExu).size = ${io.wakeUpFromInt.get.filter(_.bits.params.hasLoadExu).size}")
    println(s"[Region_${params.schdType}] iqWakeUpBundle = ${iqWakeUpBundle.keys.toList.sorted}")
    iq.io.wakeupFromIQ.foreach { wakeUp =>
      val exuIdx = wakeUp.bits.exuIdx
      println(s"[Region_${params.schdType}] Connect wakeup exuName = ${backendParams.allExuParams(exuIdx).name}, exuIdx = ${exuIdx}")
      val wakeUpIn = iqWakeUpBundle(wakeUp.bits.exuIdx)
      wakeUp.valid := wakeUpIn.valid
      connectSamePort(wakeUp.bits, wakeUpIn.bits)
      backendParams.connectWakeup(exuIdx)
      if (backendParams.isCopyPdest(exuIdx)) {
        println(s"[Region_${params.schdType}] exuIdx ${exuIdx} use pdestCopy ${backendParams.getCopyPdestIndex(exuIdx)}")
        wakeUp.bits.pdest := wakeUpIn.bits.pdestCopy.get(backendParams.getCopyPdestIndex(exuIdx))
        if (wakeUpIn.bits.rfWenCopy.nonEmpty) wakeUp.bits.rfWen := wakeUpIn.bits.rfWenCopy.get(backendParams.getCopyPdestIndex(exuIdx))
        if (wakeUpIn.bits.fpWenCopy.nonEmpty) wakeUp.bits.fpWen := wakeUpIn.bits.fpWenCopy.get(backendParams.getCopyPdestIndex(exuIdx))
        if (wakeUpIn.bits.vecWenCopy.nonEmpty) wakeUp.bits.vecWen := wakeUpIn.bits.vecWenCopy.get(backendParams.getCopyPdestIndex(exuIdx))
        if (wakeUpIn.bits.v0WenCopy.nonEmpty) wakeUp.bits.v0Wen := wakeUpIn.bits.v0WenCopy.get(backendParams.getCopyPdestIndex(exuIdx))
        if (wakeUpIn.bits.vlWenCopy.nonEmpty) wakeUp.bits.vlWen := wakeUpIn.bits.vlWenCopy.get(backendParams.getCopyPdestIndex(exuIdx))
        if (wakeUpIn.bits.loadDependencyCopy.nonEmpty) wakeUp.bits.loadDependency := wakeUpIn.bits.loadDependencyCopy.get(backendParams.getCopyPdestIndex(exuIdx))
      }
      if (iq.param.numIntSrc == 0) wakeUp.bits.rfWen := false.B
      if (iq.param.numFpSrc == 0) wakeUp.bits.fpWen := false.B
      if (iq.param.numVfSrc == 0) wakeUp.bits.vecWen := false.B
      if (iq.param.numV0Src == 0) wakeUp.bits.v0Wen := false.B
      if (iq.param.numVlSrc == 0) wakeUp.bits.vlWen := false.B
    }
    iq.io.wakeupFromIQDelayed.foreach { wakeUp =>
      val wakeUpIn = iqWakeUpBundleDelayed(wakeUp.bits.exuIdx)
      wakeUp.valid := wakeUpIn.valid
      connectSamePort(wakeUp.bits, wakeUpIn.bits)
      if (iq.param.numIntSrc == 0) wakeUp.bits.rfWen := false.B
      if (iq.param.numFpSrc == 0) wakeUp.bits.fpWen := false.B
      if (iq.param.numVfSrc == 0) wakeUp.bits.vecWen := false.B
      if (iq.param.numV0Src == 0) wakeUp.bits.v0Wen := false.B
      if (iq.param.numVlSrc == 0) wakeUp.bits.vlWen := false.B
    }
    if (iq.param.needLoadDependency)
      iq.io.ldCancel := io.ldCancel
    else
      iq.io.ldCancel := 0.U.asTypeOf(io.ldCancel)
  }
  issueQueues.filter(_.param.StdCnt == 0).zip(io.fromDispatch).map{ case (sinks, sources) => {
    sinks.io.enq.zip(sources).map{ case (sink, source) => {
      sink.valid := source.valid
      // TODO add option connect method
      connectSamePort(sink.bits, source.bits)
      sinks.io.vlFromIntIsZero := false.B
      sinks.io.vlFromIntIsVlmax := false.B
      sinks.io.vlFromVfIsZero := false.B
      sinks.io.vlFromVfIsVlmax := false.B
      source.ready := sink.ready
    }}
  }}
  issueQueues.filterNot(_.param.StdCnt == 0).map { case stdiq => {
      stdiq.io.vlFromIntIsZero := false.B
      stdiq.io.vlFromIntIsVlmax := false.B
      stdiq.io.vlFromVfIsZero := false.B
      stdiq.io.vlFromVfIsVlmax := false.B
    }
  }
  issueQueues.filter(_.param.needUncertainWakeupFromExu).zip(exuBlock.io.uncertainWakeupOut.get).map { case (iq, exuWakeUpIn) =>
    iq.io.wakeupFromExu.get.map(x => x.valid := false.B)
    iq.io.wakeupFromExu.get.map(x => x.bits := 0.U.asTypeOf(x.bits))
    iq.io.wakeupFromExu.get.head <> exuWakeUpIn
  }
  val iqWakeUpOutMap: Map[Int, ValidIO[IssueQueueIQWakeUpBundle]] =
    issueQueues.flatMap(_.io.wakeupToIQ)
      .map(x => (x.bits.exuIdx, x))
      .toMap
  io.wakeUpToDispatch.foreach { wakeUp =>
    wakeUp := iqWakeUpOutMap(wakeUp.bits.exuIdx)
  }


  // Response
  val fromDataPathResp = if (params.isIntSchd) dataPath.io.toIntIQ
                         else if (params.isFpSchd) dataPath.io.toFpIQ
                         else dataPath.io.toVfIQ
  issueQueues.zipWithIndex.foreach { case (iq, i) =>
    iq.io.flush := io.flush
    iq.io.og0Cancel := og0Cancel
    iq.io.og1Cancel := og1Cancel
    iq.io.og0Resp.zipWithIndex.foreach { case (og0Resp, j) =>
      og0Resp := fromDataPathResp(i)(j).og0resp
    }
    iq.io.og1Resp.zipWithIndex.foreach { case (og1Resp, j) =>
      og1Resp := fromDataPathResp(i)(j).og1resp
    }
    iq.io.replaceRCIdx.foreach(x => x := 0.U.asTypeOf(x))
    iq.io.wakeupFromWB.foreach(x => x := 0.U.asTypeOf(x))
    iq.io.wakeupFromWBDelayed.foreach(x => x := 0.U.asTypeOf(x))
  }
  // 1 iq has 1 ldu, 1sta, 1vstu
  val ldAddrIQs = issueQueues.filter(iq => iq.param.LduCnt > 0)
  ldAddrIQs.zipWithIndex.foreach {
    case (imp: IssueQueueMemAddrImp, i) =>
      imp.io.memIO.get.feedbackIO := 0.U.asTypeOf(imp.io.memIO.get.feedbackIO)
      imp.io.memIO.get.checkWait := 0.U.asTypeOf(imp.io.memIO.get.checkWait)
      imp.io.memIO.get.loadWakeUp.head := io.wakeupFromLDU.get(i)
    case _ =>
  }
  val stAddrIQs = issueQueues.filter(iq => iq.param.StaCnt > 0)
  stAddrIQs.zipWithIndex.foreach {
    case (imp: IssueQueueMemAddrImp, i) =>
      imp.io.memIO.get.feedbackIO.head := io.staFeedback.get(i)
      imp.io.memIO.get.checkWait := 0.U.asTypeOf(imp.io.memIO.get.checkWait)
    case _ =>
  }
  val vecMemIQs = issueQueues.filter(iq => iq.param.VlduCnt > 0)
  vecMemIQs.zipWithIndex.foreach {
    case (imp: IssueQueueVecMemImp, i) =>
      imp.io.memIO.get.feedbackIO.head := io.vstuFeedback.get(i)
      imp.io.memIO.get.checkWait := 0.U.asTypeOf(imp.io.memIO.get.checkWait)
      imp.io.memIO.get.lqDeqPtr.get := io.lqDeqPtr.get
      imp.io.memIO.get.sqDeqPtr.get := io.sqDeqPtr.get
    case _ =>
  }
  // other wakeup, int vec need WB wakeup
  def connectWakeupWB(sink: ValidIO[IssueQueueWBWakeUpBundle], source: RfWritePortWithConfig): Unit = {
    sink.valid := source.wen
    sink.bits.rfWen := source.intWen
    sink.bits.fpWen := source.fpWen
    sink.bits.vecWen := source.vecWen
    sink.bits.v0Wen := source.v0Wen
    sink.bits.vlWen := source.vlWen
    sink.bits.pdest := source.addr
  }

  def connectWakeupWBDelay(sink: ValidIO[IssueQueueWBWakeUpBundle], source: ValidIO[RfWritePortBundle]): Unit = {
    sink.valid := source.valid
    sink.bits.rfWen := source.bits.rfWen
    sink.bits.fpWen := source.bits.fpWen
    sink.bits.vecWen := source.bits.vecWen
    sink.bits.v0Wen := source.bits.v0Wen
    sink.bits.vlWen := source.bits.vlWen
    sink.bits.pdest := source.bits.pdest
  }

  if (params.isIntSchd) {
    // exu can write to only one register file port
    val idxes = backendParams.vecSchdParams.get.exuBlockParams.filter(_.writeIntRf).map(_.wbPortConfigs.filter(_.isInstanceOf[IntWB]).head.port)
    println(s"[Region] vec write int port = ${idxes}")
    val wakeupFromWB = MixedVecInit(idxes.map(x => io.fromIntWb(x)))
    val wakeupFromWBDelayed = Reg(MixedVec(Vec(wakeupFromWB.size,
      Valid(new RfWritePortBundle(backendParams.intPregParams.dataCfg, backendParams.intPregParams.addrWidth)))))
    wakeupFromWBDelayed.zip(wakeupFromWB).map{case (sink, source) =>
      sink.valid := source.wen
      sink.bits.rfWen := source.intWen
      sink.bits.fpWen := source.fpWen
      sink.bits.vecWen := source.vecWen
      sink.bits.v0Wen := source.v0Wen
      sink.bits.vlWen := source.vlWen
      sink.bits.pdest := source.addr
    }
    issueQueues.map { case iq =>
      val vecExuIndices = params.backendParam.allExuParams.filter(x => x.isVfExeUnit || x.isMemExeUnit && x.needVecWen).map(_.exuIdx)
      println(s"[Region_int] vecExuIndices = ${vecExuIndices}")
      val vecWBIndices = iq.io.wakeupFromWB.zipWithIndex.filter(x => x._1.bits.exuIndices.intersect(vecExuIndices).nonEmpty).map(_._2)
      println(s"[Region_int] vecWBIndices = ${vecWBIndices}")
      vecWBIndices.zip(wakeupFromWB).zip(wakeupFromWBDelayed).map { case ((i, source1), source2) =>
        connectWakeupWB(iq.io.wakeupFromWB(i), source1)
        connectWakeupWBDelay(iq.io.wakeupFromWBDelayed(i), source2)
      }
      iq.io.wakeupFromF2I.foreach(_ := io.wakeupFromF2I.get)
      println(s"[Region_int] wakeupFromWB.size = ${wakeupFromWB.size}")
      println(s"[Region_int] iq.io.wakeupFromWB.size = ${iq.io.wakeupFromWB.size}")
      println(s"[Region_int] ${iq.param.getIQName}: iq.param.needWakeupFromIntWBPort = ${iq.param.needWakeupFromIntWBPort.map(x => (x._1, x._2.map(_.name)))}")
    }
  }
  else if (params.isFpSchd) {
    val idxes = backendParams.vecSchdParams.get.exuBlockParams.filter(_.writeFpRf).map(_.wbPortConfigs.filter(_.isInstanceOf[FpWB]).head.port)
    println(s"[Region] vec write fp port = ${idxes}")
    val wakeupFromWB = MixedVecInit(idxes.map(x => io.fromFpWb(x)))
    val wakeupFromWBDelayed = Reg(MixedVec(Vec(wakeupFromWB.size,
      Valid(new RfWritePortBundle(backendParams.fpPregParams.dataCfg, backendParams.fpPregParams.addrWidth)))))
    wakeupFromWBDelayed.zip(wakeupFromWB).map { case (sink, source) =>
      sink.valid := source.wen
      sink.bits.rfWen := source.intWen
      sink.bits.fpWen := source.fpWen
      sink.bits.vecWen := source.vecWen
      sink.bits.v0Wen := source.v0Wen
      sink.bits.vlWen := source.vlWen
      sink.bits.pdest := source.addr
    }
    issueQueues.map { case iq =>
      val vecExuIndices = params.backendParam.allExuParams.filter(x => x.isVfExeUnit || x.isMemExeUnit && x.needVecWen).map(_.exuIdx)
      println(s"[Region_fp] vecExuIndices = ${vecExuIndices}")
      val vecWBIndices = iq.io.wakeupFromWB.zipWithIndex.filter(x => x._1.bits.exuIndices.intersect(vecExuIndices).nonEmpty).map(_._2)
      println(s"[Region_fp] vecWBIndices = ${vecWBIndices}")
      vecWBIndices.zip(wakeupFromWB).zip(wakeupFromWBDelayed).map { case ((i, source1), source2) =>
        connectWakeupWB(iq.io.wakeupFromWB(i), source1)
        connectWakeupWBDelay(iq.io.wakeupFromWBDelayed(i), source2)
      }
      iq.io.wakeupFromI2F.foreach(_ := io.wakeupFromI2F.get)
    }
  }
  else if (params.isVecSchd) {
    val wakeupFromWB = io.fromVfWb ++ io.fromV0Wb ++ io.fromVlWb
    val wakeupFromWBDelayedVf = Reg((Vec(io.fromVfWb.size,
      Valid(new RfWritePortBundle(backendParams.vfPregParams.dataCfg, backendParams.vfPregParams.addrWidth)))))
    val wakeupFromWBDelayedV0 = Reg((Vec(io.fromV0Wb.size,
      Valid(new RfWritePortBundle(backendParams.v0PregParams.dataCfg, backendParams.v0PregParams.addrWidth)))))
    val wakeupFromWBDelayedVl = Reg((Vec(io.fromVlWb.size,
      Valid(new RfWritePortBundle(backendParams.vlPregParams.dataCfg, backendParams.vlPregParams.addrWidth)))))
    wakeupFromWBDelayedVf.zip(io.fromVfWb).map { case (sink, source) =>
      sink.valid := source.wen
      sink.bits.rfWen := source.intWen
      sink.bits.fpWen := source.fpWen
      sink.bits.vecWen := source.vecWen
      sink.bits.v0Wen := source.v0Wen
      sink.bits.vlWen := source.vlWen
      sink.bits.pdest := source.addr
    }
    wakeupFromWBDelayedV0.zip(io.fromV0Wb).map { case (sink, source) =>
      sink.valid := source.wen
      sink.bits.rfWen := source.intWen
      sink.bits.fpWen := source.fpWen
      sink.bits.vecWen := source.vecWen
      sink.bits.v0Wen := source.v0Wen
      sink.bits.vlWen := source.vlWen
      sink.bits.pdest := source.addr
    }
    wakeupFromWBDelayedVl.zip(io.fromVlWb).map { case (sink, source) =>
      sink.valid := source.wen
      sink.bits.rfWen := source.intWen
      sink.bits.fpWen := source.fpWen
      sink.bits.vecWen := source.vecWen
      sink.bits.v0Wen := source.v0Wen
      sink.bits.vlWen := source.vlWen
      sink.bits.pdest := source.addr
    }
    issueQueues.map { case iq =>
      println(s"[Region_vec] wakeupFromWB.size = ${wakeupFromWB.size}")
      println(s"[Region_vec] iq.io.wakeupFromWB.size = ${iq.io.wakeupFromWB.size}")
      println(s"[Region_vec] ${iq.param.getIQName}: iq.param.needWakeupFromVfWBPort = ${iq.param.needWakeupFromVfWBPort.map(x => (x._1, x._2.map(_.name)))}")
      iq.io.wakeupFromWB.zip(wakeupFromWB).map(x => connectWakeupWB(x._1, x._2))
      iq.io.wakeupFromWBDelayed.zip(wakeupFromWBDelayedVf ++ wakeupFromWBDelayedV0 ++ wakeupFromWBDelayedVl).map(x => connectWakeupWBDelay(x._1, x._2))
    }
  }
  // std dispatch
  val stDataIQs = issueQueues.filter(iq => iq.param.StdCnt > 0)
  val staEnqs = stAddrIQs.map(_.io.enq).flatten
  val stdEnqs = stDataIQs.map(_.io.enq).flatten.take(staEnqs.size)
  val noStdExuParams = params.issueBlockParams.map(x => Seq.fill(x.numEnq)(x.exuBlockParams)).flatten.filter { x => x.map(!_.hasStdFu).reduce(_ && _) }
  val staIdx = io.fromDispatch.zipWithIndex.filter(x => x._1.head.bits.params.isStAddrIQ).map(_._2)
  val staReady = stAddrIQs.map(_.io.enq.head.ready)
  val stdReady = stDataIQs.map(_.io.enq.head.ready)
  staIdx.zipWithIndex.map { case (sta, i) => {
    io.fromDispatch(sta).map(_.ready := staReady(i) && stdReady(i))
  }
  }
  val staFromDispatch = staIdx.map(x => io.fromDispatch(x)).flatten
  staEnqs.zip(staFromDispatch).map { case (sink, source) =>
    sink.valid := source.valid && !source.bits.isDropAmocasSta
  }
  (stdEnqs).zip(staFromDispatch).zipWithIndex.foreach { case ((stdIQEnq, staIQEnq), i) =>
    stdIQEnq.valid := staIQEnq.valid
    connectSamePort(stdIQEnq.bits, staIQEnq.bits)
    // Store data reuses store addr src(1) in dispatch2iq
    // [dispatch2iq] --src*------src*(0)--> [staIQ|hyaIQ]
    //                       \
    //                        ---src*(1)--> [stdIQ]
    // Since the src(1) of sta is easier to get, stdIQEnq.bits.src*(0) is assigned to staIQEnq.bits.src*(1)
    // instead of dispatch2Iq.io.out(x).bits.src*(1)
    val stdIdx = 1
    stdIQEnq.bits.srcState(0) := staIQEnq.bits.srcState(stdIdx)
    stdIQEnq.bits.srcLoadDependency(0) := staIQEnq.bits.srcLoadDependency(stdIdx)
    stdIQEnq.bits.srcType(0) := staIQEnq.bits.srcType(stdIdx)
    stdIQEnq.bits.psrc(0) := staIQEnq.bits.psrc(stdIdx)
    stdIQEnq.bits.sqIdx := staIQEnq.bits.sqIdx
    stdIQEnq.bits.useRegCache(0) := staIQEnq.bits.useRegCache(stdIdx)
    stdIQEnq.bits.regCacheIdx(0) := staIQEnq.bits.regCacheIdx(stdIdx)
  }
  // Connect each replace RCIdx to IQ
  if (params.needWriteRegCache) {
    val iqReplaceRCIdxVec = issueQueues.filter(_.param.needWriteRegCache).flatMap { iq =>
      iq.param.allExuParams.zip(iq.io.replaceRCIdx.get).filter(_._1.needWriteRegCache).map(_._2)
    }
    iqReplaceRCIdxVec.zip(dataPath.io.toWakeupQueueRCIdx).foreach { case (iq, in) =>
      iq := in
    }
    println(s"[Region] numWriteRegCache: ${params.numWriteRegCache}")
    println(s"[Region] iqReplaceRCIdxVec: ${iqReplaceRCIdxVec.size}")
  }
  dataPath.io.hartId := io.hartId
  dataPath.io.flush := io.flush
  dataPath.io.fromIntIQ.flatten.map(x => {
    x.valid := false.B
    x.bits := 0.U.asTypeOf(x.bits)
  })
  dataPath.io.fromFpIQ.flatten.map(x => {
    x.valid := false.B
    x.bits := 0.U.asTypeOf(x.bits)
  })
  dataPath.io.fromVfIQ.flatten.map(x => {
    x.valid := false.B
    x.bits := 0.U.asTypeOf(x.bits)
  })
  val dataPathToExus = (dataPath.io.toIntExu ++ dataPath.io.toFpExu ++ dataPath.io.toVecExu).flatten
  dataPathToExus.map(x => {
    x.ready := false.B
  })
  dataPath.io.ldCancel := 0.U.asTypeOf(dataPath.io.ldCancel)
  dataPath.io.fromIntWb := 0.U.asTypeOf(dataPath.io.fromIntWb)
  dataPath.io.fromFpWb := 0.U.asTypeOf(dataPath.io.fromFpWb)
  dataPath.io.fromVfWb := 0.U.asTypeOf(dataPath.io.fromVfWb)
  dataPath.io.fromV0Wb := 0.U.asTypeOf(dataPath.io.fromV0Wb)
  dataPath.io.fromVlWb := 0.U.asTypeOf(dataPath.io.fromVlWb)
  dataPath.io.wbConfictRead := 0.U.asTypeOf(dataPath.io.wbConfictRead)
  dataPath.io.fromBypassNetwork := 0.U.asTypeOf(dataPath.io.fromBypassNetwork)
  dataPath.io.fromPcTargetMem.toDataPathTargetPC := 0.U.asTypeOf(dataPath.io.fromPcTargetMem.toDataPathTargetPC)
  dataPath.io.fromPcTargetMem.toDataPathPC := 0.U.asTypeOf(dataPath.io.fromPcTargetMem.toDataPathPC)
  dataPath.io.topDownInfo.lqEmpty := false.B
  dataPath.io.topDownInfo.sqEmpty := false.B
  dataPath.io.topDownInfo.l1Miss := false.B
  dataPath.io.topDownInfo.l2TopMiss.l2Miss := false.B
  dataPath.io.topDownInfo.l2TopMiss.l3Miss := false.B

  bypassNetwork.io.fromDataPath.int.foreach(x => x.foreach{ xx =>
      xx.valid := false.B
      xx.bits := 0.U.asTypeOf(xx.bits)
  })
  bypassNetwork.io.fromDataPath.fp.foreach(x => x.foreach { xx =>
    xx.valid := false.B
    xx.bits := 0.U.asTypeOf(xx.bits)
  })
  bypassNetwork.io.fromDataPath.vf.foreach(x => x.foreach { xx =>
    xx.valid := false.B
    xx.bits := 0.U.asTypeOf(xx.bits)
  })
  bypassNetwork.io.fromDataPath.immInfo := 0.U.asTypeOf(bypassNetwork.io.fromDataPath.immInfo)
  bypassNetwork.io.fromDataPath.rcData := 0.U.asTypeOf(bypassNetwork.io.fromDataPath.rcData)
  bypassNetwork.io.fromExus := 0.U.asTypeOf(bypassNetwork.io.fromExus)
  val bypassNetworkToExus = (bypassNetwork.io.toExus.int ++ bypassNetwork.io.toExus.fp ++ bypassNetwork.io.toExus.vf).flatten
  bypassNetworkToExus.map(_.ready := false.B)

  wbDataPath.io.flush := io.flush
  wbDataPath.io.fromTop.hartId := io.hartId
  wbDataPath.io.fromIntExu.flatten.map { case x =>
    x.valid := false.B
    x.bits := 0.U.asTypeOf(x.bits)
  }
  if (!params.isFpSchd){
    wbDataPath.io.fromFpExu.flatten.map { case x =>
      x.valid := false.B
      x.bits := 0.U.asTypeOf(x.bits)
    }
  }
  wbDataPath.io.fromVfExu.flatten.map { case x =>
    x.valid := false.B
    x.bits := 0.U.asTypeOf(x.bits)
  }
  wbDataPath.io.fromCSR.vstart := 0.U
  io.toIntPreg := 0.U.asTypeOf(io.toIntPreg)
  io.toFpPreg  := 0.U.asTypeOf(io.toFpPreg )
  io.toVfPreg  := 0.U.asTypeOf(io.toVfPreg )
  io.toV0Preg  := 0.U.asTypeOf(io.toV0Preg )
  io.toVlPreg  := 0.U.asTypeOf(io.toVlPreg )
  io.vlWriteBackInfoOut := 0.U.asTypeOf(io.vlWriteBackInfoOut)

  exuBlock.io.frm.foreach(_ := io.frm)
  exuBlock.io.vxrm.foreach(_ := io.vxrm)
  exuBlock.io.flush := io.flush

  val wbFuBusyTableWrite = Wire(MixedVec(params.issueBlockParams.map(x => x.genWbFuBusyTableWriteBundle)))
  issueQueues.zipWithIndex.foreach { case (iq, i) =>
    wbFuBusyTableWrite(i) := iq.io.wbBusyTableWrite
  }
  io.wbFuBusyTableWriteOut := wbFuBusyTableWrite
  val criticalErrors = exuBlock.getCriticalErrors
  for (((name, error), _) <- criticalErrors.zipWithIndex) {
    XSError(error, s"critical error: $name \n")
  }
  generateCriticalErrors()
  if (params.isIntSchd) {
    io.toFrontendBJUResolve.get := exuBlock.io.toFrontendBJUResolve.get
    issueQueues.zipWithIndex.foreach { case (iq, i) =>
      iq.io.wbBusyTableRead := wbFuBusyTable.io.out.intRespRead(i)
    }
    io.vlWriteBackInfoOut.vlFromIntIsZero := exuBlock.io.vlIsZero.get
    io.vlWriteBackInfoOut.vlFromIntIsVlmax := exuBlock.io.vlIsVlmax.get
    io.I2FWakeupOut.get := exuBlock.io.I2FWakeupOut.get
    io.I2FDataOut.get := exuBlock.io.I2FDataOut.get
    exuBlock.io.F2IDataIn.get := io.F2IDataIn.get
    exuBlock.io.csrio.get <> io.csrio.get
    exuBlock.io.csrin.get := io.csrin.get
    val fromMemExuOutput = Wire(params.genExuOutputDecoupledBundleMemBlock)
    fromMemExuOutput.flatten.zip(io.fromMemExuOutput).map{ case (sink, source) => {
      sink.valid := source.valid
      source.ready := sink.ready
      sink.bits.data := VecInit(Seq.fill(sink.bits.params.wbPathNum)(source.bits.data))
      sink.bits.pdest := source.bits.uop.pdest
      sink.bits.robIdx := source.bits.uop.robIdx
      sink.bits.intWen.foreach(_ := source.bits.uop.rfWen)
      sink.bits.fpWen.foreach(_ := source.bits.uop.fpWen)
      sink.bits.vecWen.foreach(_ := source.bits.uop.vecWen)
      sink.bits.v0Wen.foreach(_ := source.bits.uop.v0Wen)
      sink.bits.vlWen.foreach(_ := source.bits.uop.vlWen)
      sink.bits.exceptionVec.foreach(_ := source.bits.uop.exceptionVec)
      sink.bits.flushPipe.foreach(_ := source.bits.uop.flushPipe)
      sink.bits.replay.foreach(_ := source.bits.uop.replayInst)
      sink.bits.debug := source.bits.debug
      sink.bits.debugInfo := source.bits.uop.debugInfo
      sink.bits.debug_seqNum := source.bits.uop.debug_seqNum
      sink.bits.lqIdx.foreach(_ := source.bits.uop.lqIdx)
      sink.bits.sqIdx.foreach(_ := source.bits.uop.sqIdx)
      sink.bits.predecodeInfo.foreach(_ := source.bits.uop.preDecodeInfo)
      sink.bits.vls.foreach(x => {
        x.vdIdx := source.bits.vdIdx.get
        x.vdIdxInField := source.bits.vdIdxInField.get
        x.vpu := source.bits.uop.vpu
        x.oldVdPsrc := source.bits.uop.psrc(2)
        x.isIndexed := VlduType.isIndexed(source.bits.uop.fuOpType)
        x.isMasked := VlduType.isMasked(source.bits.uop.fuOpType)
        x.isStrided := VlduType.isStrided(source.bits.uop.fuOpType)
        x.isWhole := VlduType.isWhole(source.bits.uop.fuOpType)
        x.isVecLoad := VlduType.isVecLd(source.bits.uop.fuOpType)
        x.isVlm := VlduType.isMasked(source.bits.uop.fuOpType) && VlduType.isVecLd(source.bits.uop.fuOpType)
      })
      sink.bits.trigger.foreach(_ := source.bits.uop.trigger)
    }
    }
    println(s"[Region_int] wbDataPath.io.fromIntExu.size = ${wbDataPath.io.fromIntExu.size}")
    println(s"[Region_int] exuBlock.io.out.size = ${exuBlock.io.out.size}")
    println(s"[Region_int] fromMemExuOutput.size = ${fromMemExuOutput.size}")
    wbDataPath.io.fromIntExu.flatten.zip((exuBlock.io.out ++ fromMemExuOutput).flatten).map{ case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
      source.ready := sink.ready
    }
    wbDataPath.io.fromFpExu.flatten.zip((io.fromFpExu.get).flatten).map { case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
    }
    wbDataPath.io.fromVfExu.flatten.zip((io.fromVecExu.get).flatten).map { case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
    }
    io.vtype.get := exuBlock.io.vtype.get
    io.toIntPreg := wbDataPath.io.toIntPreg
    io.fenceio.get <> exuBlock.io.fenceio.get
    io.csrToDecode.get <> exuBlock.io.csrToDecode.get

    dataPath.io.ldCancel := io.ldCancel
    dataPath.io.fromIntIQ.zip(issueQueues).zip(io.intIQOut.get).map{ case ((sink, source), iqOut) =>
      sink.zipWithIndex.map{ case (s, i) =>
        s.valid := source.io.deqDelay(i).valid
        iqOut(i).valid := source.io.deqDelay(i).valid
        s.bits := source.io.deqDelay(i).bits
        iqOut(i).bits := source.io.deqDelay(i).bits
        source.io.deqDelay(i).ready := s.ready && iqOut(i).ready
      }
    }
    // for write int regfile and resp
    dataPath.io.fromFpIQ.zip(io.fromFpIQ.get).map { case (sink, source) =>
      sink <> source
    }
    io.intToFpIQResp.get := dataPath.io.toFpIQ
    dataPath.io.fromIntWb := wbDataPath.io.toIntPreg
    dataPath.io.fromPcTargetMem <> io.fromPcTargetMem.get
    dataPath.io.fromBypassNetwork := bypassNetwork.io.toDataPath
    dataPath.io.diffIntRat.foreach(_ := io.diffIntRat.get)

    bypassNetwork.io.fromDataPath.int <> dataPath.io.toIntExu
    bypassNetwork.io.fromDataPath.immInfo := dataPath.io.og1ImmInfo
    bypassNetwork.io.fromDataPath.rcData := dataPath.io.toBypassNetworkRCData
    bypassNetwork.io.fromExus.connectExuOutput(_.int)(exuBlock.io.out)
    bypassNetwork.io.fromExus.connectExuOutput(_.fp)(io.formFpExuBlockOut.get)
    // no use
    io.formFpExuBlockOut.get.flatten.map(_.ready := true.B)
    val intLoadWB = bypassNetwork.io.fromExus.int.flatten.filter(_.bits.params.hasLoadExu)
    intLoadWB.zip(io.fromMemExuOutput.take(intLoadWB.size)).foreach { case (sink, source) =>
      sink.valid := source.valid
      sink.bits.intWen := source.bits.uop.rfWen && source.bits.isFromLoadUnit
      sink.bits.pdest := source.bits.uop.pdest
      sink.bits.data := source.bits.data
    }
    for (i <- 0 until exuBlock.io.in.length) {
      for (j <- 0 until exuBlock.io.in(i).length) {
        val shouldLdCancel = LoadShouldCancel(bypassNetwork.io.toExus.int(i)(j).bits.loadDependency, io.ldCancel)
        val rightOut = Wire(chiselTypeOf(exuBlock.io.in(i)(j)))
        // no block for uops to exu, when idiv busy, use og1 resp
        rightOut.ready := true.B
        NewPipelineConnect(
          bypassNetwork.io.toExus.int(i)(j), rightOut, rightOut.fire,
          Mux(
            bypassNetwork.io.toExus.int(i)(j).valid,
            bypassNetwork.io.toExus.int(i)(j).bits.robIdx.needFlush(io.flush) || shouldLdCancel,
            false.B
          ),
          Option(s"pipeTo${rightOut.bits.params.name}")
        )
        exuBlock.io.in(i)(j).valid := rightOut.valid
        exuBlock.io.in(i)(j).bits := rightOut.bits
        val ldCancelResp = !exuBlock.io.in(i)(j).bits.params.needUncertainWakeup.B || !shouldLdCancel
        bypassNetwork.io.toExus.int(i)(j).ready := exuBlock.io.in(i)(j).ready && ldCancelResp
      }
    }
    val toMem = Wire(io.toMemExu.get.cloneType)
    io.toMemExu.get <> toMem
    val firstMemExu = bypassNetwork.io.toExus.int.indexWhere(x => x.map(xx => xx.bits.params.isMemExeUnit).reduce(_ || _))
    println(s"[Regin_int] firstMemExu = $firstMemExu")
    for (i <- toMem.indices) {
      for (j <- toMem(i).indices) {
        val toMemExuInput = bypassNetwork.io.toExus.int(firstMemExu + i)(j)
        val shouldLdCancel = LoadShouldCancel(toMemExuInput.bits.loadDependency, io.ldCancel)
        val needIssueTimeout = toMemExuInput.bits.params.hasLoadExu
        val issueTimeout =
          if (needIssueTimeout)
            Counter(0 until 16, toMem(i)(j).valid && !toMem(i)(j).fire, toMemExuInput.fire)._2
          else
            false.B

        NewPipelineConnect(
          toMemExuInput, toMem(i)(j), toMem(i)(j).fire,
          Mux(
            toMemExuInput.fire,
            toMemExuInput.bits.robIdx.needFlush(io.flush) || shouldLdCancel,
            toMem(i)(j).bits.robIdx.needFlush(io.flush) || issueTimeout
          ),
          Option(s"pipeTo${toMemExuInput.bits.params.name}")
        )
        val thisIQ = issueQueues.filter(x => x.param.allExuParams.contains(toMem(i)(j).bits.params)).head
        if (needIssueTimeout) {
          thisIQ.io.finalIssueResp.get(j).valid := issueTimeout
          thisIQ.io.finalIssueResp.get(j).bits.fuType := toMem(i)(j).bits.fuType
          thisIQ.io.finalIssueResp.get(j).bits.resp := RespType.block
          thisIQ.io.finalIssueResp.get(j).bits.robIdx := toMem(i)(j).bits.robIdx
          thisIQ.io.finalIssueResp.get(j).bits.uopIdx.foreach(_ := toMem(i)(j).bits.vpu.get.vuopIdx)
          thisIQ.io.finalIssueResp.get(j).bits.sqIdx.foreach(_ := toMem(i)(j).bits.sqIdx.get)
          thisIQ.io.finalIssueResp.get(j).bits.lqIdx.foreach(_ := toMem(i)(j).bits.lqIdx.get)
        }
        if (thisIQ.io.memAddrIssueResp.nonEmpty) {
          thisIQ.io.memAddrIssueResp.get(j).valid := toMem(i)(j).fire && FuType.isLoad(toMem(i)(j).bits.fuType)
          thisIQ.io.memAddrIssueResp.get(j).bits.fuType := toMem(i)(j).bits.fuType
          thisIQ.io.memAddrIssueResp.get(j).bits.robIdx := toMem(i)(j).bits.robIdx
          thisIQ.io.memAddrIssueResp.get(j).bits.sqIdx.foreach(_ := toMem(i)(j).bits.sqIdx.get)
          thisIQ.io.memAddrIssueResp.get(j).bits.lqIdx.foreach(_ := toMem(i)(j).bits.lqIdx.get)
          thisIQ.io.memAddrIssueResp.get(j).bits.resp := RespType.success // for load inst, firing at toMem means issuing successfully
        }
      }
    }
    io.exuOut.flatten.zip((exuBlock.io.out ++ fromMemExuOutput).flatten).map { case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
    }
  }
  else if (params.isFpSchd) {
    issueQueues.zipWithIndex.foreach { case (iq, i) =>
      iq.io.wbBusyTableRead := wbFuBusyTable.io.out.fpRespRead(i)
    }
    io.F2IWakeupOut.get := exuBlock.io.F2IWakeupOut.get
    io.F2IDataOut.get := exuBlock.io.F2IDataOut.get
    exuBlock.io.I2FDataIn.get := io.I2FDataIn.get
    wbDataPath.io.fromFpExu.flatten.zip(exuBlock.io.out.flatten).map{ case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
      source.ready := sink.ready
    }
    wbDataPath.io.fromIntExu.flatten.zip((io.fromIntExu.get).flatten).map { case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
    }
    wbDataPath.io.fromVfExu.flatten.zip((io.fromVecExu.get).flatten).map { case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
    }
    dataPath.io.ldCancel := io.ldCancel
    dataPath.io.fromFpIQ.zip(issueQueues).zip(io.fpIQOut.get).map { case ((sink, source), iqOut) =>
      sink.zipWithIndex.map { case (s, i) =>
        s.valid := source.io.deqDelay(i).valid
        iqOut(i).valid := source.io.deqDelay(i).valid
        s.bits := source.io.deqDelay(i).bits
        iqOut(i).bits := source.io.deqDelay(i).bits
        source.io.deqDelay(i).ready := s.ready && iqOut(i).ready
      }
    }
    // for read fp regfile and resp
    dataPath.io.fromIntIQ.zip(io.fromIntIQ.get).map { case (sink, source) =>
      sink <> source
    }
    io.fpToIntIQResp.get := dataPath.io.toIntIQ
    dataPath.io.fromFpWb := wbDataPath.io.toFpPreg
    dataPath.io.fromBypassNetwork <> bypassNetwork.io.toDataPath
    dataPath.io.diffFpRat.foreach(_ := io.diffFpRat.get)
    io.toFpPreg := wbDataPath.io.toFpPreg
    bypassNetwork.io.fromDataPath.fp <> dataPath.io.toFpExu

    val intLoadWB = bypassNetwork.io.fromExus.int.flatten.filter(_.bits.params.hasLoadExu)
    intLoadWB.zip(io.fromLduOutput.get).foreach { case (sink, source) =>
      sink.valid := source.valid
      sink.bits.intWen := source.bits.uop.rfWen && source.bits.isFromLoadUnit
      sink.bits.pdest := source.bits.uop.pdest
      sink.bits.data := source.bits.data
    }
    bypassNetwork.io.fromExus.connectExuOutput(_.fp)(exuBlock.io.out)
    for (i <- 0 until exuBlock.io.in.length) {
      for (j <- 0 until exuBlock.io.in(i).length) {
        val shouldLdCancel = LoadShouldCancel(bypassNetwork.io.toExus.fp(i)(j).bits.loadDependency, io.ldCancel)
        val rightOut = Wire(chiselTypeOf(exuBlock.io.in(i)(j)))
        rightOut.ready := true.B
        NewPipelineConnect(
          bypassNetwork.io.toExus.fp(i)(j), rightOut, rightOut.fire,
          Mux(
            bypassNetwork.io.toExus.fp(i)(j).valid,
            bypassNetwork.io.toExus.fp(i)(j).bits.robIdx.needFlush(io.flush) || shouldLdCancel,
            false.B
          ),
          Option(s"pipeTo${rightOut.bits.params.name}")
        )
        exuBlock.io.in(i)(j).valid := rightOut.valid
        exuBlock.io.in(i)(j).bits := rightOut.bits
        val ldCancelResp = !exuBlock.io.in(i)(j).bits.params.needUncertainWakeup.B || !shouldLdCancel
        bypassNetwork.io.toExus.fp(i)(j).ready := exuBlock.io.in(i)(j).ready && ldCancelResp
      }
    }
    io.exuOut.flatten.zip((exuBlock.io.out).flatten).map { case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
    }
    io.fpExuBlockOut.get.flatten.zip(exuBlock.io.out.flatten).map{ case(sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
    }
  }
  else if (params.isVecSchd) {
    issueQueues.zipWithIndex.foreach { case (iq, i) =>
      iq.io.wbBusyTableRead := wbFuBusyTable.io.out.vfRespRead(i)
      iq.io.vlFromIntIsZero := io.vlWriteBackInfoIn.vlFromIntIsZero
      iq.io.vlFromIntIsVlmax := io.vlWriteBackInfoIn.vlFromIntIsVlmax
      iq.io.vlFromVfIsZero := io.vlWriteBackInfoIn.vlFromVfIsZero
      iq.io.vlFromVfIsVlmax := io.vlWriteBackInfoIn.vlFromVfIsVlmax
    }
    val og2Resp = issueQueues.map(_.io.og2Resp.get).flatten
    og2Resp.zip(og2ForVector.get.io.toVfIQOg2Resp.flatten).map{ case (sink, source) =>
      sink := source
    }
    io.vlWriteBackInfoOut.vlFromVfIsZero := exuBlock.io.vlIsZero.get
    io.vlWriteBackInfoOut.vlFromVfIsVlmax := exuBlock.io.vlIsVlmax.get
    io.vtype.get := exuBlock.io.vtype.get
    val fromMemExuOutput = Wire(params.genExuOutputDecoupledBundleMemBlock)
    fromMemExuOutput.flatten.zip(io.fromMemExuOutput).map { case (sink, source) => {
      sink.valid := source.valid
      source.ready := sink.ready
      sink.bits.data := VecInit(Seq.fill(sink.bits.params.wbPathNum)(source.bits.data))
      sink.bits.pdest := source.bits.uop.pdest
      sink.bits.robIdx := source.bits.uop.robIdx
      sink.bits.intWen.foreach(_ := source.bits.uop.rfWen)
      sink.bits.fpWen.foreach(_ := source.bits.uop.fpWen)
      sink.bits.vecWen.foreach(_ := source.bits.uop.vecWen)
      sink.bits.v0Wen.foreach(_ := source.bits.uop.v0Wen)
      sink.bits.vlWen.foreach(_ := source.bits.uop.vlWen)
      sink.bits.exceptionVec.foreach(_ := source.bits.uop.exceptionVec)
      sink.bits.flushPipe.foreach(_ := source.bits.uop.flushPipe)
      sink.bits.replay.foreach(_ := source.bits.uop.replayInst)
      sink.bits.debug := source.bits.debug
      sink.bits.debugInfo := source.bits.uop.debugInfo
      sink.bits.debug_seqNum := source.bits.uop.debug_seqNum
      sink.bits.lqIdx.foreach(_ := source.bits.uop.lqIdx)
      sink.bits.sqIdx.foreach(_ := source.bits.uop.sqIdx)
      sink.bits.predecodeInfo.foreach(_ := source.bits.uop.preDecodeInfo)
      sink.bits.vls.foreach(x => {
        x.vdIdx := source.bits.vdIdx.get
        x.vdIdxInField := source.bits.vdIdxInField.get
        x.vpu := source.bits.uop.vpu
        x.oldVdPsrc := source.bits.uop.psrc(2)
        x.isIndexed := VlduType.isIndexed(source.bits.uop.fuOpType)
        x.isMasked := VlduType.isMasked(source.bits.uop.fuOpType)
        x.isStrided := VlduType.isStrided(source.bits.uop.fuOpType)
        x.isWhole := VlduType.isWhole(source.bits.uop.fuOpType)
        x.isVecLoad := VlduType.isVecLd(source.bits.uop.fuOpType)
        x.isVlm := VlduType.isMasked(source.bits.uop.fuOpType) && VlduType.isVecLd(source.bits.uop.fuOpType)
      })
      sink.bits.trigger.foreach(_ := source.bits.uop.trigger)
    }
    }
    wbDataPath.io.fromVfExu.flatten.zip((exuBlock.io.out ++ fromMemExuOutput).flatten).map { case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
      source.ready := sink.ready
    }
    wbDataPath.io.fromIntExu.flatten.zip((io.fromIntExu.get).flatten).map { case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
    }
    wbDataPath.io.fromFpExu.flatten.zip((io.fromFpExu.get).flatten).map { case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
    }
    wbDataPath.io.fromCSR.vstart := io.vstart.get
    io.toVfPreg := wbDataPath.io.toVfPreg
    io.toV0Preg := wbDataPath.io.toV0Preg
    io.toVlPreg := wbDataPath.io.toVlPreg
    io.toVecExcpMod.foreach(_ := dataPath.io.toVecExcpMod.get)

    dataPath.io.fromVfIQ.zip(issueQueues).map { case (sink, source) =>
      sink <> source.io.deqDelay
    }
    dataPath.io.fromVfWb := wbDataPath.io.toVfPreg
    dataPath.io.fromV0Wb := wbDataPath.io.toV0Preg
    dataPath.io.fromVlWb := wbDataPath.io.toVlPreg
    dataPath.io.fromBypassNetwork <> bypassNetwork.io.toDataPath
    dataPath.io.diffVecRat.foreach(_ := io.diffVecRat.get)
    dataPath.io.diffV0Rat.foreach(_ := io.diffV0Rat.get)
    dataPath.io.diffVlRat.foreach(_ := io.diffVlRat.get)

    dataPath.io.fromVecExcpMod.foreach(_ := io.fromVecExcpMod.get)
    og2ForVector.get.io.flush := io.flush
    og2ForVector.get.io.ldCancel := io.ldCancel
    og2ForVector.get.io.fromOg1VfArith <> dataPath.io.toVecExu
    og2ForVector.get.io.fromOg1ImmInfo := dataPath.io.og1ImmInfo.zip(backendParams.allExuParams).filter(_._2.needOg2).map(_._1)

    bypassNetwork.io.fromDataPath.vf <> og2ForVector.get.io.toVfArithExu
    bypassNetwork.io.fromDataPath.immInfo.zip(backendParams.allExuParams).filter(_._2.needOg2).map(_._1)
      .zip(og2ForVector.get.io.toBypassNetworkImmInfo).foreach {
      case (immInfo, og2ImmInfo) => immInfo := og2ImmInfo
    }
    bypassNetwork.io.fromExus.connectExuOutput(_.vf)(exuBlock.io.out)
    for (i <- 0 until exuBlock.io.in.size) {
      for (j <- 0 until exuBlock.io.in(i).size) {
        val leftIn = Wire(chiselTypeOf(bypassNetwork.io.toExus.vf(i)(j)))
        leftIn.valid := bypassNetwork.io.toExus.vf(i)(j).valid
        leftIn.bits := bypassNetwork.io.toExus.vf(i)(j).bits
        leftIn.ready := true.B
        bypassNetwork.io.toExus.vf(i)(j).ready := exuBlock.io.in(i)(j).ready && !(exuBlock.io.in(i)(j).fire && FuType.isUncertain(exuBlock.io.in(i)(j).bits.fuType))
        NewPipelineConnect(
          leftIn, exuBlock.io.in(i)(j), exuBlock.io.in(i)(j).fire,
          Mux(
            bypassNetwork.io.toExus.vf(i)(j).fire,
            bypassNetwork.io.toExus.vf(i)(j).bits.robIdx.needFlush(io.flush),
            exuBlock.io.in(i)(j).bits.robIdx.needFlush(io.flush)
          ) || !bypassNetwork.io.toExus.vf(i)(j).ready,
          Option(s"pipeTo${leftIn.bits.params.name}")
        )
      }
    }
    val toMem = Wire(io.toMemExu.get.cloneType)
    io.toMemExu.get <> toMem
    val firstMemExu = bypassNetwork.io.toExus.vf.indexWhere(x => x.map(xx => xx.bits.params.isMemExeUnit).reduce(_ || _))
    println(s"[Regin_int] firstMemExu = $firstMemExu")
    for (i <- toMem.indices) {
      for (j <- toMem(i).indices) {
        val toMemExuInput = bypassNetwork.io.toExus.vf(firstMemExu + i)(j)
        val needIssueTimeout = toMemExuInput.bits.params.hasVLoadFu
        val issueTimeout =
          if (needIssueTimeout)
            Counter(0 until 16, toMem(i)(j).valid && !toMem(i)(j).fire, toMemExuInput.fire)._2
          else
            false.B

        NewPipelineConnect(
          toMemExuInput, toMem(i)(j), toMem(i)(j).fire,
          Mux(
            toMemExuInput.fire,
            toMemExuInput.bits.robIdx.needFlush(io.flush),
            toMem(i)(j).bits.robIdx.needFlush(io.flush) || issueTimeout
          ),
          Option(s"pipeTo${toMemExuInput.bits.params.name}")
        )
        val thisIQ = issueQueues.filter(x => x.param.allExuParams.contains(toMem(i)(j).bits.params)).head
        if (needIssueTimeout) {
          thisIQ.io.finalIssueResp.get(j).valid := issueTimeout
          thisIQ.io.finalIssueResp.get(j).bits.fuType := toMem(i)(j).bits.fuType
          thisIQ.io.finalIssueResp.get(j).bits.resp := RespType.block
          thisIQ.io.finalIssueResp.get(j).bits.robIdx := toMem(i)(j).bits.robIdx
          thisIQ.io.finalIssueResp.get(j).bits.uopIdx.foreach(_ := toMem(i)(j).bits.vpu.get.vuopIdx)
          thisIQ.io.finalIssueResp.get(j).bits.sqIdx.foreach(_ := toMem(i)(j).bits.sqIdx.get)
          thisIQ.io.finalIssueResp.get(j).bits.lqIdx.foreach(_ := toMem(i)(j).bits.lqIdx.get)
        }
        if (thisIQ.io.vecLoadIssueResp.nonEmpty) {
          thisIQ.io.vecLoadIssueResp.get(j) match {
            case resp =>
              resp.valid := toMem(i)(j).fire && VlduType.isVecLd(toMem(i)(j).bits.fuOpType)
              resp.bits.fuType := toMem(i)(j).bits.fuType
              resp.bits.robIdx := toMem(i)(j).bits.robIdx
              resp.bits.uopIdx.get := toMem(i)(j).bits.vpu.get.vuopIdx
              resp.bits.sqIdx.get := toMem(i)(j).bits.sqIdx.get
              resp.bits.lqIdx.get := toMem(i)(j).bits.lqIdx.get
              resp.bits.resp := RespType.success
          }
        }
      }
    }
    io.exuOut.flatten.zip((exuBlock.io.out ++ fromMemExuOutput).flatten).map{ case (sink, source) =>
      sink.valid := source.valid
      sink.bits := source.bits
    }
  }
  io.wbDataPathToCtrlBlock := wbDataPath.io.toCtrlBlock
  io.IQValidNumVec := issueQueues.filter(_.param.StdCnt == 0).map(_.io.validCntDeqVec).flatten
  io.og0Cancel := dataPath.io.og0Cancel
  io.diffVl.foreach(_ := dataPath.io.diffVl.get)
  io.fromLduOutput.foreach(_.map(_.ready := true.B))
  io.fpRfRdataOut.foreach(_ := dataPath.io.fpRfRdataOut.get)
  dataPath.io.fpRfRdataIn.foreach(_ := io.fpRfRdataIn.get)
}

class RegionIO(val params: SchdBlockParams)(implicit p: Parameters) extends XSBundle {
  val intSchdParam = backendParams.intSchdParams.get
  val fpSchdParam = backendParams.fpSchdParams.get
  val vecSchdParam = backendParams.vecSchdParams.get
  // uops from dispatch, two level vec, not flatten
  val fromDispatch = MixedVec(params.issueBlockParams.filter(_.StdCnt == 0).map(x => Flipped(Vec(x.numEnq, DecoupledIO(new RegionInUop(x))))))
  val hartId = Input(UInt(8.W))
  val flush = Flipped(ValidIO(new Redirect))
  val ldCancel = Vec(backendParams.LduCnt, Flipped(new LoadCancelIO))
  val fromPcTargetMem = Option.when(params.isIntSchd)(Flipped(new PcToDataPathIO(backendParams)))
  val diffIntRat = Option.when(params.isIntSchd)(Input(Vec(32, UInt(params.pregIdxWidth.W))))
  val diffFpRat = Option.when(params.isFpSchd)(Input(Vec(32, UInt(params.pregIdxWidth.W))))
  val diffVecRat = Option.when(params.isVecSchd)(Input(Vec(31, UInt(params.pregIdxWidth.W))))
  val diffV0Rat = Option.when(params.isVecSchd)(Input(Vec(1, UInt(log2Up(V0PhyRegs).W))))
  val diffVlRat = Option.when(params.isVecSchd)(Input(Vec(1, UInt(log2Up(VlPhyRegs).W))))
  val diffVl = Option.when(params.isVecSchd)(Output(UInt(VlData().dataWidth.W)))
  val vlWriteBackInfoIn = new Bundle {
    val vlFromIntIsZero = Input(Bool())
    val vlFromIntIsVlmax = Input(Bool())
    val vlFromVfIsZero = Input(Bool())
    val vlFromVfIsVlmax = Input(Bool())
  }
  val vlWriteBackInfoOut = Flipped(new Bundle {
    val vlFromIntIsZero = Input(Bool())
    val vlFromIntIsVlmax = Input(Bool())
    val vlFromVfIsZero = Input(Bool())
    val vlFromVfIsVlmax = Input(Bool())
  })
  val wakeUpToDispatch = params.genIQWakeUpOutValidBundle
  val wakeUpFromFp = Option.when(params.isIntSchd)(Flipped(fpSchdParam.genIQWakeUpOutValidBundle))
  val wakeUpFromInt = Option.when(params.isFpSchd)(Flipped(intSchdParam.genIQWakeUpOutValidBundle))
  val wakeupFromI2F = Option.when(params.isFpSchd)(Flipped(ValidIO(new IssueQueueIQWakeUpBundle(params.backendParam.getExuIdxI2F, params.backendParam))))
  val wakeupFromF2I = Option.when(params.isIntSchd)(Flipped(ValidIO(new IssueQueueIQWakeUpBundle(params.backendParam.getExuIdxF2I, params.backendParam))))
  val I2FWakeupOut = Option.when(params.isIntSchd)(ValidIO(new IssueQueueIQWakeUpBundle(params.backendParam.getExuIdxI2F, params.backendParam)))
  val I2FDataOut = Option.when(params.isIntSchd)(ValidIO(UInt(XLEN.W)))
  val I2FDataIn = Option.when(params.isFpSchd)(Flipped(ValidIO(UInt(XLEN.W))))
  val F2IWakeupOut = Option.when(params.isFpSchd)(ValidIO(new IssueQueueIQWakeUpBundle(params.backendParam.getExuIdxF2I, params.backendParam)))
  val F2IDataOut = Option.when(params.isFpSchd)(ValidIO(UInt(XLEN.W)))
  val F2IDataIn = Option.when(params.isIntSchd)(Flipped(ValidIO(UInt(XLEN.W))))
  val toMemExu = Option.when(!params.isFpSchd)(params.genExuInputCopySrcBundleMemBlock)
  // fromMem
  val wakeupFromLDU = Option.when(params.isIntSchd)(Vec(params.LdExuCnt, Flipped(Valid(new MemWakeUpBundle))))
  val staFeedback = Option.when(params.isIntSchd)(Flipped(Vec(params.StaCnt, new MemRSFeedbackIO)))
  val vstuFeedback = Option.when(params.isVecSchd)(Flipped(Vec(params.VstuCnt, new MemRSFeedbackIO(isVector = true))))
  val fromVecExcpMod = Option.when(params.isVecSchd)(Input(new ExcpModToVprf(maxMergeNumPerCycle * 2, maxMergeNumPerCycle)))
  val csrio = Option.when(params.hasCSR)(new CSRFileIO)
  val csrin = Option.when(params.hasCSR)(new CSRInput)
  val csrToDecode = Option.when(params.hasCSR)(Output(new CSRToDecode))
  val vtype = Option.when(params.writeVConfig)((Valid(new VType)))
  val wbDataPathToCtrlBlock = new Bundle {
    val writeback: MixedVec[ValidIO[ExuOutput]] = MixedVec(params.genExuOutputValidBundle.flatten)
  }
  val fromMemExuOutput = Flipped(Vec(params.issueBlockParams.filter(_.isMemBlockIQ).size, DecoupledIO(new MemExuOutput(params.isVecSchd))))
  val fromLduOutput = Option.when(params.isFpSchd)(Flipped(Vec(intSchdParam.issueBlockParams.filter(_.isLdAddrIQ).size, DecoupledIO(new MemExuOutput(params.isVecSchd)))))
  val lqDeqPtr = Option.when(params.isVecSchd)(Input(new LqPtr))
  val sqDeqPtr = Option.when(params.isVecSchd)(Input(new SqPtr))
  val allIssueParams = params.issueBlockParams.filter(_.StdCnt == 0)
  val IssueQueueDeqSum = allIssueParams.map(_.numDeq).sum
  val maxIQSize = allIssueParams.map(_.numEntries).max
  val IQValidNumVec = Output(Vec(IssueQueueDeqSum, UInt((maxIQSize).U.getWidth.W)))
  val toIntPreg = Flipped(MixedVec(Vec(backendParams.numPregWb(IntData()),
    new RfWritePortWithConfig(backendParams.intPregParams.dataCfg, backendParams.intPregParams.addrWidth))))
  val toFpPreg = Flipped(MixedVec(Vec(backendParams.numPregWb(FpData()),
    new RfWritePortWithConfig(backendParams.fpPregParams.dataCfg, backendParams.fpPregParams.addrWidth))))
  val toVfPreg = Flipped(MixedVec(Vec(backendParams.numPregWb(VecData()),
    new RfWritePortWithConfig(backendParams.vfPregParams.dataCfg, backendParams.vfPregParams.addrWidth))))
  val toV0Preg = Flipped(MixedVec(Vec(backendParams.numPregWb(V0Data()),
    new RfWritePortWithConfig(backendParams.v0PregParams.dataCfg, backendParams.v0PregParams.addrWidth))))
  val toVlPreg = Flipped(MixedVec(Vec(backendParams.numPregWb(VlData()),
    new RfWritePortWithConfig(backendParams.vlPregParams.dataCfg, backendParams.vlPregParams.addrWidth))))
  val fromIntWb = MixedVec(Vec(backendParams.numPregWb(IntData()),
    new RfWritePortWithConfig(backendParams.intPregParams.dataCfg, backendParams.intPregParams.addrWidth)))
  val fromFpWb = MixedVec(Vec(backendParams.numPregWb(FpData()),
    new RfWritePortWithConfig(backendParams.fpPregParams.dataCfg, backendParams.fpPregParams.addrWidth)))
  val fromVfWb = MixedVec(Vec(backendParams.numPregWb(VecData()),
    new RfWritePortWithConfig(backendParams.vfPregParams.dataCfg, backendParams.vfPregParams.addrWidth)))
  val fromV0Wb = MixedVec(Vec(backendParams.numPregWb(V0Data()),
    new RfWritePortWithConfig(backendParams.v0PregParams.dataCfg, backendParams.v0PregParams.addrWidth)))
  val fromVlWb = MixedVec(Vec(backendParams.numPregWb(VlData()),
    new RfWritePortWithConfig(backendParams.vlPregParams.dataCfg, backendParams.vlPregParams.addrWidth)))
  val I2FWakeupIn = Option.when(params.isFpSchd)(Flipped(ValidIO(new IssueQueueIQWakeUpBundle(params.backendParam.getExuIdxI2F, params.backendParam))))
  val F2IWakeupIn = Option.when(params.isIntSchd)(Flipped(ValidIO(new IssueQueueIQWakeUpBundle(params.backendParam.getExuIdxF2I, params.backendParam))))
  val og0Cancel = Output(ExuVec())
  val fenceio = Option.when(params.isIntSchd)(new FenceIO)
  val frm = Input(UInt(3.W))
  val vxrm = Input(UInt(2.W))
  val vstart = Option.when(params.isVecSchd)(Input(Vstart()))
  val toVecExcpMod = Option.when(params.isVecSchd)(Output(new VprfToExcpMod(maxMergeNumPerCycle * 2)))
  val exuOut = params.genExuOutputValidBundle
  val fromIntExu = Option.when(!params.isIntSchd)(Flipped(intSchdParam.genExuOutputValidBundle))
  val fromFpExu = Option.when(!params.isFpSchd)(Flipped(fpSchdParam.genExuOutputValidBundle))
  val fromVecExu = Option.when(!params.isVecSchd)(Flipped(vecSchdParam.genExuOutputValidBundle))
  val intSchdBusyTable = MixedVec(intSchdParam.issueBlockParams.map(x => Input(x.genWbFuBusyTableWriteBundle)))
  val fpSchdBusyTable = MixedVec(fpSchdParam.issueBlockParams.map(x => Input(x.genWbFuBusyTableWriteBundle)))
  val vfSchdBusyTable = MixedVec(vecSchdParam.issueBlockParams.map(x => Input(x.genWbFuBusyTableWriteBundle)))
  val wbFuBusyTableWriteOut = MixedVec(params.issueBlockParams.map(x => Output(x.genWbFuBusyTableWriteBundle)))
  val toFrontendBJUResolve = Option.when(params.isIntSchd)(Vec(backendParams.BrhCnt, Valid(new Resolve)))
  val fpExuBlockOut = Option.when(params.isFpSchd)(params.genExuOutputDecoupledBundle)
  val formFpExuBlockOut = Option.when(params.isIntSchd)(Flipped(fpSchdParam.genExuOutputDecoupledBundle))
  // to read fp regfile
  val intIQOut  = Option.when(params.isIntSchd)(MixedVec(params.issueBlockParams.map(_.genIssueDecoupledBundle)))
  val fromIntIQ = Option.when(params.isFpSchd)(Flipped(MixedVec(intSchdParam.issueBlockParams.map(_.genIssueDecoupledBundle))))
  val fpToIntIQResp = Option.when(params.isFpSchd)(MixedVec(intSchdParam.issueBlockParams.map(_.genOGRespBundle)))
  // fp regfile read data
  val fpRfRdataIn = Option.when(params.isIntSchd)(Input(Vec(backendParams.numPregRd(FpData()), UInt(backendParams.fpSchdParams.get.rfDataWidth.W))))
  val fpRfRdataOut = Option.when(params.isFpSchd)(Output(Vec(backendParams.numPregRd(FpData()), UInt(backendParams.fpSchdParams.get.rfDataWidth.W))))
  // to write int regfile
  val fpIQOut = Option.when(params.isFpSchd)(MixedVec(params.issueBlockParams.map(_.genIssueDecoupledBundle)))
  val fromFpIQ = Option.when(params.isIntSchd)(Flipped(MixedVec(fpSchdParam.issueBlockParams.map(_.genIssueDecoupledBundle))))
  val intToFpIQResp = Option.when(params.isIntSchd)(MixedVec(fpSchdParam.issueBlockParams.map(_.genOGRespBundle)))
}

