/***************************************************************************************
 * Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
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

package xiangshan.backend.float

import chisel3._
import chisel3.util._
import difftest.{DiffPhyFpRegState, DifftestModule}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.Bundles.{IssueQueueDeqOg1Payload, Og0InUop}
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.{FpRFReadArbiter, RFRdArbParams}
import xiangshan.backend.datapath.RdConfig.RdConfig
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.regfile.{FpRegFile, PregParams, VfRegFile}
import xiangshan.backend.rob.RobPtr
import xiangshan.backend.float.FltRegionModule._
import xiangshan.backend.vector.fu.VecFuConfig
import xiangshan.backend.vector.VecRegionModule.RfWriteBundle
import xiangshan.backend.{ExcpModToVprf, VprfToExcpMod}
import xiangshan.mem.StoreQueueDataWrite
import xiangshan.backend.vector.{ExuParam, IssueParam, IssuePipe, RegionParam, VecIssueQueue}
import xiangshan.backend.vector.VecIssueQueue.{BypassDelay, Enq, WakeUpBundle}
import xiangshan.backend.fu.vector.Bundles.Vxrm
import xiangshan.backend.float.FltIssueQueue.FltWakeUpBundle
import xiangshan.backend.float.FltWbDataPath
import xiangshan.backend.vector.Exu
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.issue.IntScheduler

class FltRegionModule(val regionParam: RegionParam)(implicit p: Parameters) extends LazyModule {
  override def shouldBeInlined: Boolean = false

  implicit val param: RegionParam = regionParam

  lazy val module = new FltRegionImp(this)

  val issueQueues: Seq[FltIssueQueue.LazyMod] = param.issueParams.map(issueParam =>
    LazyModule(new FltIssueQueue.LazyMod()(p, issueParam))
  )

  val issuePipes: Seq[Seq[FltIssuePipe.LazyMod]] = param.issueParams.map { case issueParam =>
    issueParam.exuParams.map { case exuParam =>
      LazyModule(new FltIssuePipe.LazyMod(exuParam))
    }
  }
}

class FltRegionImp(
  override val wrapper: FltRegionModule
)(
  implicit
  p: Parameters, param: RegionParam
) extends LazyModuleImp(wrapper) with HasXSParameter {
  val debugEn = backendParams.debugEn
  val basicDebugEn = backendParams.basicDebugEn

  val numFp = backendParams.fpPregParams.numEntries
  val numFpWritePort = backendParams.getFpRfWriteSize
  val numFpReadPort = backendParams.getFpRfReadSize
  val in = IO(Input(new In))
  val out = IO(Output(new Out))
  val fromIntIQ = IO(Flipped(MixedVec(backendParams.intSchdParams.get.issueBlockParams.map(_.genIssueDecoupledBundle))))

  dontTouch(in)
  dontTouch(out)

  val issueQueues: Seq[FltIssueQueue] = wrapper.issueQueues.map(x => x.module.suggestName(x.param.getInstanceNameOfIQ))
  val issuePipes: Seq[Seq[FltIssuePipe]] = wrapper.issuePipes.map(_.map(x => x.module.suggestName(x.param.getInstanceNameOfPipe)))

  val fpWbDataPath = Module(new FltWbDataPath(backendParams.fpPregParams))

  val fpRaddr = Wire(Vec(numFpReadPort, UInt(FpPhyRegIdxWidth.W)))
  val fpRdata = Wire(Vec(numFpReadPort, UInt(XLEN.W)))
  val fpWen   = Wire(Vec(numFpWritePort, Bool()))
  val fpWaddr = Wire(Vec(numFpWritePort, UInt(FpPhyRegIdxWidth.W)))
  val fpWdata = Wire(Vec(numFpWritePort, UInt(XLEN.W)))
  val fpDiffReadData: Option[Vec[UInt]] = Option.when(basicDebugEn)(Wire(Vec(numFp, UInt(XLEN.W))))


  FpRegFile("FpRegFile", FpPhyRegs, fpRaddr, fpRdata, fpWen, fpWaddr, fpWdata,
    bankNum = 1,
    debugAllRData = fpDiffReadData,
  )

  // These ids are global regfile writeback port indices from the active backend
  // configuration, not a per-region subset.
  private val intRegfileWbPortIds = backendParams.getWbPortIndices(IntData())
  private val fpRegfileWbPortIds = backendParams.getWbPortIndices(FpData())
  private val vpRegfileWbPortIds = backendParams.getWbPortIndices(VecData())
  private val v0RegfileWbPortIds = backendParams.getWbPortIndices(V0Data())
  private val vlRegfileWbPortIds = backendParams.getWbPortIndices(VlData())

  private val intWbPortIdxMap = intRegfileWbPortIds.zipWithIndex.toMap
  private val fpWbPortIdxMap = fpRegfileWbPortIds.zipWithIndex.toMap
  private val vpWbPortIdxMap = vpRegfileWbPortIds.zipWithIndex.toMap
  private val v0WbPortIdxMap = v0RegfileWbPortIds.zipWithIndex.toMap
  private val vlWbPortIdxMap = vlRegfileWbPortIds.zipWithIndex.toMap

  private val intNonFixedLatFuWbSources = nonFixedLatFuWbSources(_.getGpWriteCfg.map(_.port), _.writeIntRf)
  private val fpNonFixedLatFuWbSources = nonFixedLatFuWbSources(_.getFpWriteCfg.map(_.port), _.writeFpRf)
  private val vpNonFixedLatFuWbSources = nonFixedLatFuWbSources(_.getVpWriteCfg.map(_.port), _.writeVecRf)
  private val v0NonFixedLatFuWbSources = nonFixedLatFuWbSources(_.getV0WriteCfg.map(_.port), _.writeV0Rf)
  private val vlNonFixedLatFuWbSources = nonFixedLatFuWbSources(_.getVlWriteCfg.map(_.port), _.writeVlRf)

  private val intWbFuBusyTable = buildWbFuBusyTable(
    intRegfileWbPortIds,
    nonFixedLatFuPortIndices(_.intNonFixedLatFuWbPortIds, intWbPortIdxMap),
  )
  private val fpWbFuBusyTable = buildWbFuBusyTable(
    fpRegfileWbPortIds,
    nonFixedLatFuPortIndices(_.fpNonFixedLatFuWbPortIds, fpWbPortIdxMap),
  )

  private val intWbFuBusyTableRead = connectWbFuBusyTable(
    intWbFuBusyTable, "Int", intRegfileWbPortIds,
    issueQueueWbSources(_.out.toWbFuBusyTable.intWbFuBusyTableIn, _.intWbPortIds),
    intNonFixedLatFuWbSources,
  )
  private val fpWbFuBusyTableReadBase = connectWbFuBusyTable(
    fpWbFuBusyTable, "Fp", fpRegfileWbPortIds,
    issueQueueWbSources(_.out.toWbFuBusyTable.fpWbFuBusyTableIn, _.fpWbPortIds),
    fpNonFixedLatFuWbSources,
  )
  private val i2fWBPort = backendParams.getIntRegionParam.issueParams.filter(_.needFpWen).head.fpWbPortIds.head
  private val fpWbFuBusyTableRead = fpWbFuBusyTableReadBase.transform( (k, v) => {
    if (k == i2fWBPort) v | Cat(in.fromIntRegion.busyTableI2F, 0.U(4.W)) // TODO, why latency + 4?
    else v
  })
  private val intCtrlBlockRead = ctrlBlockRead(intWbFuBusyTable)
  private val fpCtrlBlockRead = ctrlBlockRead(fpWbFuBusyTable)

  issueQueues.foreach { iq =>
    connectBusyTableRead(iq.in.fromWbFuBusyTable.intWbFuBusyTableRead, iq.param.intWbPortIds, intWbFuBusyTableRead, intWbPortIdxMap)
    connectBusyTableRead(iq.in.fromWbFuBusyTable.fpWbFuBusyTableRead, iq.param.fpWbPortIds, fpWbFuBusyTableRead, fpWbPortIdxMap)
    connectBusyTableRead(iq.in.fromWbFuBusyTable.intCtrlBlockRead, iq.param.intWbPortIds, intCtrlBlockRead, intWbPortIdxMap)
    connectBusyTableRead(iq.in.fromWbFuBusyTable.fpCtrlBlockRead, iq.param.fpWbPortIds, fpCtrlBlockRead, fpWbPortIdxMap)
  }

  issueQueues.filterNot(_.param.hasVStd).zipWithIndex.foreach {
    case (iq, i) =>
      iq.in.flush := in.flush
      iq.in.enq := in.fromDispatch.uops(i)
  }
  // TODO, use M3 to arbiter
  val fpWbM2WakeUpThisRegion = issuePipes.flatten.filter(_.param.needFpWen).map(_.out.fpWbM2Wakeup)
  val fpWbM2WakeUpThisRegionIs1Lat = issuePipes.flatten.filter(_.param.needFpWen).map(_.out.fpWbM2WakeupIs1Lat)
  val fpWbM2WakeupOBeforeArbiter = RegNext(in.fromIntRegion.fpWbM3Wakeup) ++ fpWbM2WakeUpThisRegion ++ RegNext(in.fromVecRegion.fpWbM3Wakeup)
  val writeFpRfIssueParams = backendParams.allIssueParams.filter(_.writeFpRf)
  assert(writeFpRfIssueParams.size == fpWbM2WakeupOBeforeArbiter.size)
  val fpWbM2WakeUp = Wire(Vec(backendParams.getFpRfWriteSize, new FltWakeUpBundle(backendParams.fpPregParams)))
  val fpWbM2WakeUpIs1Lat = Wire(Vec(backendParams.getFpRfWriteSize, Bool()))
  fpWbM2WakeUpIs1Lat := fpWbM2WakeUpThisRegionIs1Lat ++ Seq.fill(fpWbM2WakeUpIs1Lat.size - fpWbM2WakeUpThisRegionIs1Lat.size)(false.B)
  for (wbPortIdx <- 0 until backendParams.getFpRfWriteSize) {
    val seqIdx = writeFpRfIssueParams.zipWithIndex.collect {
      case (issueParams, idx) if (issueParams.exuBlockParams.map(_.wbPortConfigs.map(x => x.writeFp && x.port == wbPortIdx).fold(false)(_ | _)).reduce(_ | _)) => idx
    }
    val seqWakeup = seqIdx.map(x => fpWbM2WakeupOBeforeArbiter(x))
    fpWbM2WakeUp(wbPortIdx) := Mux1H(seqWakeup.map(_.wen), seqWakeup)
  }

  val ex0RespFailLat1 = RegInit(0.U.asTypeOf(Vec(backendParams.getFltRegionParam.getFpWriteSize, Bool())))
  val ex0RespFailLat1Next = issuePipes.flatten.map(_.out.ex0RespFailLat1Next)
  ex0RespFailLat1 := ex0RespFailLat1Next


  val fpWbM2WakeupToDispatch = VecInit(fpWbM2WakeUp.zipWithIndex.map { case (wakeup, i) =>
    val d1 = Wire(new FltWakeUpBundle(backendParams.fpPregParams))
    d1 := wakeup
    if (i < ex0RespFailLat1Next.size) d1.wen := wakeup.wen && !ex0RespFailLat1Next(i)
    d1
  })
  val ldCancelToIQ = RegNext(in.fromMem.ldCancel)
  val fpWbM2D1Vec = RegNext(fpWbM2WakeupToDispatch)
  issueQueues.zipWithIndex.foreach {
    case (iq, i) =>
      iq.in.resps.is0 := issuePipes(i).map(_.out.is0Resp)
      iq.in.resps.is1 := issuePipes(i).map(_.out.is1Resp)
      iq.in.resps.ex0 := issuePipes(i).map(_.out.ex0Resp)
      iq.in.resps.ex0RespFailLat1 := ex0RespFailLat1
      iq.in.wakeup.fpWbM2Vec := fpWbM2WakeUp
      iq.in.wakeup.fpWbM2D1Vec := fpWbM2D1Vec
      iq.in.ldCancel := ldCancelToIQ
  }

  (issueQueues lazyZip issuePipes).zipWithIndex.foreach { case ((iq, pipes), iqIdx) =>
    pipes.zipWithIndex.foreach { case (pipe, pipeIdx) =>
      pipe.in.flush := in.flush
      pipe.in.is0Next := iq.out.deq(pipeIdx)
      pipe.in.is0RdFail := false.B // Todo
      pipe.in.is0WtFail := false.B // Todo
      pipe.in.ldCancel := in.fromMem.ldCancel
      pipe.in.ex0RespFailLat1 := ex0RespFailLat1
      pipe.in.frm.foreach(_ := in.fromCSR.frm)
      pipe.in.fpWb0Next := fpWbDataPath.out.wb0Next.map(_.data)
      pipe.in.fpWb0 := fpWbDataPath.out.wb0.map(_.data)
      pipe.in.fpWb1 := fpWbDataPath.out.wb1.map(_.data)
      pipe.in.busyTableEmpty.foreach(_ := !iq.in.fromWbFuBusyTable.fpWbFuBusyTableRead.get.reduce(_ | _).asUInt.orR)
      pipe.in.busyTableI2F.foreach(_ := in.fromIntRegion.busyTableI2F)
      pipe.in.is1FpRdDataNext.foreach { case rdata =>
        rdata.data := fpRdata(rdata.rdConfig.port)
      }
    }
  }

  println(s"[tmp-${this.getClass}] " +
    s"fpWbDataPath.in.fromExus: ${fpWbDataPath.in.fromExus.map(_.map(_.size))} " +
    s"issuePipes fpWbNext: ${issuePipes.map(_.map(_.out.fpWbNext).collect { case x if x.nonEmpty => x.get }.size)}" +
    s"in.fromIntRegion.fpWbNext: ${in.fromIntRegion.fpWbNext.map(_.size)}"
  )

  val fpWbFromIntRegion = in.fromIntRegion.fpWbNext
  fpWbDataPath.in.fromExus.flatten.flatten
    .zip(Seq(
      fpWbFromIntRegion,
      issuePipes.map(_.map(_.out.fpWbNext).collect { case x if x.nonEmpty => x.get }),
      in.fromVecRegion.fpWbNext
    ).flatten.flatten)
    .foreach { case (sink, source) => sink := source }

  fpWen := fpWbDataPath.out.wb0.map(_.wen)
  fpWaddr := fpWbDataPath.out.wb0.map(_.pdest)
  fpWdata := fpWbDataPath.out.wb0.map(_.data)
  // TODO
  private val intRFReadArbiterIn = Wire((new RFRdArbParams(backendParams.getRdCfgsIntSch[FpRD], backendParams.fpPregParams)).genInputBundle)
  private val fltRFReadArbiterIn = Wire((new RFRdArbParams(backendParams.getRdCfgsFltSch[FpRD], backendParams.fpPregParams)).genInputBundle)
  private val vecRFReadArbiterIn = Wire((new RFRdArbParams(backendParams.getRdCfgsVecSch[FpRD], backendParams.fpPregParams)).genInputBundle)
  intRFReadArbiterIn.lazyZip(fromIntIQ).lazyZip(in.fromIntRegion.fromIntIQDeqOg1Payload).foreach{ case (sink, source1, source2) =>
    sink.lazyZip(source1).lazyZip(source2).foreach{ case (ssink, ssource1, ssource2) =>
      ssink.zipWithIndex.map{ case (sssink, i) =>
        if (ssource1.bits.fpRen.nonEmpty){
          sssink.valid := ssource1.valid && ssource1.bits.fpRen.get(i)
        }
        else sssink.valid := false.B
        sssink.bits.issueValid := false.B
        sssink.bits.addr := ssource2.psrc(i)
        sssink.bits.robIdx := ssource1.bits.robIdx
        ssource1.ready := sssink.ready
      }
    }
  }
  vecRFReadArbiterIn := 0.U.asTypeOf(vecRFReadArbiterIn)
  fltRFReadArbiterIn.zipWithIndex.foreach { case (iq, iqIdx) =>
    iq.zipWithIndex.foreach { case (exu, exuIdx) =>
      exu.zipWithIndex.foreach { case (src, srcIdx) =>
        src.valid := issuePipes(iqIdx)(exuIdx).out.is0FpRdAddr(srcIdx).ren
        src.bits.issueValid := false.B
        src.bits.addr := issuePipes(iqIdx)(exuIdx).out.is0FpRdAddr(srcIdx).addr
        src.bits.robIdx := issuePipes(iqIdx)(exuIdx).out.is0FpRdAddr(srcIdx).robIdx
        // TODO
        src.bits.bankValidVec.foreach(x => x := 0.U.asTypeOf(x))
      }
    }
  }
  private val allFpRFReadArbiterIn = intRFReadArbiterIn ++ fltRFReadArbiterIn ++ vecRFReadArbiterIn
  private val fltRFReadArbiter = Module(new FpRFReadArbiter(backendParams))
  fltRFReadArbiter.io.in.zipWithIndex.foreach { case (arbInSeq2, iqIdx) =>
    arbInSeq2.zipWithIndex.foreach { case (arbInSeq, exuIdx) =>
      val srcIndices: Seq[Int] = FpRegSrcDataSet.flatMap(data => backendParams.allIssueParams(iqIdx).exuParams(exuIdx).getRfReadSrcIdx(data)).toSeq.sorted
      for (srcIdx <- 0 until backendParams.allIssueParams(iqIdx).exuParams(exuIdx).numRegSrc) {
        if (srcIndices.contains(srcIdx)) {
          println(s"fltRFReadArbiter.io.in isn't zero: iqIdx = $iqIdx, exuIdx = $exuIdx, srcIdx = $srcIdx")
          arbInSeq(srcIdx).valid := allFpRFReadArbiterIn(iqIdx)(exuIdx)(srcIdx).valid
          arbInSeq(srcIdx).bits.issueValid := false.B
          arbInSeq(srcIdx).bits.addr := allFpRFReadArbiterIn(iqIdx)(exuIdx)(srcIdx).bits.addr
          arbInSeq(srcIdx).bits.robIdx := allFpRFReadArbiterIn(iqIdx)(exuIdx)(srcIdx).bits.robIdx
          allFpRFReadArbiterIn(iqIdx)(exuIdx)(srcIdx).ready := arbInSeq(srcIdx).ready
        } else {
          println(s"fltRFReadArbiter.io.in is zero: iqIdx = $iqIdx, exuIdx = $exuIdx, srcIdx = $srcIdx")
          arbInSeq(srcIdx).valid := false.B
          arbInSeq(srcIdx).bits := 0.U.asTypeOf(arbInSeq(srcIdx).bits)
          allFpRFReadArbiterIn(iqIdx)(exuIdx)(srcIdx).ready := true.B
        }
      }
    }
  }
  fpRaddr := fltRFReadArbiter.io.out.map(_.bits.addr)

  out.toDispatch.IQValidNumVec := issueQueues.filterNot(_.param.hasVStd).map(_.out.validNum)
  out.toDispatch.debug.foreach(_.IQValidNumVec := issueQueues.filterNot(_.param.hasVStd).map(_.out.validNum))
  out.toDispatch.debug.foreach(_.IQEnqHasIssuedVec.foreach(_ := 0.U))
  for ((iq, i) <- issueQueues.filterNot(_.param.hasVStd).zipWithIndex) {
    out.toDispatch.canAccept(i).foreach(_ := iq.out.canAccept)
  }
  // TODO
  out.toDispatch.debugIQValidNumVec.foreach(_ := 0.U.asTypeOf(out.toDispatch.debugIQValidNumVec.get))
  out.toDispatch.debugIQEnqHasIssuedVec.foreach(_ := 0.U.asTypeOf(out.toDispatch.debugIQEnqHasIssuedVec.get))
  out.toDispatch.wakeUpFp := fpWbM2WakeupToDispatch
  out.toDispatch.wakeUpFpIs1Lat := fpWbM2WakeUpIs1Lat
  val wakeupF2I = Wire(new FltWakeUpBundle(backendParams.intPregParams))
  // TODO
  wakeupF2I := issuePipes.flatten.find(_.out.wakeupF2I.nonEmpty).get.out.wakeupF2I.get
  out.toIntRegion.wakeupF2I := wakeupF2I
  out.toIntRegion.intWbNext.flatten.zip(issuePipes.flatten.filter(_.out.gpWbNext.nonEmpty)).foreach { case (sink,source) =>
    sink := source.out.gpWbNext.get
  }
  out.toIntRegion.busyTableF2I := issuePipes.flatten.find(_.out.busyTableF2I.nonEmpty).get.out.busyTableF2I.get
  out.toIntRegion.fpRfRdataOut := fpRdata
  val fpExuOut = fpWbDataPath.in.fromExus(1)
  out.toIntRegion.fpExuOut.flatten.zip(fpExuOut.flatten).foreach { case (sink, source) =>
    sink := 0.U.asTypeOf(sink)
    sink.valid := source.wen
    sink.bits.toFpRf.get.valid := source.wen
    sink.bits.toFpRf.get.bits := source.data
    sink.bits.pdest := source.pdest
  }
  out.toIntRegion.og0CancelForStd := issuePipes.flatten.map(_.out.ex0RespFailLat1Next)
  out.toVecRegion.is0FpRdDataFail := 0.U.asTypeOf(out.toVecRegion.is0FpRdDataFail)
  out.toVecRegion.is1FpRdDataNext := 0.U.asTypeOf(out.toVecRegion.is1FpRdDataNext)
  out.toVecRegion.fpWbWakeUp := 0.U.asTypeOf(out.toVecRegion.fpWbWakeUp)
  out.toVecRegion.fpWb0 := fpWbDataPath.out.wb0.map(_.data)
  out.toRob.debugIQDeqRobIdxVec.foreach(_ := 0.U.asTypeOf(out.toRob.debugIQDeqRobIdxVec.get))
  out.toTopDownMod := 0.U.asTypeOf(out.toTopDownMod)
  out.fpWb := fpWbDataPath.out.wb0

  require(
    out.toRob.writeback.flatten.size == issuePipes.flatMap(_.map(_.out.robWbNext)).size,
    s"out.toRob.writeback: ${out.toRob.writeback.map(_.size)}" +
      s"issuePipes: ${issuePipes.map(_.map(_.out.robWbNext).size)}"
  )

  out.toRob.writeback.flatten zip issuePipes.flatMap(_.map(_.out.robWbNext)) foreach {
    case (sink: ValidIO[Exu.ToRob], source: ValidIO[Exu.ToRob]) => sink := source
  }

  for (i <- out.gpWbNext.indices) {
    out.gpWbNext(i) := issuePipes(i).map(_.out.gpWbNext).filter(_.nonEmpty).map(_.get)
    out.fpWbNext(i) := issuePipes(i).map(_.out.fpWbNext).filter(_.nonEmpty).map(_.get)
  }

  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    val difftest = DifftestModule(new DiffPhyFpRegState(numFp), delay = 2)
    difftest.coreid := in.fromTop.hartId
    difftest.value := fpDiffReadData.get
  }

  private def issueQueueWbSources(
    sinkSel: FltIssueQueue => Option[WbFuBusyTable.In],
    wbPortIdsSel: IssueParam => Seq[Int],
  ): Seq[WbFuBusyTable.Source] =
    WbFuBusyTable.issueQueueSources(issueQueues, sinkSel, wbPortIdsSel)

  private def nonFixedLatFuWbSources(
    wbPortSel: ExuParam => Option[Int],
    writeSel: VecFuConfig => Boolean,
  ): Seq[WbFuBusyTable.Source] =
    issuePipes.flatten.flatMap { pipe =>
      pipe.out.outFuLat.toSeq.flatMap { latencies =>
        latencies.zip(pipe.param.nonFixedLatFuConfigs).flatMap { case (latency, fuCfg) =>
          wbPortSel(pipe.param)
            .filter(_ => writeSel(fuCfg))
            .map(port => WbFuBusyTable.Source.NonFixedLatFu(port, latency.valid, latency.bits))
        }
      }
    }

  private def nonFixedLatFuPortIndices(
    wbPortIdsSel: IssueParam => Seq[Int],
    wbPortIdxMap: Map[Int, Int],
  ): Seq[Int] =
    param.issueParams.flatMap(wbPortIdsSel).distinct.flatMap(wbPortIdxMap.get)

  private def buildWbFuBusyTable(
    regfileWbPortIds: Seq[Int],
    nonFixedLatFuPortIndices: Seq[Int],
  ): Option[WbFuBusyTable] =
    Option.when(regfileWbPortIds.nonEmpty)(
      Module(new WbFuBusyTable(regfileWbPortIds.size, nonFixedLatFuPortIndices))
    )

  private def connectWbFuBusyTable(
    table: Option[WbFuBusyTable],
    wbType: String,
    regfileWbPortIds: Seq[Int],
    issueSources: Seq[WbFuBusyTable.Source],
    nonFixedLatFuSources: Seq[WbFuBusyTable.Source],
  ): Map[Int, UInt] =
    table.map { t =>
      t.in =#> WbFuBusyTable.ConnectInfo(wbType, regfileWbPortIds, issueSources ++ nonFixedLatFuSources)
      t.out.fuBusyTable.zipWithIndex.map(_.swap).toMap
    }.getOrElse(Map.empty)

  private def ctrlBlockRead(table: Option[WbFuBusyTable]): Map[Int, WbFuBusyTable.CtrlBlockEntry] =
    table.map(_.out.ctrlBlock.zipWithIndex.map(_.swap).toMap).getOrElse(Map.empty)

  private def connectBusyTableRead[T <: Data](
    sink: Option[Vec[T]],
    wbPortIds: Seq[Int],
    tableRead: Map[Int, T],
    wbPortIdxMap: Map[Int, Int],
  ): Unit = {
    sink.foreach(_.zip(wbPortIds).foreach { case (readPort, wbPortId) =>
      readPort := tableRead(wbPortIdxMap(wbPortId))
    })
  }

}

object FltRegionModule {
  class In(implicit p: Parameters, param: RegionParam) extends XSBundle {
    val intRegion = backendParams.getIntRegionParam
    val fltRegion = backendParams.getFltRegionParam

    val fpSchdParam = backendParams.fpSchdParams.get

    val fromTop = new Bundle {
      val hartId = UInt(8.W)
    }
    val flush = ValidIO(new Redirect)
    val fromDispatch = new Bundle {
      val uops: MixedVec[Vec[ValidIO[Enq]]] = MixedVec(
        param.issueParams.filterNot(_.hasVStd).map(x => Vec(x.numEnq, ValidIO(new Enq()(p, x))))
      )
    }
    val fromMem = new FromMem

    val fromIntRegion = new Bundle {
      val wakeupFromI2F = new WakeUpBundle(backendParams.fpPregParams)
      val fpWbNext: MixedVec[MixedVec[Exu.ToRf]] = intRegion.genExuToRfBundle(backendParams.fpPregParams)
      val fpWbM3Wakeup = Vec(backendParams.getIntRegionParam.getFpWriteSize, new FltWakeUpBundle(backendParams.fpPregParams))
      val busyTableI2F = Input(UInt(3.W))
      val fromIntIQDeqOg1Payload: MixedVec[MixedVec[IssueQueueDeqOg1Payload]] =
        Input(MixedVec(backendParams.schdParams(IntScheduler()).issueBlockParams.map(_.genIssueDeqOg1PayloadBundle)))
    }

    val fromVecRegion = new Bundle {
      val fromVecFpRdAddr = backendParams.getVecRegionParam.genRfRdAddrBundle(backendParams.fpPregParams)
      val fpWbNext: MixedVec[MixedVec[Exu.ToRf]] = backendParams.getVecRegionParam.genExuToRfBundle(backendParams.fpPregParams)
      val fpWbM3Wakeup = Vec(backendParams.getVecRegionParam.getFpWriteSize, new FltWakeUpBundle(backendParams.fpPregParams))
    }

    val fromCSR = new Bundle {
      val frm = Frm()
    }
  }

  class FromMem(implicit p: Parameters, param: RegionParam) extends XSBundle {
    val ldCancel = Vec(backendParams.LdExuCnt, Input(new LoadCancelIO))
  }

  class Out(implicit p: Parameters, param: RegionParam) extends XSBundle {
    private val intRegion = backendParams.getIntRegionParam

    val numDeq: Int = param.issueParams.map(_.numDeq).sum
    val numEntry: Int = param.issueParams.map(_.numEntry).max
    val numIQ: Int = param.issueParams.count(x => !x.hasVStd)

    val toDispatch = new Bundle {
      val IQValidNumVec: Vec[UInt] = Vec(
        param.issueParams.filterNot(_.hasVStd).map(_.numDeq).sum,
        UInt(numEntry.U.getWidth.W)
      )
      val IQNum = param.issueParams.size
      val maxIQSize = param.issueParams.map(_.numEntry).max
      val debugIQValidNumVec = Option.when(backendParams.debugEn)(Vec(IQNum, Output(UInt(maxIQSize.U.getWidth.W))))
      val debugIQEnqHasIssuedVec = Option.when(backendParams.debugEn)(Vec(IQNum, Output(Bool())))
      val wakeUpFp: Vec[FltWakeUpBundle] = Vec(backendParams.getFpWriteSize, new FltWakeUpBundle(backendParams.fpPregParams))
      val wakeUpFpIs1Lat: Vec[Bool] = Vec(backendParams.getFpWriteSize, Bool())

      val canAccept: MixedVec[Vec[Bool]] = MixedVec(
        param.issueParams.filterNot(_.hasVStd).map(x => Flipped(Vec(x.numEnq, Bool())))
      )

      val debug = Option.when(backendParams.debugEn)(new Bundle {
        val IQValidNumVec = Vec(numIQ, UInt(numEntry.U.getWidth.W))
        val IQEnqHasIssuedVec = Vec(numIQ, Output(Bool()))
      })
    }

    val toIntRegion = new Bundle {
      val wakeupF2I = new FltWakeUpBundle(backendParams.intPregParams)
      val intWbNext: MixedVec[MixedVec[Exu.ToRf]] = param.genExuToRfBundle(backendParams.intPregParams)
      val busyTableF2I = UInt(3.W)
      val fpRfRdataOut = Vec(backendParams.numPregRd(FpData()), UInt(backendParams.fpSchdParams.get.rfDataWidth.W))
      val fpExuOut = backendParams.fpSchdParams.get.genNewExuOutputValidBundle
      val og0CancelForStd = Vec(backendParams.getFltRegionParam.getFpWriteSize, Bool())
    }

    val toVecRegion = new Bundle {
      val is0FpRdDataFail: MixedVec[MixedVec[Vec[Bool]]] = backendParams.getVecRegionParam.genRfRdFailBundle(backendParams.fpPregParams)
      val is1FpRdDataNext: MixedVec[MixedVec[MixedVec[IssuePipe.RfReadDataBundle]]] = backendParams.getVecRegionParam.genRfRdDataBundle(backendParams.fpPregParams)
      val fpWbWakeUp = Vec(backendParams.getFpRfWriteSize, new WakeUpBundle(backendParams.fpPregParams))
      val fpWb0 = Vec(backendParams.getFpRfWriteSize, UInt(XLEN.W))
    }

    val toRob = new Bundle {
      val writeback: MixedVec[MixedVec[ValidIO[Exu.ToRob]]] = param.genExuToRobBundle(ValidIO(_))
      val iqDeqSum = param.issueParams.map(_.numDeq).sum
      val debugIQDeqRobIdxVec = Option.when(backendParams.debugEn)(Vec(iqDeqSum, ValidIO(new RobPtr())))
    }
    val toTopDownMod = new Bundle {
      val uopTopDown = new UopTopDown
    }
    val gpWbNext: MixedVec[MixedVec[Exu.ToRf]] = param.genExuToRfBundle(backendParams.intPregParams)
    val fpWbNext: MixedVec[MixedVec[Exu.ToRf]] = param.genExuToRfBundle(backendParams.fpPregParams)

    /**
     * [[fpWb]] is after WbDataPath while [[fpWbNext]] is before WbDataPath
     */

    val fpWb: Vec[RfWriteBundle] = Vec(
      backendParams.getRfWriteSize(backendParams.fpPregParams.dataCfg),
      new RfWriteBundle(backendParams.fpPregParams)
    )
  }

  class RfArbiterBundle(val rdConfig: RdConfig)(implicit p: Parameters) extends XSBundle {
    val ren = Bool()
    val addr = UInt(PhyRegIdxWidth.W)
    val robIdx = new RobPtr()
  }

  object RfArbiterBundle {
    def apply(rdConfig: RdConfig)(ren: Bool, addr: UInt, robIdx: RobPtr)(implicit p: Parameters): RfArbiterBundle = {
      val rfArbiterBundle = Wire(new RfArbiterBundle(rdConfig))
      rfArbiterBundle.ren := ren
      rfArbiterBundle.addr := addr
      rfArbiterBundle.robIdx := robIdx
      rfArbiterBundle
    }
  }

  class DebugBundle(implicit p: Parameters) extends XSBundle {
    val debug          = new xiangshan.DebugBundle
    val perfDebugInfo  = new PerfDebugInfo()
    val seqNum         = InstSeqNum()
  }
}
