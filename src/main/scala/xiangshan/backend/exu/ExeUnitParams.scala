package xiangshan.backend.exu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.{ExuBypassBundle, ExuInput, ExuOutput}
import xiangshan.backend.datapath.DataConfig.DataConfig
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.datapath.{DataConfig, WakeUpConfig}
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.issue.{IssueBlockParams, SchedulerType, IntScheduler, VfScheduler, MemScheduler}
import scala.collection.mutable

case class ExeUnitParams(
  name          : String,
  fuConfigs     : Seq[FuConfig],
  wbPortConfigs : Seq[PregWB],
  rfrPortConfigs: Seq[Seq[RdConfig]],
  copyWakeupOut: Boolean = false,
  copyDistance: Int = 1,
  fakeUnit      : Boolean = false,
)(
  implicit
  val schdType: SchedulerType,
) {
  // calculated configs
  var iqWakeUpSourcePairs: Seq[WakeUpConfig] = Seq()
  var iqWakeUpSinkPairs: Seq[WakeUpConfig] = Seq()
  // used in bypass to select data of exu output
  var exuIdx: Int = -1
  var backendParam: BackendParams = null

  val numIntSrc: Int = fuConfigs.map(_.numIntSrc).max
  val numFpSrc: Int = fuConfigs.map(_.numFpSrc).max
  val numVecSrc: Int = fuConfigs.map(_.numVecSrc).max
  val numVfSrc: Int = fuConfigs.map(_.numVfSrc).max
  val numV0Src: Int = fuConfigs.map(_.numV0Src).max
  val numVlSrc: Int = fuConfigs.map(_.numVlSrc).max
  val numRegSrc: Int = fuConfigs.map(_.numRegSrc).max
  val numSrc: Int = fuConfigs.map(_.numSrc).max
  val destDataBitsMax: Int = fuConfigs.map(_.destDataBits).max
  val srcDataBitsMax: Int = fuConfigs.map(x => x.srcDataBits.getOrElse(x.destDataBits)).max
  val readIntRf: Boolean = numIntSrc > 0
  val readFpRf: Boolean = numFpSrc > 0
  val readVecRf: Boolean = numVecSrc > 0
  val readVfRf: Boolean = numVfSrc > 0
  val writeIntRf: Boolean = fuConfigs.map(_.writeIntRf).reduce(_ || _)
  val writeFpRf: Boolean = fuConfigs.map(_.writeFpRf).reduce(_ || _)
  val writeVecRf: Boolean = fuConfigs.map(_.writeVecRf).reduce(_ || _)
  val writeV0Rf: Boolean = fuConfigs.map(_.writeV0Rf).reduce(_ || _)
  val writeVlRf: Boolean = fuConfigs.map(_.writeVlRf).reduce(_ || _)
  val needIntWen: Boolean = fuConfigs.map(_.needIntWen).reduce(_ || _)
  val needFpWen: Boolean = fuConfigs.map(_.needFpWen).reduce(_ || _)
  val needVecWen: Boolean = fuConfigs.map(_.needVecWen).reduce(_ || _)
  val needV0Wen: Boolean = fuConfigs.map(_.needV0Wen).reduce(_ || _)
  val needVlWen: Boolean = fuConfigs.map(_.needVlWen).reduce(_ || _)
  val needOg2: Boolean = fuConfigs.map(_.needOg2).reduce(_ || _)
  val writeVfRf: Boolean = writeVecRf
  val writeFflags: Boolean = fuConfigs.map(_.writeFflags).reduce(_ || _)
  val writeVxsat: Boolean = fuConfigs.map(_.writeVxsat).reduce(_ || _)
  val hasNoDataWB: Boolean = fuConfigs.map(_.hasNoDataWB).reduce(_ && _)
  val hasRedirect: Boolean = fuConfigs.map(_.hasRedirect).reduce(_ || _)
  val hasPredecode: Boolean = fuConfigs.map(_.hasPredecode).reduce(_ || _)
  val exceptionOut: Seq[Int] = fuConfigs.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted
  val hasLoadError: Boolean = fuConfigs.map(_.hasLoadError).reduce(_ || _)
  val flushPipe: Boolean = fuConfigs.map(_.flushPipe).reduce(_ || _)
  val replayInst: Boolean = fuConfigs.map(_.replayInst).reduce(_ || _)
  val trigger: Boolean = fuConfigs.map(_.trigger).reduce(_ || _)
  val needExceptionGen: Boolean = exceptionOut.nonEmpty || flushPipe || replayInst || trigger
  val needPc: Boolean = fuConfigs.map(_.needPc).reduce(_ || _)
  val needTarget: Boolean = fuConfigs.map(_.needTargetPc).reduce(_ || _)
  val needPdInfo: Boolean = fuConfigs.map(_.needPdInfo).reduce(_ || _)
  val needSrcFrm: Boolean = fuConfigs.map(_.needSrcFrm).reduce(_ || _)
  val needSrcVxrm: Boolean = fuConfigs.map(_.needSrcVxrm).reduce(_ || _)
  val needFPUCtrl: Boolean = fuConfigs.map(_.needFPUCtrl).reduce(_ || _)
  val needVPUCtrl: Boolean = fuConfigs.map(_.needVecCtrl).reduce(_ || _)
  val writeVConfig: Boolean = fuConfigs.map(_.writeVlRf).reduce(_ || _)
  val writeVType: Boolean = fuConfigs.map(_.writeVType).reduce(_ || _)
  val isHighestWBPriority: Boolean = wbPortConfigs.forall(_.priority == 0)

  val isIntExeUnit: Boolean = schdType.isInstanceOf[IntScheduler]
  val isVfExeUnit: Boolean = schdType.isInstanceOf[VfScheduler]
  val isMemExeUnit: Boolean = schdType.isInstanceOf[MemScheduler]

  val needReadRegCache: Boolean = isIntExeUnit || isMemExeUnit && readIntRf

  // exu writeback: 0 normalout; 1 intout; 2 fpout; 3 vecout
  val wbNeedIntWen : Boolean = writeIntRf && !isMemExeUnit
  val wbNeedFpWen  : Boolean = writeFpRf  && !isMemExeUnit
  val wbNeedVecWen : Boolean = writeVecRf && !isMemExeUnit
  val wbNeedV0Wen  : Boolean = writeV0Rf  && !isMemExeUnit
  val wbNeedVlWen  : Boolean = writeVlRf  && !isMemExeUnit
  val wbPathNum: Int = Seq(wbNeedIntWen, wbNeedFpWen, wbNeedVecWen, wbNeedV0Wen, wbNeedVlWen).count(_ == true) + 1
  val wbNeeds = Seq(
    ("int", wbNeedIntWen),
    ("fp", wbNeedFpWen),
    ("vec", wbNeedVecWen),
    ("v0", wbNeedV0Wen),
    ("vl", wbNeedVlWen)
  )
  val wbIndexeds = wbNeeds.filter(_._2).zipWithIndex.map {
    case ((label, _), index) => (label, index + 1)
  }.toMap
  val wbIntIndex: Int = wbIndexeds.getOrElse("int", 0)
  val wbFpIndex : Int = wbIndexeds.getOrElse("fp",  0)
  val wbVecIndex: Int = wbIndexeds.getOrElse("vec", 0)
  val wbV0Index : Int = wbIndexeds.getOrElse("v0" , 0)
  val wbVlIndex : Int = wbIndexeds.getOrElse("vl" , 0)
  val wbIndex: Seq[Int] = Seq(wbIntIndex, wbFpIndex, wbVecIndex, wbV0Index, wbVlIndex)



  require(needPc && needTarget || !needPc && !needTarget, "The ExeUnit must need both PC and Target PC")

  def copyNum: Int = {
    val setIQ = mutable.Set[IssueBlockParams]()
    iqWakeUpSourcePairs.map(_.sink).foreach{ wakeupSink =>
      backendParam.allIssueParams.map{ issueParams =>
        if (issueParams.exuBlockParams.contains(wakeupSink.getExuParam(backendParam.allExuParams))) {
          setIQ.add(issueParams)
        }
      }
    }
    println(s"[Backend] exuIdx ${exuIdx} numWakeupIQ ${setIQ.size}")
    1 + setIQ.size / copyDistance
  }
  def rdPregIdxWidth: Int = {
    this.pregRdDataCfgSet.map(dataCfg => backendParam.getPregParams(dataCfg).addrWidth).fold(0)(_ max _)
  }

  def wbPregIdxWidth: Int = {
    this.pregWbDataCfgSet.map(dataCfg => backendParam.getPregParams(dataCfg).addrWidth).fold(0)(_ max _)
  }

  val writeIntFuConfigs: Seq[FuConfig] = fuConfigs.filter(x => x.writeIntRf)
  val writeFpFuConfigs: Seq[FuConfig] = fuConfigs.filter(x => x.writeFpRf)
  val writeVfFuConfigs: Seq[FuConfig] = fuConfigs.filter(x => x.writeVecRf)
  val writeV0FuConfigs: Seq[FuConfig] = fuConfigs.filter(x => x.writeV0Rf)
  val writeVlFuConfigs: Seq[FuConfig] = fuConfigs.filter(x => x.writeVlRf)

  /**
    * Check if this exu has certain latency
    */
  def latencyCertain: Boolean = fuConfigs.map(x => x.latency.latencyVal.nonEmpty).reduce(_ && _)
  def intLatencyCertain: Boolean = writeIntFuConfigs.forall(x => x.latency.latencyVal.nonEmpty)
  def fpLatencyCertain: Boolean = writeFpFuConfigs.forall(x => x.latency.latencyVal.nonEmpty)
  def vfLatencyCertain: Boolean = writeVfFuConfigs.forall(x => x.latency.latencyVal.nonEmpty)
  def v0LatencyCertain: Boolean = writeV0FuConfigs.forall(x => x.latency.latencyVal.nonEmpty)
  def vlLatencyCertain: Boolean = writeVlFuConfigs.forall(x => x.latency.latencyVal.nonEmpty)
  // only load use it
  def hasUncertainLatencyVal: Boolean = fuConfigs.map(x => x.latency.uncertainLatencyVal.nonEmpty).reduce(_ || _)

  /**
    * Get mapping from FuType to Latency value.
    * If both [[latencyCertain]] and [[hasUncertainLatencyVal]] are false, get empty [[Map]]
    *
    * @return Map[ [[BigInt]], Latency]
    */
  def fuLatencyMap: Map[FuType.OHType, Int] = {
    if (latencyCertain)
      if(needOg2) fuConfigs.map(x => (x.fuType, x.latency.latencyVal.get + 1)).toMap else fuConfigs.map(x => (x.fuType, x.latency.latencyVal.get)).toMap
    else if (hasUncertainLatencyVal)
      fuConfigs.map(x => (x.fuType, x.latency.uncertainLatencyVal)).toMap.filter(_._2.nonEmpty).map(x => (x._1, x._2.get))
    else
      Map()
  }
  def wakeUpFuLatencyMap: Map[FuType.OHType, Int] = {
    if (latencyCertain)
      fuConfigs.filterNot(_.hasNoDataWB).map(x => (x.fuType, x.latency.latencyVal.get)).toMap
    else if (hasUncertainLatencyVal)
      fuConfigs.filterNot(_.hasNoDataWB).map(x => (x.fuType, x.latency.uncertainLatencyVal.get)).toMap
    else
      Map()
  }

  /**
    * Get set of latency of function units.
    * If both [[latencyCertain]] and [[hasUncertainLatencyVal]] are false, get empty [[Set]]
    *
    * @return Set[Latency]
    */
  def fuLatancySet: Set[Int] = fuLatencyMap.values.toSet

  def wakeUpFuLatancySet: Set[Int] = wakeUpFuLatencyMap.values.toSet

  def latencyValMax: Int = fuLatancySet.fold(0)(_ max _)

  def intFuLatencyMap: Map[FuType.OHType, Int] = {
    if (intLatencyCertain) {
      if (isVfExeUnit) {
        // vf exe unit writing back to int regfile should delay 1 cycle
        // vf exe unit need og2 --> delay 1 cycle
        writeIntFuConfigs.map(x => (x.fuType, x.latency.latencyVal.get + 2)).toMap
      } else {
        writeIntFuConfigs.map(x => (x.fuType, x.latency.latencyVal.get)).toMap
      }
    }
    else
      Map()
  }

  def intLatencyValMax: Int = intFuLatencyMap.values.fold(0)(_ max _)

  def fpFuLatencyMap: Map[FuType.OHType, Int] = {
    if (fpLatencyCertain)
      writeFpFuConfigs.map(x => (x.fuType, x.latency.latencyVal.get)).toMap
    else
      Map()
  }

  def fpLatencyValMax: Int = fpFuLatencyMap.values.fold(0)(_ max _)

  def vfFuLatencyMap: Map[FuType.OHType, Int] = {
    if (vfLatencyCertain)
      if(needOg2) writeVfFuConfigs.map(x => (x.fuType, x.latency.latencyVal.get + 1)).toMap else writeVfFuConfigs.map(x => (x.fuType, x.latency.latencyVal.get)).toMap
    else
      Map()
  }

  def vfLatencyValMax: Int = vfFuLatencyMap.values.fold(0)(_ max _)

  def v0FuLatencyMap: Map[FuType.OHType, Int] = {
    if (v0LatencyCertain)
      if(needOg2) writeV0FuConfigs.map(x => (x.fuType, x.latency.latencyVal.get + 1)).toMap else writeV0FuConfigs.map(x => (x.fuType, x.latency.latencyVal.get)).toMap
    else
      Map()
  }

  def v0LatencyValMax: Int = v0FuLatencyMap.values.fold(0)(_ max _)

  def vlFuLatencyMap: Map[FuType.OHType, Int] = {
    if (vlLatencyCertain)
      if(needOg2) writeVlFuConfigs.map(x => (x.fuType, x.latency.latencyVal.get + 1)).toMap else writeVlFuConfigs.map(x => (x.fuType, x.latency.latencyVal.get)).toMap
    else
      Map()
  }

  def vlLatencyValMax: Int = vlFuLatencyMap.values.fold(0)(_ max _)

  /**
    * Check if this exu has fixed latency
    */
  def isFixedLatency: Boolean = {
    if (latencyCertain)
      return fuConfigs.map(x => x.latency.latencyVal.get == fuConfigs.head.latency.latencyVal.get).reduce(_ && _)
    false
  }

  def hasCSR: Boolean = fuConfigs.map(_.isCsr).reduce(_ || _)

  def hasFence: Boolean = fuConfigs.map(_.isFence).reduce(_ || _)

  def hasBrhFu = fuConfigs.map(_.fuType == FuType.brh).reduce(_ || _)

  def hasi2vFu = fuConfigs.map(_.fuType == FuType.i2v).reduce(_ || _)

  def hasJmpFu = fuConfigs.map(_.fuType == FuType.jmp).reduce(_ || _)

  def hasLoadFu = fuConfigs.map(_.name == "ldu").reduce(_ || _)

  def hasVLoadFu = fuConfigs.map(_.fuType == FuType.vldu).reduce(_ || _)

  def hasVStoreFu = fuConfigs.map(_.fuType == FuType.vstu).reduce(_ || _)

  def hasVecLsFu = fuConfigs.map(x => FuType.FuTypeOrR(x.fuType, Seq(FuType.vldu, FuType.vstu))).reduce(_ || _)

  def hasStoreAddrFu = fuConfigs.map(_.name == "sta").reduce(_ || _)

  def hasStdFu = fuConfigs.map(_.name == "std").reduce(_ || _)

  def hasMemAddrFu = hasLoadFu || hasStoreAddrFu || hasVLoadFu || hasHyldaFu || hasHystaFu || hasVLoadFu || hasVStoreFu

  def hasHyldaFu = fuConfigs.map(_.name == "hylda").reduce(_ || _)

  def hasHystaFu = fuConfigs.map(_.name == "hysta").reduce(_ || _)

  def hasLoadExu = hasLoadFu || hasHyldaFu

  def hasStoreAddrExu = hasStoreAddrFu || hasHystaFu

  def hasVecFu = fuConfigs.map(x => FuConfig.VecArithFuConfigs.contains(x)).reduce(_ || _)

  def CanCompress = !hasBrhFu || (hasBrhFu && hasi2vFu)

  def getSrcDataType(srcIdx: Int): Set[DataConfig] = {
    fuConfigs.map(_.getSrcDataType(srcIdx)).reduce(_ ++ _)
  }

  def immType: Set[UInt] = fuConfigs.map(x => x.immType).reduce(_ ++ _)

  def getWBSource: SchedulerType = {
    schdType
  }

  def hasCrossWb: Boolean = {
    schdType match {
      case IntScheduler() => writeFpRf || writeVecRf
      case VfScheduler() => writeIntRf
      case _ => false
    }
  }

  def canAccept(fuType: UInt): Bool = {
    Cat(fuConfigs.map(_.fuType.U === fuType)).orR
  }

  def hasUncertainLatency: Boolean = fuConfigs.map(_.latency.latencyVal.isEmpty).reduce(_ || _)

  def bindBackendParam(param: BackendParams): Unit = {
    backendParam = param
  }

  def updateIQWakeUpConfigs(cfgs: Seq[WakeUpConfig]) = {
    this.iqWakeUpSourcePairs = cfgs.filter(_.source.name == this.name)
    this.iqWakeUpSinkPairs = cfgs.filter(_.sink.name == this.name)
    if (this.isIQWakeUpSource) {
      require(!this.hasUncertainLatency || hasLoadFu || hasHyldaFu, s"${this.name} is a not-LDU IQ wake up source , but has UncertainLatency")
    }
  }

  def updateExuIdx(idx: Int): Unit = {
    this.exuIdx = idx
  }

  def isIQWakeUpSource = this.iqWakeUpSourcePairs.nonEmpty

  def isIQWakeUpSink = this.iqWakeUpSinkPairs.nonEmpty

  def getIntWBPort = {
    wbPortConfigs.collectFirst {
      case x: IntWB => x
    }
  }

  def getFpWBPort = {
    wbPortConfigs.collectFirst {
      case x: FpWB => x
    }
  }

  def getVfWBPort = {
    wbPortConfigs.collectFirst {
      case x: VfWB => x
    }
  }

  def getV0WBPort = {
    wbPortConfigs.collectFirst {
      case x: V0WB => x
    }
  }

  def getVlWBPort = {
    wbPortConfigs.collectFirst {
      case x: VlWB => x
    }
  }

  /**
    * Get the [[DataConfig]] that this exu need to read
    */
  def pregRdDataCfgSet: Set[DataConfig] = {
    this.rfrPortConfigs.flatten.map(_.getDataConfig).toSet
  }

  /**
    * Get the [[DataConfig]] that this exu need to write
    */
  def pregWbDataCfgSet: Set[DataConfig] = {
    this.wbPortConfigs.map(_.dataCfg).toSet
  }

  def getRfReadDataCfgSet: Seq[Set[DataConfig]] = {
    val fuSrcsCfgSet: Seq[Seq[Set[DataConfig]]] = fuConfigs.map(_.getRfReadDataCfgSet)
    val alignedFuSrcsCfgSet: Seq[Seq[Set[DataConfig]]] = fuSrcsCfgSet.map(x => x ++ Seq.fill(numRegSrc - x.length)(Set[DataConfig]()))

    val exuSrcsCfgSet = alignedFuSrcsCfgSet.reduce((x, y) => (x zip y).map { case (cfg1, cfg2) => cfg1 union cfg2 })

    exuSrcsCfgSet
  }

  /**
    * Get the [[DataConfig]] mapped indices of source data of exu
    *
    * @example
    * {{{
    *   fuCfg.srcData = Seq(VecData(), VecData(), VecData(), V0Data(), VlData())
    *   getRfReadSrcIdx(VecData()) = Seq(0, 1, 2)
    *   getRfReadSrcIdx(V0Data()) = Seq(3)
    *   getRfReadSrcIdx(VlData()) = Seq(4)
    * }}}
    * @return Map[DataConfig -> Seq[indices]]
    */
  def getRfReadSrcIdx: Map[DataConfig, Seq[Int]] = {
    val dataCfgs = DataConfig.RegSrcDataSet
    val rfRdDataCfgSet = this.getRfReadDataCfgSet
    dataCfgs.toSeq.map { cfg =>
      (
        cfg,
        rfRdDataCfgSet.zipWithIndex.map { case (set, srcIdx) =>
          if (set.contains(cfg))
            Option(srcIdx)
          else
            None
        }.filter(_.nonEmpty).map(_.get)
      )
    }.toMap
  }

  def genExuModule(implicit p: Parameters): ExeUnit = {
    new ExeUnit(this)
  }

  def genExuInputBundle(implicit p: Parameters): ExuInput = {
    new ExuInput(this)
  }

  def genExuOutputBundle(implicit p: Parameters): ExuOutput = {
    new ExuOutput(this)
  }

  def genExuBypassBundle(implicit p: Parameters): ExuBypassBundle = {
    new ExuBypassBundle(this)
  }
}
