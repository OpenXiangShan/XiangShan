package xiangshan.backend.exu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.Bundles.{ExuInput, ExuOutput}
import xiangshan.backend.datapath.DataConfig.DataConfig
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.datapath.WbConfig.{VfWB, IntWB, WbConfig}
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.issue.{IntScheduler, SchedulerType, VfScheduler}

case class ExeUnitParams(
  fuConfigs     : Seq[FuConfig],
  wbPortConfigs : Seq[WbConfig],
  rfrPortConfigs: Seq[Seq[RdConfig]],
)(
  implicit
  val schdType: SchedulerType,
) {
  val numIntSrc: Int = fuConfigs.map(_.numIntSrc).max
  val numFpSrc: Int = fuConfigs.map(_.numFpSrc).max
  val numVecSrc: Int = fuConfigs.map(_.numVecSrc).max
  val numVfSrc: Int = fuConfigs.map(_.numVfSrc).max
  val numRegSrc: Int = fuConfigs.map(_.numRegSrc).max
  val numSrc: Int = fuConfigs.map(_.numSrc).max
  val dataBitsMax: Int = fuConfigs.map(_.dataBits).max
  val readIntRf: Boolean = numIntSrc > 0
  val readFpRf: Boolean = numFpSrc > 0
  val readVecRf: Boolean = numVecSrc > 0
  val writeIntRf: Boolean = fuConfigs.map(_.writeIntRf).reduce(_ || _)
  val writeFpRf: Boolean = fuConfigs.map(_.writeFpRf).reduce(_ || _)
  val writeVecRf: Boolean = fuConfigs.map(_.writeVecRf).reduce(_ || _)
  val writeVfRf: Boolean = writeFpRf || writeVecRf
  val writeFflags: Boolean = fuConfigs.map(_.writeFflags).reduce(_ || _)
  val hasNoDataWB: Boolean = fuConfigs.map(_.hasNoDataWB).reduce(_ || _)
  val hasRedirect: Boolean = fuConfigs.map(_.hasRedirect).reduce(_ || _)
  val hasPredecode: Boolean = fuConfigs.map(_.hasPredecode).reduce(_ || _)
  val exceptionOut: Seq[Int] = fuConfigs.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted
  val hasLoadError: Boolean = fuConfigs.map(_.hasLoadError).reduce(_ || _)
  val flushPipe: Boolean = fuConfigs.map(_.flushPipe).reduce(_ || _)
  val replayInst: Boolean = fuConfigs.map(_.replayInst).reduce(_ || _)
  val trigger: Boolean = fuConfigs.map(_.trigger).reduce(_ || _)
  val needExceptionGen: Boolean = exceptionOut.nonEmpty || flushPipe || replayInst || trigger
  val needPc: Boolean = fuConfigs.map(_.needPc).reduce(_ || _)
  val needSrcFrm: Boolean = fuConfigs.map(_.needSrcFrm).reduce(_ || _)
  val needFPUCtrl: Boolean = fuConfigs.map(_.needFPUCtrl).reduce(_ || _)
  val wbPregIdxWidth = if (wbPortConfigs.nonEmpty) wbPortConfigs.map(_.pregIdxWidth).max else 0

  protected val latencyCertain = fuConfigs.map(x => x.latency.latencyVal.nonEmpty).reduce(_&&_)
  val fuLatencyMap = if (latencyCertain) Some(fuConfigs.map(y => (y.fuType, y.latency.latencyVal.get))) else None
  val latencyValMax = fuLatencyMap.map(x => x.map(_._2).max)

  def hasCSR: Boolean = fuConfigs.map(_.isCsr).reduce(_ || _)

  def hasFence: Boolean = fuConfigs.map(_.isFence).reduce(_ || _)

  def hasBrhFu = fuConfigs.map(_.fuType == FuType.brh).reduce(_ || _)

  def hasJmpFu = fuConfigs.map(_.fuType == FuType.jmp).reduce(_ || _)

  def hasLoadFu = fuConfigs.map(_.fuType == FuType.ldu).reduce(_ || _)

  def hasStoreAddrFu = fuConfigs.map(_.name == "sta").reduce(_ || _)

  def hasStdFu = fuConfigs.map(_.name == "std").reduce(_ || _)

  def hasMemAddrFu = hasLoadFu || hasStoreAddrFu

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

  def getIntWBPort = {
    wbPortConfigs.collectFirst {
      case x: IntWB => x
    }
  }

  def getVfWBPort = {
    wbPortConfigs.collectFirst {
      case x: VfWB => x
    }
  }

  def getRfReadDataCfgSet: Seq[Set[DataConfig]] = {
    val fuSrcsCfgSet: Seq[Seq[Set[DataConfig]]] = fuConfigs.map(_.getRfReadDataCfgSet)
    val alignedFuSrcsCfgSet: Seq[Seq[Set[DataConfig]]] = fuSrcsCfgSet.map(x => x ++ Seq.fill(numRegSrc - x.length)(Set[DataConfig]()))

    val exuSrcsCfgSet = alignedFuSrcsCfgSet.reduce((x, y) => (x zip y).map { case (cfg1, cfg2) => cfg1 union cfg2 })

    exuSrcsCfgSet
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
}
