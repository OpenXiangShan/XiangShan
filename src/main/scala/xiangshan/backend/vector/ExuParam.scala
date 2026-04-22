package xiangshan.backend.vector

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util.MixedVec
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.BackendParams
import xiangshan.backend.datapath.DataConfig
import xiangshan.backend.datapath.DataConfig.DataConfig
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.decode.Imm
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.regfile._
import xiangshan.backend.vector.IssuePipe.{RfReadAddrBundle, RfReadDataBundle}
import xiangshan.backend.vector.fu.VecFuConfig

import scala.beans.BeanProperty

class ExuParam(
  val name         : String,
  val fuConfigs    : Seq[VecFuConfig],
  val writePortCfgs: Seq[PregWB],
  val readPortCfgs : Seq[Seq[RdConfig]],
  val v0RD         : V0RD = null,
  val v0WB         : V0WB = null,
  val vlRD         : VlRD = null,
  val vlWB         : VlWB = null,
) {
  @BeanProperty
  var issueParam: IssueParam = _

  @BeanProperty
  var backendParams: BackendParams = _

  def getRegionParam: RegionParam = issueParam.getRegionParam()

  require(readPortCfgs.forall(!_.exists(_.isInstanceOf[V0RD])), "V0RD should not appear in rfrPortConfigs")
  require(readPortCfgs.forall(!_.exists(_.isInstanceOf[VlRD])), "VlRD should not appear in rfrPortConfigs")
  require(!writePortCfgs.exists(_.isInstanceOf[V0WB]), "V0WB should not appear in wbPortConfigs")
  require(!writePortCfgs.exists(_.isInstanceOf[VlWB]), "VlWB should not appear in wbPortConfigs")

  val numRegSrc: Int = fuConfigs.map(_.numRegSrc).max
  val numGpSrc: Int = fuConfigs.map(_.numIntSrc).max
  val numFpSrc: Int = fuConfigs.map(_.numFpSrc).max
  val numVpSrc: Int = fuConfigs.map(_.numVecSrc).max
  val numSrc: Int = fuConfigs.map(_.numSrc).max

  val destDataBitsMax: Int = fuConfigs.map(_.destDataBits).max
  val srcDataBitsMax: Int = fuConfigs.map(x => x.srcDataBits.getOrElse(x.destDataBits)).max
  val readGpRf: Boolean = numGpSrc > 0
  val readFpRf: Boolean = numFpSrc > 0
  val readVpRf: Boolean = numVpSrc > 0
  val readV0Rf: Boolean = fuConfigs.exists(_.readV0)
  val readVlRf: Boolean = fuConfigs.exists(_.readVl)
  val readFrm: Boolean = fuConfigs.exists(_.needSrcFrm)
  val readVxrm: Boolean = fuConfigs.exists(_.needSrcVxrm)
  val readVType: Boolean = fuConfigs.exists(_.readVType)
  val readOldVType: Boolean = fuConfigs.exists(_.writeVType)

  val needVM: Boolean = this.readV0Rf

  val needGpWen: Boolean = fuConfigs.exists(_.needIntWen)
  val needFpWen: Boolean = fuConfigs.exists(_.needFpWen)
  val needVpWen: Boolean = fuConfigs.exists(_.needVecWen)
  val needV0Wen: Boolean = fuConfigs.exists(_.needV0Wen)
  val needVlWen: Boolean = fuConfigs.exists(_.needVlWen)

  val needFFlagsWen: Boolean = fuConfigs.exists(_.writeFflags)
  val needVxsatWen: Boolean = fuConfigs.exists(_.writeVxsat)
  val needVTypeWen = fuConfigs.exists(_.writeVType)

  val needRedirect: Boolean = fuConfigs.exists(_.hasRedirect)
  val needIsRVC: Boolean = fuConfigs.exists(_.hasIsRVC)
  val needRasAction: Boolean = fuConfigs.exists(_.hasRasAction)
  val needFlushPipe: Boolean = fuConfigs.exists(_.flushPipe)
  val needReplay: Boolean = fuConfigs.exists(_.replayInst)
  val needTrigger: Boolean = fuConfigs.exists(_.trigger)
  val needPc: Boolean = fuConfigs.exists(_.needPc)
  val exceptionOut: Seq[Int] = fuConfigs.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted
  val needExceptionGen: Boolean = exceptionOut.nonEmpty || needFlushPipe || needReplay || needTrigger

  val needSqIdx = this.hasSta || this.hasLdu || this.hasStd || this.hasVStd

  val needTarget: Boolean = fuConfigs.exists(_.needTargetPc)
  val needPdInfo: Boolean = fuConfigs.exists(_.needPdInfo)
  val needCriticalErrors: Boolean = fuConfigs.exists(_.needCriticalErrors)

  def isIntExeUnit: Boolean = getRegionParam.region == VecRegion && (name.contains("ALU") || name.contains("BJU"))
  def isFltExeUnit: Boolean = getRegionParam.region == FltRegion
  def isVecExeUnit: Boolean = getRegionParam.region == VecRegion
  def isMemExeUnit: Boolean = getRegionParam.region == VecRegion && !name.contains("ALU") && !name.contains("BJU")

  def writeGpFuConfigs: Seq[VecFuConfig] = fuConfigs.filter(_.writeIntRf)
  def writeFpFuConfigs: Seq[VecFuConfig] = fuConfigs.filter(_.writeFpRf)
  def writeVpFuConfigs: Seq[VecFuConfig] = fuConfigs.filter(_.writeVecRf)
  def writeV0FuConfigs: Seq[VecFuConfig] = fuConfigs.filter(_.writeV0Rf)
  def writeVlFuConfigs: Seq[VecFuConfig] = fuConfigs.filter(_.writeVlRf)

  def getGpWriteCfg: Option[IntWB] = {
    writePortCfgs.collectFirst {
      case x: IntWB => x
    }
  }

  def getFpWriteCfg: Option[FpWB] = {
    writePortCfgs.collectFirst {
      case x: FpWB => x
    }
  }

  def getVpWriteCfg: Option[VfWB] = {
    writePortCfgs.collectFirst {
      case x: VfWB => x
    }
  }

  def getV0WriteCfg: Option[V0WB] = {
    Option(v0WB)
  }

  def getVlWriteCfg: Option[VlWB] = {
    Option(vlWB)
  }

  /**
   * Get the [[DataConfig]] that this exu need to read
   */
  def pregRdDataCfgSet: Set[DataConfig] = {
    this.readPortCfgs.flatten.map(_.getDataConfig).toSet
  }

  /**
   * Get the [[DataConfig]] that this exu need to write
   */
  def pregWbDataCfgSet: Set[DataConfig] = {
    this.writePortCfgs.map(_.dataCfg).toSet
  }

  def getRfReadDataCfgSet: Seq[Set[DataConfig]] = {
    val fuSrcsCfgSet: Seq[Seq[Set[DataConfig]]] = fuConfigs.map(_.getRfReadDataCfgSet)
    val alignedFuSrcsCfgSet: Seq[Seq[Set[DataConfig]]] = fuSrcsCfgSet.map(x => x ++ Seq.fill(numRegSrc - x.length)(Set[DataConfig]()))

    val exuSrcsCfgSet = alignedFuSrcsCfgSet.reduce((x, y) => (x zip y).map { case (cfg1, cfg2) => cfg1 union cfg2 })

    exuSrcsCfgSet
  }

  def getGpReadCfgs: Seq[RdConfig] = {
    readPortCfgs.flatten.filter(_.isInstanceOf[IntRD])
  }

  def getFpReadCfgs: Seq[RdConfig] = {
    readPortCfgs.flatten.filter(_.isInstanceOf[FpRD])
  }

  def getVpReadCfgs: Seq[RdConfig] = {
    readPortCfgs.flatten.filter(_.isInstanceOf[VfRD])
  }

  /**
   * Get the [[DataConfig]] mapped indices of source data of exu
   *
   * @example
   * {{{
   *   fuCfg.srcData = Seq(VecData(), VecData(), VecData())
   *   getRfReadSrcIdx(VecData()) = Seq(0, 1, 2)
   * }}}
   * @return Map[DataConfig -> Seq[indices] ]
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

  def getSrcDataType(srcIdx: Int): Set[DataConfig] = {
    fuConfigs.map(_.getSrcDataType(srcIdx)).reduce(_ ++ _)
  }

  def getWbConfigByPregParam(pregParam: PregParams): Option[WbConfig] = {
    pregParam match {
      case IntPregParams(numEntries, numBank, numRead, numWrite) => getGpWriteCfg
      case FpPregParams(numEntries, numBank, numRead, numWrite) => getFpWriteCfg
      case VfPregParams(numEntries, numBank, numRead, numWrite) => getVpWriteCfg
      case V0PregParams(numEntries, numBank, numRead, numWrite) => getV0WriteCfg
      case VlPregParams(numEntries, numBank, numRead, numWrite) => getVlWriteCfg
      case _ => ???
    }
  }

  def getRdConfigWithSrcIdx(dataConfig: DataConfig): Seq[(RdConfig, Int)] = {
    readPortCfgs.zipWithIndex.filter {
      case (rdCfgs, srcIdx) => rdCfgs.exists(_.getDataConfig == dataConfig)
    }.map {
      case (rdCfgs, srcIdx) => rdCfgs.filter(_.getDataConfig == dataConfig).head -> srcIdx
    }
  }

  def immTypes: Set[Imm] = fuConfigs.map(_.immType).reduce(_ | _)

  def immWidth: Int = immTypes.map(_.len).max

  def needImm = immTypes.nonEmpty

  def hasLdu: Boolean = fuConfigs.contains(VecFuConfig.LduCfg)

  def hasSta: Boolean = fuConfigs.contains(VecFuConfig.StaCfg)

  def hasHya: Boolean = fuConfigs.exists(Seq(VecFuConfig.HyldaCfg, VecFuConfig.HystaCfg).contains)

  def hasStd: Boolean = fuConfigs.contains(VecFuConfig.StdCfg)

  def hasVStd: Boolean = fuConfigs.exists(Seq(VecFuConfig.VStdCfg).contains)

  def genRfRdAddrBundle(pregParams: PregParams): MixedVec[RfReadAddrBundle] = MixedVec(
    pregParams match {
      case IntPregParams(_, _, _, _) |
           FpPregParams(_, _, _, _) |
           VfPregParams(_, _, _, _) =>
        this
          .readPortCfgs
          .map(_.collectFirst { case x if x.getDataConfig == pregParams.dataCfg => x })
          .zipWithIndex
          .filter { case (x: Option[RdConfig], _) => x.nonEmpty }
          .map { case (x: Option[RdConfig], srcIdx) => new RfReadAddrBundle(x.get, srcIdx, pregParams) }
      case _: V0PregParams =>
        Option(this.v0RD).map { x: RdConfig => new RfReadAddrBundle(x, 0, pregParams) }.toSeq
      case _: VlPregParams =>
        Option(this.vlRD).map { x: RdConfig => new RfReadAddrBundle(x, 0, pregParams) }.toSeq
    }
  )

  def genRfRdDataBundle(pregParams: PregParams): MixedVec[RfReadDataBundle] = MixedVec(
    pregParams match {
      case IntPregParams(_, _, _, _) |
           FpPregParams(_, _, _, _) |
           VfPregParams(_, _, _, _) =>
        this
          .readPortCfgs
          .map(_.collectFirst { case x if x.getDataConfig == pregParams.dataCfg => x })
          .zipWithIndex
          .filter { case (x: Option[RdConfig], _) => x.nonEmpty }
          .map { case (x: Option[RdConfig], srcIdx) => new RfReadDataBundle(x.get, srcIdx, pregParams) }
      case _: V0PregParams =>
        Option(this.v0RD).map { case x: RdConfig => new RfReadDataBundle(x, 0, pregParams) }.toSeq
      case _: VlPregParams =>
        Option(this.vlRD).map { case x: RdConfig => new RfReadDataBundle(x, 0, pregParams) }.toSeq
    }
  )

  def genRfRdFailBundle(pregParams: PregParams): Vec[Bool] = Vec(
    this.readPortCfgs.count(_.exists(_.getDataConfig == pregParams.dataCfg)),
    Bool(),
  )

  def genExuInputBundle[T <: Bundle](wrapper: Exu.InUop => T)(implicit p: Parameters): T = {
    wrapper(new Exu.InUop(this))
  }

  def genExuOutputBundle[T <: Bundle](wrapper: Exu.OutUop => T)(implicit p: Parameters): T = {
    wrapper(new Exu.OutUop(this))
  }

  def genExuToRobBundle[T <: Bundle](
    wrapper: Exu.ToRob => T,
  )(implicit p: Parameters): T = {
    wrapper(new Exu.ToRob(this))
  }

  def genExuToRfBundle(
    pregParams: PregParams,
  ): Option[Exu.ToRf] = {
    this.getWbConfigByPregParam(pregParams).map(
      x => new Exu.ToRf(x, pregParams)
    )
  }

  def getDefinitionNameOfPipe: String = {
    this.getInstanceNameOfPipe.capitalize
  }

  def getInstanceNameOfPipe: String = {
    "issuePipe" ++ fuConfigs.map(_.name).distinct.map(_.capitalize).reduce(_ ++ _)
  }

  @BeanProperty
  var exeUnitParams: ExeUnitParams = _
}

object ExuParam {
  def apply(exeUnitParams: ExeUnitParams): ExuParam = {
    val instance = new ExuParam(
      name = exeUnitParams.name,
      fuConfigs = exeUnitParams.fuConfigs.map(x => VecFuConfig.allConfigs.find(_.fuConfig == x).get),
      writePortCfgs = exeUnitParams.wbPortConfigs,
      readPortCfgs = exeUnitParams.rfrPortConfigs,
      v0RD = exeUnitParams.v0RD,
      v0WB = exeUnitParams.v0WB,
      vlRD = exeUnitParams.vlRD,
      vlWB = exeUnitParams.vlWB,
    )

    instance.setExeUnitParams(exeUnitParams)

    instance
  }
}
