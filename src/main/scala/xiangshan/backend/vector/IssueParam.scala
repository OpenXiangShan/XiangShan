package xiangshan.backend.vector

import chisel3.util.MixedVec
import chisel3.{Bool, Bundle, Vec}
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.BackendParams
import xiangshan.backend.datapath.RdConfig
import xiangshan.backend.datapath.RdConfig.RdConfig
import xiangshan.backend.datapath.WbConfig.PregWB
import xiangshan.backend.decode.Imm
import xiangshan.backend.issue.IssueBlockParams
import xiangshan.backend.regfile.PregParams
import xiangshan.backend.vector.fu.util.VecFuConfig

import scala.beans.BeanProperty

class IssueParam(
  val exuParams   : Seq[ExuParam],
  val numEnq      : Int,
  val numFastEntry: Int,
  val numSlowEntry: Int,
) {
  @BeanProperty
  var regionParam: RegionParam = _
  @BeanProperty
  var backendParams: BackendParams = _

  def inIntRegion: Boolean = regionParam.isIntRegion

  def inFltRegion: Boolean = regionParam.isFltRegion

  def inVecRegion: Boolean = regionParam.isVecRegion

  def numExu: Int = exuParams.size

  def numRegSrc = exuParams.map(_.numRegSrc).max
  def numGpSrc: Int = exuParams.map(_.numGpSrc).max
  def numFpSrc: Int = exuParams.map(_.numFpSrc).max
  def numVpSrc: Int = exuParams.map(_.numVpSrc).max
  val readV0Rf: Boolean = exuParams.exists(_.readV0Rf)
  val readVlRf: Boolean = exuParams.exists(_.readVlRf)
  val readFrm : Boolean = exuParams.exists(_.readFrm)
  val readVxrm: Boolean = exuParams.exists(_.readVxrm)
  val readVType: Boolean = exuParams.exists(_.readVType)
  val readOldVType: Boolean = exuParams.exists(_.readOldVType)

  val needVM: Boolean = exuParams.exists(_.needVM)

  val needGpWen = exuParams.exists(_.needGpWen)
  val needFpWen = exuParams.exists(_.needFpWen)
  val needVpWen = exuParams.exists(_.needVpWen)
  val needV0Wen = exuParams.exists(_.needV0Wen)
  val needVlWen = exuParams.exists(_.needVlWen)

  val needFflagsWen = exuParams.exists(_.needFFlagsWen)
  val needVxsatWen = exuParams.exists(_.needVxsatWen)
  val needVTypeWen = exuParams.exists(_.needVTypeWen)

  val needRedirect: Boolean = exuParams.exists(_.needRedirect)
  val needIsRVC: Boolean = exuParams.exists(_.needIsRVC)
  val needRasAction: Boolean = exuParams.exists(_.needRasAction)
  val needFlushPipe: Boolean = exuParams.exists(_.needFlushPipe)
  val needReplay: Boolean = exuParams.exists(_.needReplay)
  val needTrigger: Boolean = exuParams.exists(_.needTrigger)
  val needPc: Boolean = exuParams.exists(_.needPc)
  val needExceptionGen: Boolean = exuParams.exists(_.needExceptionGen)

  def needSqIdx: Boolean = this.hasSta || this.hasLdu || this.hasStd || this.hasVStd

  def numGpWen: Int = exuParams.count(_.needGpWen)
  def numFpWen: Int = exuParams.count(_.needFpWen)
  def numVpWen: Int = exuParams.count(_.needVpWen)
  def numV0Wen: Int = exuParams.count(_.needV0Wen)
  def numVlWen: Int = exuParams.count(_.needVlWen)

  def getVlReadCfgs: Seq[RdConfig.VlRD] = exuParams.map(_.vlRD)

  def numDeq: Int = numExu

  def numEntry: Int = numEnq + numFastEntry

  def exceptionOut: Seq[Int] = exuParams.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted

  def numRedirect: Int = exuParams.count(_.needRedirect)

  def getFuCfgs: Seq[VecFuConfig] = exuParams.flatMap(_.fuConfigs).distinct

  def deqFuCfgs: Seq[Seq[VecFuConfig]] = exuParams.map(_.fuConfigs)

  def deqImmTypes: Seq[Imm] = getFuCfgs.flatMap(_.immType).distinct

  def needImm: Boolean = deqImmTypes.nonEmpty

  def deqImmTypesMaxLen: Int = deqImmTypes.map(x => x).maxBy(_.len).len

  def hasVStd: Boolean = this.exuParams.exists(_.hasVStd)

  def hasSta: Boolean = this.exuParams.exists(_.hasSta)

  def hasStd: Boolean = this.exuParams.exists(_.hasStd)

  def hasLdu: Boolean = this.exuParams.exists(_.hasLdu)

  def numVStd: Int = this.exuParams.count(_.hasVStd)

  /**
   * Get [[PregWB]] of this IssueBlock
   * @return set of [[PregWB]] of [[Exu]]
   */
  def getWbCfgs: Seq[Set[PregWB]] = {
    exuParams.map(exu => exu.writePortCfgs.toSet)
  }

  def getGpReadCfgs: Seq[RdConfig] = {
    exuParams.flatMap(_.getGpReadCfgs)
  }

  private def collectWbPortIds(wbPorts: Seq[Option[Int]]): Seq[Int] = {
    wbPorts.flatten.distinct.sorted
  }

  def intWbPortIds: Seq[Int] = collectWbPortIds(exuParams.map(_.getGpWbPort.map(_.port)))

  def fpWbPortIds: Seq[Int] = collectWbPortIds(exuParams.map(_.getFpWbPort.map(_.port)))

  def vpWbPortIds: Seq[Int] = collectWbPortIds(exuParams.map(_.getVpWbPort.map(_.port)))

  def v0WbPortIds: Seq[Int] = collectWbPortIds(exuParams.map(_.getV0WbPort.map(_.port)))

  def vlWbPortIds: Seq[Int] = collectWbPortIds(exuParams.map(_.getVlWbPort.map(_.port)))

  def getFpReadCfgs: Seq[RdConfig] = {
    exuParams.flatMap(_.getFpReadCfgs)
  }

  def getVpReadCfgs: Seq[RdConfig] = {
    exuParams.flatMap(_.getVpReadCfgs)
  }

  def genIssueBundle[T <: Bundle](wrapper: VecIssueQueue.Deq => T)(implicit p: Parameters): MixedVec[T] = {
    MixedVec(this.exuParams.map(x => wrapper(new VecIssueQueue.Deq(x))))
  }

  def genRfRdAddrBundle(pregParams: PregParams): MixedVec[MixedVec[IssuePipe.RfReadAddrBundle]] = {
    MixedVec(this.exuParams.map(_.genRfRdAddrBundle(pregParams)))
  }

  def genRfRdDataBundle(pregParams: PregParams): MixedVec[MixedVec[IssuePipe.RfReadDataBundle]] = {
    MixedVec(this.exuParams.map(_.genRfRdDataBundle(pregParams)))
  }

  def genRfRdFailBundle(pregParams: PregParams): MixedVec[Vec[Bool]] = {
    MixedVec(this.exuParams.map(_.genRfRdFailBundle(pregParams)))
  }

  def genExuInputBundle[T <: Bundle](
    wrapper: Exu.InUop => T,
    cond: ExuParam => Boolean = _ => true,
  )(implicit p: Parameters): MixedVec[T] = {
    MixedVec(this.exuParams.filter(cond).map(x => x.genExuInputBundle(wrapper)))
  }

  def genExuOutputBundle[T <: Bundle](
    wrapper: Exu.OutUop => T,
    cond: ExuParam => Boolean = _ => true,
  )(implicit p: Parameters): MixedVec[T] = {
    MixedVec(this.exuParams.filter(cond).map(x => x.genExuOutputBundle(wrapper)))
  }

  def genExuToRobBundle[T <: Bundle](
    wrapper: Exu.ToRob => T,
    cond: ExuParam => Boolean = _ => true,
  )(implicit p: Parameters): MixedVec[T] = {
    MixedVec(this.exuParams.filter(cond).map(x => x.genExuToRobBundle(wrapper)))
  }

  def genExuToRfBundle(
    pregParams: PregParams,
  ): MixedVec[Exu.ToRf] = {
    MixedVec(this.exuParams.map(x => x.genExuToRfBundle(pregParams)).filter(_.nonEmpty).map(_.get))
  }

  def getDefinitionNameOfIQ: String = {
    this.getInstanceNameOfIQ.capitalize
  }

  def getInstanceNameOfIQ: String = {
    "issueQueue" ++ getFuCfgs.map(_.name).distinct.map(_.capitalize).reduce(_ ++ _)
  }

  def issuedTimerWidth = 3

  @BeanProperty
  var issueBlockParams: IssueBlockParams = _
}

object IssueParam {
  def apply(issueBlockParams: IssueBlockParams): IssueParam = {
    val instance: IssueParam = new IssueParam(
      exuParams = issueBlockParams.exuBlockParams.map(ExuParam(_)),
      numEnq = issueBlockParams.numEnq,
      numFastEntry = issueBlockParams.numComp,
      numSlowEntry = issueBlockParams.numSimp,
    )

    instance.exuParams.foreach(_.setIssueParam(instance))

    instance.setIssueBlockParams(issueBlockParams)

    instance
  }
}
