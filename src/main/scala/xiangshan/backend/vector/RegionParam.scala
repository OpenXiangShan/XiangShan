package xiangshan.backend.vector

import chisel3._
import chisel3.util.MixedVec
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.BackendParams
import xiangshan.backend.datapath.RdConfig
import xiangshan.backend.datapath.RdConfig.RdConfig
import xiangshan.backend.issue._
import xiangshan.backend.regfile.PregParams

import scala.beans.BeanProperty

trait RegionType {
  def getSchdType: SchedulerType = {
    this match {
      case FltRegion => FpScheduler()
      case IntRegion => IntScheduler()
      case VecRegion => VecScheduler()
      case _ => ???
    }
  }
}

case object IntRegion extends RegionType
case object FltRegion extends RegionType
case object VecRegion extends RegionType

class RegionParam(
  val issueParams: Seq[IssueParam],
)(
  val region: RegionType,
) {
  @BeanProperty
  var backendParams: BackendParams = _

  def isIntRegion = region == IntRegion
  def isFltRegion = region == FltRegion
  def isVecRegion = region == VecRegion

  def sumNumDeq: Int = issueParams.map(_.numDeq).sum
  def maxIQEntry: Int = issueParams.map(_.numEntry).max
  def numNonVStdIQ: Int = issueParams.count(x => !x.hasVStd)

  def getGpReadCfgs: Seq[RdConfig] = {
    issueParams.flatMap(_.getGpReadCfgs)
  }

  def getFpReadCfgs: Seq[RdConfig] = {
    issueParams.flatMap(_.getFpReadCfgs)
  }

  def getVpReadCfgs: Seq[RdConfig] = {
    issueParams.flatMap(_.getVpReadCfgs)
  }

  def getGpWriteSize: Int = {
    issueParams.map(_.numGpWen).sum
  }

  def getFpWriteSize: Int = {
    issueParams.map(_.numFpWen).sum
  }

  def getVpWriteSize: Int = {
    issueParams.map(_.numVpWen).sum
  }

  def getVlReadCfgs: Seq[Seq[RdConfig.VlRD]] = {
    issueParams.map(_.getVlReadCfgs)
  }

  def genExuBundle[T <: Bundle](
    cond: ExuParam => Boolean = _ => true,
    gen: => T,
  ): MixedVec[MixedVec[T]] = {
    MixedVec(issueParams.map(_.genExuBundle(cond, gen)))
  }

  def genRfRdAddrBundle(pregParams: PregParams)(implicit p: Parameters): MixedVec[MixedVec[MixedVec[IssuePipe.RfReadAddrBundle]]] = {
    MixedVec(this.issueParams.map(_.genRfRdAddrBundle(pregParams)))
  }

  def genRfRdDataBundle(pregParams: PregParams): MixedVec[MixedVec[MixedVec[IssuePipe.RfReadDataBundle]]] = {
    MixedVec(this.issueParams.map(_.genRfRdDataBundle(pregParams)))
  }

  def genRfRdFailBundle(pregParams: PregParams): MixedVec[MixedVec[Vec[Bool]]] = {
    MixedVec(this.issueParams.map(_.genRfRdFailBundle(pregParams)))
  }

  def genExuInputBundle[T <: Bundle](
    wrapper: Exu.InUop => T,
    cond: ExuParam => Boolean = _ => true
  )(implicit p: Parameters): MixedVec[MixedVec[T]] = {
    MixedVec(this.issueParams.map(x => x.genExuInputBundle(wrapper, cond)))
  }

  def genExuOutputBundle[T <: Bundle](wrapper: Exu.OutUop => T)(implicit p: Parameters): MixedVec[MixedVec[T]] = {
    MixedVec(this.issueParams.map(x => x.genExuOutputBundle(wrapper)))
  }

  def genExuToRobBundle[T <: Bundle](
    wrapper: Exu.ToRob => T,
    cond: ExuParam => Boolean = _ => true,
  )(implicit p: Parameters): MixedVec[MixedVec[T]] = {
    MixedVec(this.issueParams.map(x => x.genExuToRobBundle(wrapper, cond)))
  }

  def genExuToRfBundle(
    pregParams: PregParams,
  ): MixedVec[MixedVec[Exu.ToRf]] = {
    MixedVec(this.issueParams.map(x => x.genExuToRfBundle(pregParams)))
  }

}

object RegionParam {
  def apply(
    schdBlockParams: SchdBlockParams,
    backendParams: BackendParams,
  ): RegionParam = {
    val instance: RegionParam = new RegionParam(
      issueParams = schdBlockParams.issueBlockParams.map(IssueParam(_)),
    )(
      region = schdBlockParams.schdType match {
        case IntScheduler() => IntRegion
        case FpScheduler() => FltRegion
        case VecScheduler() => VecRegion
        case NoScheduler() => ???
      },
    )

    instance.issueParams.foreach(_.exuParams.map(_.setBackendParams(backendParams)))
    instance.issueParams.foreach(_.setBackendParams(backendParams))
    instance.issueParams.foreach(_.setRegionParam(instance))

    instance.setBackendParams(backendParams)

    instance
  }
}
