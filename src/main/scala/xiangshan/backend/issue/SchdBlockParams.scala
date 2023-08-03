package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import utils.SeqUtils
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.WakeUpSource
import xiangshan.backend.datapath.WbConfig.PregWB

case class SchdBlockParams(
  issueBlockParams: Seq[IssueBlockParams],
  numPregs        : Int,
  numDeqOutside   : Int,
  schdType        : SchedulerType,
  rfDataWidth     : Int,
  numUopIn        : Int,
) {
  var backendParam: BackendParams = null

  def isMemSchd: Boolean = schdType == MemScheduler()

  def isIntSchd: Boolean = schdType == IntScheduler()

  def isVfSchd: Boolean = schdType == VfScheduler()

  def JmpCnt: Int = issueBlockParams.map(_.JmpCnt).sum

  def BrhCnt: Int = issueBlockParams.map(_.BrhCnt).sum

  def I2fCnt: Int = issueBlockParams.map(_.I2fCnt).sum

  def CsrCnt: Int = issueBlockParams.map(_.CsrCnt).sum

  def AluCnt: Int = issueBlockParams.map(_.AluCnt).sum

  def MulCnt: Int = issueBlockParams.map(_.MulCnt).sum

  def DivCnt: Int = issueBlockParams.map(_.DivCnt).sum

  def FenceCnt: Int = issueBlockParams.map(_.FenceCnt).sum

  def BkuCnt: Int = issueBlockParams.map(_.BkuCnt).sum

  def VsetCnt: Int = issueBlockParams.map(_.VsetCnt).sum

  def FmacCnt: Int = issueBlockParams.map(_.FmacCnt).sum

  def FmiscCnt: Int = issueBlockParams.map(_.FmiscCnt).sum

  def FDivSqrtCnt: Int = issueBlockParams.map(_.fDivSqrtCnt).sum

  def LduCnt: Int = issueBlockParams.map(_.LduCnt).sum

  def StaCnt: Int = issueBlockParams.map(_.StaCnt).sum

  def StdCnt: Int = issueBlockParams.map(_.StdCnt).sum

  def MouCnt: Int = issueBlockParams.map(_.MouCnt).sum

  def VipuCnt: Int = issueBlockParams.map(_.VipuCnt).sum

  def VfpuCnt: Int = issueBlockParams.map(_.VfpuCnt).sum

  def VlduCnt: Int = issueBlockParams.map(_.VlduCnt).sum

  def VstuCnt: Int = issueBlockParams.map(_.VstuCnt).sum

  def numExu: Int = issueBlockParams.map(_.exuBlockParams.size).sum

  def hasCSR = CsrCnt > 0

  def hasFence = FenceCnt > 0

  def numWriteIntRf: Int = issueBlockParams.map(_.numWriteIntRf).sum

  def numWriteFpRf: Int = issueBlockParams.map(_.numWriteFpRf).sum

  def numWriteVecRf: Int = issueBlockParams.map(_.numWriteVecRf).sum

  def numWriteVfRf: Int = issueBlockParams.map(_.numWriteVfRf).sum

  def numNoDataWB: Int = issueBlockParams.map(_.numNoDataWB).sum

  def numPcReadPort = {
    val bjIssueQueues = issueBlockParams.filter(x => (x.JmpCnt + x.BrhCnt + x.FenceCnt) > 0)
    if (bjIssueQueues.map(x => x.numEnq).sum > 0) numUopIn else 0
  }

  def needSrcFrm: Boolean = issueBlockParams.map(_.needSrcFrm).reduce(_ || _)

  def numRedirect: Int = issueBlockParams.map(_.numRedirect).sum

  def pregIdxWidth: Int = log2Up(numPregs)

  def numWakeupFromWB: Int = schdType match {
    case IntScheduler() | VfScheduler() => 8
    case MemScheduler() => 16 // Todo
    case _ => 0
  }

  def numIntRfReadByExu: Int = issueBlockParams.map(_.exuBlockParams.map(_.numIntSrc).sum).sum

  def numVfRfReadByExu: Int = issueBlockParams.map(_.exuBlockParams.map(x => x.numFpSrc + x.numVecSrc).sum).sum

  def bindBackendParam(param: BackendParams): Unit = {
    backendParam = param
  }

  def genExuInputBundle(implicit p: Parameters): MixedVec[MixedVec[DecoupledIO[ExuInput]]] = {
    MixedVec(this.issueBlockParams.map(_.genExuInputDecoupledBundle))
  }

  def genExuOutputDecoupledBundle(implicit p: Parameters): MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = {
    MixedVec(this.issueBlockParams.map(_.genExuOutputDecoupledBundle))
  }

  def genExuOutputValidBundle(implicit p: Parameters): MixedVec[MixedVec[ValidIO[ExuOutput]]] = {
    MixedVec(this.issueBlockParams.map(_.genExuOutputValidBundle))
  }

  def genExuBypassValidBundle(implicit p: Parameters): MixedVec[MixedVec[ValidIO[ExuBypassBundle]]] = {
    MixedVec(this.issueBlockParams.map(_.genExuBypassValidBundle))
  }

  def wakeUpInExuSources: Seq[WakeUpSource] = {
    SeqUtils.distinctBy(
      issueBlockParams
        .flatMap(_.wakeUpInExuSources)
    )(_.name)
  }

  def wakeUpOutExuSources: Seq[WakeUpSource] = {
    SeqUtils.distinctBy(
      issueBlockParams
        .flatMap(_.wakeUpOutExuSources)
    )(_.name)
  }

  def genIQWakeUpInValidBundle(implicit p: Parameters): MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = {
    MixedVec(this.wakeUpInExuSources.map(x => ValidIO(new IssueQueueIQWakeUpBundle(backendParam.getExuIdx(x.name), backendParam))))
  }

  def genIQWakeUpOutValidBundle(implicit p: Parameters): MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = {
    MixedVec(this.wakeUpOutExuSources.map(x => ValidIO(new IssueQueueIQWakeUpBundle(backendParam.getExuIdx(x.name), backendParam))))
  }

  def genWBWakeUpSinkValidBundle: MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = {
    val intBundle: Seq[ValidIO[IssueQueueWBWakeUpBundle]] = schdType match {
      case IntScheduler() | MemScheduler() => backendParam.getIntWBExeGroup.map(x => ValidIO(new IssueQueueWBWakeUpBundle(x._2.map(_.exuIdx), backendParam))).toSeq
      case _ => Seq()
    }
    val vfBundle = schdType match {
      case VfScheduler() | MemScheduler() => backendParam.getVfWBExeGroup.map(x => ValidIO(new IssueQueueWBWakeUpBundle(x._2.map(_.exuIdx), backendParam))).toSeq
      case _ => Seq()
    }
    MixedVec(intBundle ++ vfBundle)
  }

  // cfgs(issueIdx)(exuIdx)(set of exu's wb)
  def getWbCfgs: Seq[Seq[Set[PregWB]]] = {
    this.issueBlockParams.map(_.getWbCfgs)
  }
}
