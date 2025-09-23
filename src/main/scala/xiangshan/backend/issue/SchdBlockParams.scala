package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3.util._
import utils.SeqUtils
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.WakeUpSource
import xiangshan.backend.datapath.WbConfig.PregWB
import xiangshan.backend.fu.FuConfig.I2fCfg

case class SchdBlockParams(
  issueBlockParams: Seq[IssueBlockParams],
  numPregs        : Int,
  numDeqOutside   : Int,
  schdType        : SchedulerType,
  rfDataWidth     : Int,
) {
  var backendParam: BackendParams = null

  def isIntSchd: Boolean = schdType == IntScheduler()

  def isFpSchd: Boolean = schdType == FpScheduler()

  def isVecSchd: Boolean = schdType == VecScheduler()

  def getName: String = if (isIntSchd) "Int" else if (isFpSchd) "Fp" else "Vec"

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

  def FDivSqrtCnt: Int = issueBlockParams.map(_.fDivSqrtCnt).sum

  def LduCnt: Int = issueBlockParams.map(_.LduCnt).sum

  def StaCnt: Int = issueBlockParams.map(_.StaCnt).sum

  def StdCnt: Int = issueBlockParams.map(_.StdCnt).sum

  def MouCnt: Int = issueBlockParams.map(_.MouCnt).sum

  def HyuCnt: Int = issueBlockParams.map(_.HyuCnt).sum

  def LdExuCnt: Int = issueBlockParams.map(_.LdExuCnt).sum

  def VipuCnt: Int = issueBlockParams.map(_.VipuCnt).sum

  def VfpuCnt: Int = issueBlockParams.map(_.VfpuCnt).sum

  def VlduCnt: Int = issueBlockParams.map(_.VlduCnt).sum

  def VstuCnt: Int = issueBlockParams.map(_.VstuCnt).sum

  def numExu: Int = issueBlockParams.map(_.exuBlockParams.count(!_.fakeUnit)).sum

  def hasCSR = CsrCnt > 0

  def hasFence = FenceCnt > 0

  def exuBlockParams =  issueBlockParams.flatMap(_.exuBlockParams)

  def numUopFromDispatch: Int = issueBlockParams.map(_.numEnq).sum

  def numWritebackToRob: Int = issueBlockParams.map(_.numDeq).sum

  def numWriteIntRf: Int = issueBlockParams.map(_.numWriteIntRf).sum

  def numWriteFpRf: Int = issueBlockParams.map(_.numWriteFpRf).sum

  def numWriteVecRf: Int = issueBlockParams.map(_.numWriteVecRf).sum

  def numWriteVfRf: Int = issueBlockParams.map(_.numWriteVfRf).sum

  def numNoDataWB: Int = issueBlockParams.map(_.numNoDataWB).sum

  def needOg2Resp: Boolean = isVecSchd

  def needSrcFrm: Boolean = issueBlockParams.map(_.needSrcFrm).reduce(_ || _)

  def needSrcVxrm: Boolean = issueBlockParams.map(_.needSrcVxrm).reduce(_ || _)

  def writeVConfig: Boolean = issueBlockParams.map(_.writeVConfig).reduce(_ || _)
  
  def writeVType: Boolean = issueBlockParams.map(_.writeVType).reduce(_ || _)

  def numRedirect: Int = issueBlockParams.map(_.numRedirect).sum

  def pregIdxWidth: Int = log2Up(numPregs)

  def numIntRfReadByExu: Int = issueBlockParams.map(_.exuBlockParams.map(_.numIntSrc).sum).sum

  def numFpRfReadByExu: Int = issueBlockParams.map(_.exuBlockParams.map(_.numFpSrc).sum).sum

  def numVfRfReadByExu: Int = issueBlockParams.map(_.exuBlockParams.map(_.numVecSrc).sum).sum

  def numV0RfReadByExu: Int = issueBlockParams.map(_.exuBlockParams.map(_.numV0Src).sum).sum

  def numVlRfReadByExu: Int = issueBlockParams.map(_.exuBlockParams.map(_.numVlSrc).sum).sum

  def bindBackendParam(param: BackendParams): Unit = {
    backendParam = param
  }

  def numWriteRegCache: Int = issueBlockParams.map(_.numWriteRegCache).sum

  def needWriteRegCache: Boolean = numWriteRegCache > 0

  def genExuInputBundle(implicit p: Parameters): MixedVec[MixedVec[DecoupledIO[ExuInput]]] = {
    MixedVec(this.issueBlockParams.map(_.genExuInputDecoupledBundle))
  }

  def genExuInputCopySrcBundle(implicit p: Parameters): MixedVec[MixedVec[DecoupledIO[ExuInput]]] = {
    MixedVec(this.issueBlockParams.map(_.genExuInputDecoupledCopySrcBundle))
  }

  def genExuInputCopySrcBundleMemBlock(implicit p: Parameters): MixedVec[MixedVec[DecoupledIO[ExuInput]]] = {
    MixedVec(this.issueBlockParams.filter(_.isMemBlockIQ).map(_.genExuInputDecoupledCopySrcBundle))
  }

  def genExuOutputDecoupledBundle(implicit p: Parameters): MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = {
    MixedVec(this.issueBlockParams.map(_.genExuOutputDecoupledBundle))
  }

  def genExuOutputDecoupledBundleMemBlock(implicit p: Parameters): MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = {
    MixedVec(this.issueBlockParams.filter(_.isMemBlockIQ).map(_.genExuOutputDecoupledBundle))
  }

  def genExuOutputDecoupledBundleLoad(implicit p: Parameters): MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = {
    MixedVec(backendParam.intSchdParams.get.issueBlockParams.filter(_.isLdAddrIQ).map(_.genExuOutputDecoupledBundle))
  }

  def genExuInputCopySrcBundleNoMemBlock(implicit p: Parameters): MixedVec[MixedVec[DecoupledIO[ExuInput]]] = {
    MixedVec(this.issueBlockParams.filterNot(_.isMemBlockIQ).map(_.genExuInputDecoupledCopySrcBundle))
  }

  def genExuOutputDecoupledBundleNoMemBlock(implicit p: Parameters): MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = {
    MixedVec(this.issueBlockParams.filterNot(_.isMemBlockIQ).map(_.genExuOutputDecoupledBundle))
  }

  def genExuOutputValidBundle(implicit p: Parameters): MixedVec[MixedVec[ValidIO[ExuOutput]]] = {
    MixedVec(this.issueBlockParams.map(_.genExuOutputValidBundle))
  }

  def genExuBypassValidBundle(implicit p: Parameters): MixedVec[MixedVec[ValidIO[ExuBypassBundle]]] = {
    MixedVec(this.issueBlockParams.map(_.genExuBypassValidBundle))
  }

  def wakeUpInExuSources: Seq[WakeUpSource] = {
    issueBlockParams
      .flatMap(_.wakeUpInExuSources)
      .distinctBy(_.name)
  }

  def wakeUpOutExuSources: Seq[WakeUpSource] = {
    issueBlockParams
      .flatMap(_.wakeUpOutExuSources)
      .distinctBy(_.name)
  }

  def genIQWakeUpInValidBundle(implicit p: Parameters): MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = {
    MixedVec(this.wakeUpInExuSources.map(x => {
      val param = x.getExuParam(backendParam.allExuParams)
      val isCopyPdest = param.copyWakeupOut
      val copyNum = param.copyNum
      ValidIO(new IssueQueueIQWakeUpBundle(backendParam.getExuIdx(x.name), backendParam, isCopyPdest, copyNum))
      })
    )
  }

  def genIQWakeUpOutValidBundle(implicit p: Parameters): MixedVec[ValidIO[IssueQueueIQWakeUpBundle]] = {
    MixedVec(this.wakeUpOutExuSources.map(x => {
      val param = x.getExuParam(backendParam.allExuParams)
      val isCopyPdest = param.copyWakeupOut
      val copyNum = param.copyNum
      ValidIO(new IssueQueueIQWakeUpBundle(backendParam.getExuIdx(x.name), backendParam, isCopyPdest, copyNum))
      })
    )
  }

  def genExuWakeUpOutValidBundle(implicit p: Parameters): MixedVec[DecoupledIO[IssueQueueIQWakeUpBundle]] = {
    val uncertainExuParams = this.issueBlockParams.map(_.allExuParams).flatten.filter(_.needUncertainWakeup)
    MixedVec(uncertainExuParams.map(param => {
      val isCopyPdest = param.copyWakeupOut
      val copyNum = param.copyNum
      DecoupledIO(new IssueQueueIQWakeUpBundle(backendParam.getExuIdx(param.name), backendParam, isCopyPdest, copyNum))
    })
    )
  }

  def genWBWakeUpSinkValidBundle(implicit p: Parameters): MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = {
    val intBundle: Seq[ValidIO[IssueQueueWBWakeUpBundle]] = schdType match {
      case IntScheduler() => backendParam.getIntWBExeGroup.map(x => ValidIO(new IssueQueueWBWakeUpBundle(x._2.map(_.exuIdx), backendParam))).toSeq
      case _ => Seq()
    }
    val fpBundle: Seq[ValidIO[IssueQueueWBWakeUpBundle]] = schdType match {
      case FpScheduler() => backendParam.getIntWBExeGroup.map(x => ValidIO(new IssueQueueWBWakeUpBundle(x._2.map(_.exuIdx), backendParam))).toSeq
      case _ => Seq()
    }
    val vfBundle = schdType match {
      case VecScheduler() => backendParam.getVfWBExeGroup.map(x => ValidIO(new IssueQueueWBWakeUpBundle(x._2.map(_.exuIdx), backendParam))).toSeq
      case _ => Seq()
    }
    val v0Bundle = schdType match {
      case VecScheduler() => backendParam.getV0WBExeGroup.map(x => ValidIO(new IssueQueueWBWakeUpBundle(x._2.map(_.exuIdx), backendParam))).toSeq
      case _ => Seq()
    }
    val vlBundle = schdType match {
      case VecScheduler() => backendParam.getVlWBExeGroup.map(x => ValidIO(new IssueQueueWBWakeUpBundle(x._2.map(_.exuIdx), backendParam))).toSeq
      case _ => Seq()
    }
    MixedVec(intBundle ++ fpBundle ++ vfBundle ++ v0Bundle ++ vlBundle)
  }

  def genIntWBWakeUpSinkValidBundle(implicit p: Parameters): MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = {
    MixedVec(backendParam.getIntWBExeGroup.map(x => ValidIO(new IssueQueueWBWakeUpBundle(x._2.map(_.exuIdx), backendParam))).toSeq)
  }

  def genFpWBWakeUpSinkValidBundle(implicit p: Parameters): MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = {
    MixedVec(backendParam.getFpWBExeGroup.map(x => ValidIO(new IssueQueueWBWakeUpBundle(x._2.map(_.exuIdx), backendParam))).toSeq)
  }

  def genVfWBWakeUpSinkValidBundle(implicit p: Parameters): MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = {
    MixedVec(backendParam.getVfWBExeGroup.map(x => ValidIO(new IssueQueueWBWakeUpBundle(x._2.map(_.exuIdx), backendParam))).toSeq)
  }

  def genV0WBWakeUpSinkValidBundle(implicit p: Parameters): MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = {
    MixedVec(backendParam.getV0WBExeGroup.map(x => ValidIO(new IssueQueueWBWakeUpBundle(x._2.map(_.exuIdx), backendParam))).toSeq)
  }

  def genVlWBWakeUpSinkValidBundle(implicit p: Parameters): MixedVec[ValidIO[IssueQueueWBWakeUpBundle]] = {
    MixedVec(backendParam.getVlWBExeGroup.map(x => ValidIO(new IssueQueueWBWakeUpBundle(x._2.map(_.exuIdx), backendParam))).toSeq)
  }

  def genWriteBackBundle(implicit p: Parameters) =  schdType match {
    case IntScheduler() => backendParam.genIntWriteBackBundle
    case FpScheduler() => backendParam.genFpWriteBackBundle
    case VecScheduler() => backendParam.genVfWriteBackBundle

  }
  // cfgs(issueIdx)(exuIdx)(set of exu's wb)
  def getWbCfgs: Seq[Seq[Set[PregWB]]] = {
    this.issueBlockParams.map(_.getWbCfgs)
  }
}
