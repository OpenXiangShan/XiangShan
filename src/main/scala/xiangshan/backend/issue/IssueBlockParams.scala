package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3.util._
import chisel3._
import xiangshan.backend.Bundles.{ExuInput, ExuOutput, IssueQueueIssueBundle, OGRespBundle}
import xiangshan.backend.datapath.WbConfig.WbConfig
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.{FuConfig, FuType}

case class IssueBlockParams(
  // top down
  exuBlockParams     : Seq[ExeUnitParams],
  numEntries         : Int,
  pregBits           : Int,
  numWakeupFromWB    : Int,
  numDeqOutside      : Int = 0,
  numWakeupFromOthers: Int = 0,
  XLEN               : Int = 64,
  VLEN               : Int = 128,
  vaddrBits          : Int = 39,
  // calculate in scheduler
  var numEnq         : Int = 0,
  var numWakeupFromIQ: Int = 0,
)(
  implicit
  // top down
  val schdType: SchedulerType,
) {
  def inMemSchd: Boolean = schdType == MemScheduler()

  def inIntSchd: Boolean = schdType == IntScheduler()

  def inVfSchd: Boolean = schdType == VfScheduler()

  def isMemAddrIQ: Boolean = inMemSchd && StdCnt == 0

  def isLdAddrIQ: Boolean = inMemSchd && LduCnt > 0

  def isStAddrIQ: Boolean = inMemSchd && StaCnt > 0

  def numExu: Int = exuBlockParams.length

  def numIntSrc: Int = exuBlockParams.map(_.numIntSrc).max

  def numFpSrc: Int = exuBlockParams.map(_.numFpSrc).max

  def numVecSrc: Int = exuBlockParams.map(_.numVecSrc).max

  def numVfSrc: Int = exuBlockParams.map(_.numVfSrc).max

  def numRegSrc: Int = exuBlockParams.map(_.numRegSrc).max

  def numSrc: Int = exuBlockParams.map(_.numSrc).max

  def readIntRf: Boolean = numIntSrc > 0

  def readFpRf: Boolean = numFpSrc > 0

  def readVecRf: Boolean = numVecSrc > 0

  def readVfRf: Boolean = numVfSrc > 0

  def writeIntRf: Boolean = exuBlockParams.map(_.writeIntRf).reduce(_ || _)

  def writeFpRf: Boolean = exuBlockParams.map(_.writeFpRf).reduce(_ || _)

  def writeVecRf: Boolean = exuBlockParams.map(_.writeVecRf).reduce(_ || _)

  def exceptionOut: Seq[Int] = exuBlockParams.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted

  def hasLoadError: Boolean = exuBlockParams.map(_.hasLoadError).reduce(_ || _)

  def flushPipe: Boolean = exuBlockParams.map(_.flushPipe).reduce(_ || _)

  def replayInst: Boolean = exuBlockParams.map(_.replayInst).reduce(_ || _)

  def trigger: Boolean = exuBlockParams.map(_.trigger).reduce(_ || _)

  def needExceptionGen: Boolean = exceptionOut.nonEmpty || flushPipe || replayInst || trigger

  def needPc: Boolean = JmpCnt + BrhCnt + FenceCnt > 0

  def needSrcFrm: Boolean = exuBlockParams.map(_.needSrcFrm).reduce(_ || _)

  def numPcReadPort: Int = (if (needPc) 1 else 0) * numEnq

  def numWriteIntRf: Int = exuBlockParams.count(_.writeIntRf)

  def numWriteFpRf: Int = exuBlockParams.count(_.writeFpRf)

  def numWriteVecRf: Int = exuBlockParams.count(_.writeVecRf)

  def numWriteVfRf: Int = exuBlockParams.count(_.writeVfRf)

  def numNoDataWB: Int = exuBlockParams.count(_.hasNoDataWB)

  def dataBitsMax: Int = if (numVecSrc > 0) VLEN else XLEN

  def numDeq: Int = numDeqOutside + exuBlockParams.length

  def JmpCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.jmp)).sum

  def BrhCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.brh)).sum

  def I2fCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.i2f)).sum

  def CsrCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.csr)).sum

  def AluCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.alu)).sum

  def MulCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.mul)).sum

  def DivCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.div)).sum

  def FenceCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.fence)).sum

  def BkuCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.bku)).sum

  def VsetCnt: Int = exuBlockParams.map(_.fuConfigs.count(x => x.fuType == FuType.vsetiwi || x.fuType == FuType.vsetiwf || x.fuType == FuType.vsetfwf)).sum

  def FmacCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.fmac)).sum

  def FmiscCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.fmisc)).sum

  def fDivSqrtCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.fDivSqrt)).sum

  def LduCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.name == "ldu")).sum

  def StaCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.name == "sta")).sum

  def MouCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.mou)).sum

  def StdCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.name == "std")).sum

  def VipuCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.vipu)).sum

  def VfpuCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.vfpu)).sum

  def VlduCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.vldu)).sum

  def VstuCnt: Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.vstu)).sum

  def numRedirect: Int = exuBlockParams.count(_.hasRedirect)

  def numAllWakeUp = numWakeupFromWB + numWakeupFromIQ + numWakeupFromOthers

  def getFuCfgs: Seq[FuConfig] = exuBlockParams.flatMap(_.fuConfigs).distinct

  // cfgs(exuIdx)(set of exu's wb)
  def getWbCfgs: Seq[Set[WbConfig]] = {
    exuBlockParams.map(exu => exu.wbPortConfigs.toSet)
  }

  def canAccept(fuType: UInt): Bool = {
    Cat(getFuCfgs.map(_.fuType.U === fuType)).orR
  }

  def genExuInputDecoupledBundle(implicit p: Parameters): MixedVec[DecoupledIO[ExuInput]] = {
    MixedVec(this.exuBlockParams.map(x => DecoupledIO(x.genExuInputBundle)))
  }

  def genExuOutputDecoupledBundle(implicit p: Parameters): MixedVec[DecoupledIO[ExuOutput]] = {
    MixedVec(this.exuBlockParams.map(x => DecoupledIO(x.genExuOutputBundle)))
  }

  def genExuOutputValidBundle(implicit p: Parameters): MixedVec[ValidIO[ExuOutput]] = {
    MixedVec(this.exuBlockParams.map(x => ValidIO(x.genExuOutputBundle)))
  }

  def genIssueDecoupledBundle(implicit p: Parameters): MixedVec[DecoupledIO[IssueQueueIssueBundle]] = {
    MixedVec(exuBlockParams.map(x => DecoupledIO(new IssueQueueIssueBundle(this, x, pregBits, vaddrBits))))
  }

  def genOGRespBundle(implicit p: Parameters) = {
    implicit val issueBlockParams = this
    MixedVec(exuBlockParams.map(_ => new OGRespBundle))
  }

  def getIQName = {
    "IssueQueue" ++ getFuCfgs.map(_.name).distinct.map(_.capitalize).reduce(_ ++ _)
  }
}
