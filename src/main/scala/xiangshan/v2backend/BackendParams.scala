package xiangshan.v2backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.v2backend.issue.IssueQueueParams
import xiangshan.{FuType, XSCoreParamsKey}

object SchdBlockParams {
  def dummyIntParams(numDeqOutside: Int = 0)(implicit p: Parameters): SchdBlockParams = {
    implicit val schdType: IntScheduler = IntScheduler()
    val numUopIn = 6
    val numRfRead = 14
    val numRfWrite = 8
    val numPregs = 160
    val params = SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExuBlockParams(Seq(AluCfg, VsetCfg, CsrCfg, FenceCfg, MulCfg, BkuCfg)),
        ExuBlockParams(Seq(AluCfg, MulCfg, BkuCfg)),
      ), numEntries = 16, pregBits = numPregs, numWakeupFromWB = numRfWrite, numEnq = 4),
      IssueBlockParams(Seq(
        ExuBlockParams(Seq(AluCfg, DivCfg, I2fCfg)),
        ExuBlockParams(Seq(AluCfg, DivCfg)),
      ), numEntries = 16, pregBits = numPregs, numWakeupFromWB = numRfWrite, numEnq = 4),
      IssueBlockParams(Seq(
        ExuBlockParams(Seq(BrhCfg, JmpCfg)),
        ExuBlockParams(Seq(BrhCfg))
      ), numEntries = 16, pregBits = numPregs, numWakeupFromWB = numRfWrite, numEnq = 4),
    ),
      numPregs = numPregs,
      numRfReadWrite = Some((numRfRead, numRfWrite)),
      numDeqOutside = numDeqOutside,
      schdType = schdType,
      rfDataWidth = p(XSCoreParamsKey).XLEN,
      numUopIn = numUopIn
    )
    params
  }
}

case class SchdBlockParams(
  issueBlockParams: Seq[IssueBlockParams],
  numPregs        : Int,
  numRfReadWrite  : Option[(Int, Int)],
  numDeqOutside   : Int,
  schdType        : SchedulerType,
  rfDataWidth     : Int,
  numUopIn        : Int,
) {
  def JmpCnt      :Int = issueBlockParams.map(_.JmpCnt).sum
  def BrhCnt      :Int = issueBlockParams.map(_.BrhCnt).sum
  def I2fCnt      :Int = issueBlockParams.map(_.I2fCnt).sum
  def CsrCnt      :Int = issueBlockParams.map(_.CsrCnt).sum
  def AluCnt      :Int = issueBlockParams.map(_.AluCnt).sum
  def MulCnt      :Int = issueBlockParams.map(_.MulCnt).sum
  def DivCnt      :Int = issueBlockParams.map(_.DivCnt).sum
  def FenceCnt    :Int = issueBlockParams.map(_.FenceCnt).sum
  def BkuCnt      :Int = issueBlockParams.map(_.BkuCnt).sum
  def VsetCnt     :Int = issueBlockParams.map(_.VsetCnt).sum

  def FmacCnt     :Int = issueBlockParams.map(_.FmacCnt).sum
  def FmiscCnt    :Int = issueBlockParams.map(_.FmiscCnt).sum
  def fDivSqrtCnt :Int = issueBlockParams.map(_.fDivSqrtCnt).sum

  def LduCnt      :Int = issueBlockParams.map(_.LduCnt).sum
  def StuCnt      :Int = issueBlockParams.map(_.StuCnt).sum
  def MouCnt      :Int = issueBlockParams.map(_.MouCnt).sum

  def VipuCnt     :Int = issueBlockParams.map(_.VipuCnt).sum
  def VfpuCnt     :Int = issueBlockParams.map(_.VfpuCnt).sum
  def VlduCnt     :Int = issueBlockParams.map(_.VlduCnt).sum
  def VstuCnt     :Int = issueBlockParams.map(_.VstuCnt).sum

  def numWriteIntRf: Int = issueBlockParams.map(_.numWriteIntRf).sum
  def numWriteFpRf : Int = issueBlockParams.map(_.numWriteFpRf ).sum
  def numWriteVecRf: Int = issueBlockParams.map(_.numWriteVecRf).sum
}

case class IssueBlockParams(
  // top down
  exuBlockParams     : Seq[ExuBlockParams],
  numEntries         : Int,
  pregBits           : Int,
  numWakeupFromWB    : Int,
  numDeqOutside      : Int = 0,
  numWakeupFromOthers: Int = 0,
  // calculate in scheduler
  var numEnq         : Int = 0,
  var numWakeupFromIQ: Int = 0,
)(implicit
  // top down
  val schdType       : SchedulerType,
  val p              : Parameters,
) {
  def numIntSrc     : Int = exuBlockParams.map(_.numIntSrc).max
  def numFpSrc      : Int = exuBlockParams.map(_.numFpSrc ).max
  def numVecSrc     : Int = exuBlockParams.map(_.numVecSrc).max
  def readIntRf     : Boolean = numIntSrc > 0
  def readFpRf      : Boolean = numFpSrc  > 0
  def readVecRf     : Boolean = numVecSrc > 0
  def writeIntRf    : Boolean = exuBlockParams.map(_.writeIntRf).reduce(_ || _)
  def writeFpRf     : Boolean = exuBlockParams.map(_.writeFpRf ).reduce(_ || _)
  def writeVecRf    : Boolean = exuBlockParams.map(_.writeVecRf).reduce(_ || _)
  def exceptionOut  : Seq[Int] = exuBlockParams.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted
  def hasLoadError  : Boolean = exuBlockParams.map(_.hasLoadError).reduce(_ || _)
  def flushPipe     : Boolean = exuBlockParams.map(_.flushPipe).reduce(_ ||_)
  def replayInst    : Boolean = exuBlockParams.map(_.replayInst).reduce(_ || _)
  def trigger       : Boolean = exuBlockParams.map(_.trigger).reduce(_ || _)
  def needExceptionGen: Boolean = exceptionOut.nonEmpty || flushPipe || replayInst || trigger

  def numWriteIntRf : Int = exuBlockParams.count(_.writeIntRf)
  def numWriteFpRf  : Int = exuBlockParams.count(_.writeFpRf)
  def numWriteVecRf : Int = exuBlockParams.count(_.writeVecRf)

  def numRegSrcMax  : Int = numIntSrc max numFpSrc max numVecSrc
  def dataBitsMax   : Int = if (numVecSrc > 0) p(XSCoreParamsKey).VLEN else p(XSCoreParamsKey).XLEN
  def numDeq        : Int = numDeqOutside + exuBlockParams.length

  def JmpCnt      :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.jmp)).sum
  def BrhCnt      :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.brh)).sum
  def I2fCnt      :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.i2f)).sum
  def CsrCnt      :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.csr)).sum
  def AluCnt      :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.alu)).sum
  def MulCnt      :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.mul)).sum
  def DivCnt      :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.div)).sum
  def FenceCnt    :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.fence)).sum
  def BkuCnt      :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.bku)).sum
  def VsetCnt     :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.vset)).sum

  def FmacCnt     :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.fmac)).sum
  def FmiscCnt    :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.fmisc)).sum
  def fDivSqrtCnt :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.fDivSqrt)).sum

  def LduCnt      :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.ldu)).sum
  def StuCnt      :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.stu)).sum
  def MouCnt      :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.mou)).sum

  def VipuCnt     :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.vipu)).sum
  def VfpuCnt     :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.vfpu)).sum
  def VlduCnt     :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.vldu)).sum
  def VstuCnt     :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.vstu)).sum

  def getWbParams: Seq[Seq[WriteBackConfig]] = exuBlockParams.map(params => params.getWbParamsOuter)

  def generateIqParams: IssueQueueParams = {
    IssueQueueParams(
      numEntries = numEntries,
      numEnq = numEnq,
      numDeq = numDeq,
      numSrc = numRegSrcMax,
      dataBits = dataBitsMax,
      pregBits = pregBits,
      numWakeupFromWB = numWakeupFromWB,
      schdType = schdType,
      numWakeupFromIQ = numWakeupFromIQ,
      numWakeupFromOthers = numWakeupFromOthers,
      hasBranch = BrhCnt > 0,
      hasJump = JmpCnt > 0,
      hasLoad = LduCnt > 0,
      hasStore = StuCnt > 0,
      hasMemAddr = LduCnt > 0 || StuCnt > 0
    )
  }

  def getFuCfgs: Seq[FuConfig] = exuBlockParams.flatMap(_.fuConfigs).distinct
}

case class ExuBlockParams(
  fuConfigs: Seq[FuConfig],
)(implicit val schdType: SchedulerType) {
  val numIntSrc     : Int = fuConfigs.map(_.numIntSrc).max
  val numFpSrc      : Int = fuConfigs.map(_.numFpSrc ).max
  val numVecSrc     : Int = fuConfigs.map(_.numVecSrc).max
  val readIntRf     : Boolean = numIntSrc > 0
  val readFpRf      : Boolean = numFpSrc  > 0
  val readVecRf     : Boolean = numVecSrc > 0
  val writeIntRf    : Boolean = fuConfigs.map(_.writeIntRf).reduce(_ || _)
  val writeFpRf     : Boolean = fuConfigs.map(_.writeFpRf ).reduce(_ || _)
  val writeVecRf    : Boolean = fuConfigs.map(_.writeVecRf).reduce(_ || _)
  val writeFflags   : Boolean = fuConfigs.map(_.writeFflags).reduce(_ || _)
  val hasRedirect   : Boolean = fuConfigs.map(_.hasRedirect).reduce(_ || _)
  val exceptionOut  : Seq[Int] = fuConfigs.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted
  val hasLoadError  : Boolean = fuConfigs.map(_.hasLoadError).reduce(_ || _)
  val flushPipe     : Boolean = fuConfigs.map(_.flushPipe).reduce(_ ||_)
  val replayInst    : Boolean = fuConfigs.map(_.replayInst).reduce(_ || _)
  val trigger       : Boolean = fuConfigs.map(_.trigger).reduce(_ || _)
  val needExceptionGen: Boolean = exceptionOut.nonEmpty || flushPipe || replayInst || trigger

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
    Cat(fuConfigs.map(_.fuType === fuType)).orR
  }

  def hasUncertainLatency: Boolean = fuConfigs.map(_.latency.latencyVal.isEmpty).reduce(_ || _)

  def getWbParamsInner: Seq[Seq[WriteBackConfig]] = {
    this.fuConfigs.map(cfg => {
      val res = Seq()
      if (cfg.writeIntRf) res :+ WriteBackConfig(getWBSource, IntScheduler())
      if (cfg.writeFpRf)  res :+ WriteBackConfig(getWBSource, VfScheduler())
      if (cfg.writeVecRf) res :+ WriteBackConfig(getWBSource, VfScheduler())
      res
    })
  }

  def getWbParamsOuter: Seq[WriteBackConfig] = {
    val res = Seq()
    if (writeIntRf) res :+ WriteBackConfig(getWBSource, IntScheduler())
    if (writeFpRf || writeVecRf) res :+ WriteBackConfig(getWBSource, VfScheduler())
    res
  }
}



