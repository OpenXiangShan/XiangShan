/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
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

package xiangshan.v2backend

import chisel3._
import chisel3.util._
import xiangshan.v2backend.Bundles.{ExuInput, ExuOutput}
import xiangshan.v2backend.issue.IssueQueueParams

object SchdBlockParams {
  def dummyIntParams(numDeqOutside: Int = 0): SchdBlockParams = {
    implicit val schdType: IntScheduler = IntScheduler()
    val numUopIn = 6
    val numRfRead = 14
    val numRfWrite = 8
    val numPregs = 160
    val pregBits = log2Up(numPregs)
    val rfDataWidth = 64
    var params = SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(AluCfg, MulCfg, BkuCfg), Seq(IntWB(port = 0, 0))),
        ExeUnitParams(Seq(AluCfg, MulCfg, BkuCfg), Seq(IntWB(port = 1, 0))),
      ), numEntries = 16, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 4),
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(AluCfg, DivCfg, I2fCfg), Seq(IntWB(port = 2, 0), VfWB(port = 7, Int.MaxValue))),
        ExeUnitParams(Seq(AluCfg, DivCfg), Seq(IntWB(port = 3, 0))),
      ), numEntries = 16, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 4),
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(BrhCfg, JmpCfg, FenceCfg), Seq(IntWB(port = 4, 0))),
        ExeUnitParams(Seq(BrhCfg, VsetCfg, CsrCfg), Seq(IntWB(port = 5, 0)))
      ), numEntries = 16, pregBits = pregBits, numWakeupFromWB = numRfWrite, numEnq = 4),
    ),
      numPregs = numPregs,
      numRfReadWrite = Some((numRfRead, numRfWrite)),
      numDeqOutside = numDeqOutside,
      schdType = schdType,
      rfDataWidth = rfDataWidth,
      numUopIn = numUopIn,
    )
    params
  }

  def dummyVfParams(numDeqOutside: Int = 0): SchdBlockParams = {
    implicit val schdType: SchedulerType = VfScheduler()
    val numUopIn = 6
    val numRfRead = 12
    val numRfWrite = 8
    val numPregs = 160
    val pregIdxWidth = log2Up(numPregs)
    val rfDataWidth = 128

    var params = SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(VipuCfg), Seq(VfWB(port = 0, 0))),
        ExeUnitParams(Seq(VipuCfg), Seq(VfWB(port = 0, 0))),
      ), numEntries = 16, pregBits = pregIdxWidth, numWakeupFromWB = numRfWrite, numEnq = 4),
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(VfpuCfg, F2fCfg), Seq(VfWB(port = 0, 0))),
        ExeUnitParams(Seq(VfpuCfg, F2fCfg, F2iCfg), Seq(IntWB(port = 5, Int.MaxValue), VfWB(port = 0, 0))),
      ), numEntries = 16, pregBits = pregIdxWidth, numWakeupFromWB = numRfWrite, numEnq = 4),
    ),
      numPregs = numPregs,
      numRfReadWrite = Some((numRfRead, numRfWrite)),
      numDeqOutside = numDeqOutside,
      schdType = schdType,
      rfDataWidth = rfDataWidth,
      numUopIn = numUopIn,
    )
    params
  }

  def dummyMemParams(): SchdBlockParams = {
    implicit val schdType: SchedulerType = MemScheduler()
    val numUopIn = 6
    val numPregs = 160
    val pregBits = log2Up(numPregs)
    val rfDataWidth = 64

    var params = SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(LduCfg), WBSeq(IntWB(6, 0), VfWB(6, 0))),
        ExeUnitParams(Seq(LduCfg), WBSeq(IntWB(7, 0), VfWB(7, 0))),
      ), numEntries = 16, pregBits = pregBits, numWakeupFromWB = 16, numEnq = 4),
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(StaCfg), WBSeq()),
        ExeUnitParams(Seq(StaCfg), WBSeq()),
      ), numEntries = 16, pregBits = pregBits, numWakeupFromWB = 16, numEnq = 4),
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(StdCfg), WBSeq()),
        ExeUnitParams(Seq(StdCfg), WBSeq()),
      ), numEntries = 16, pregBits = pregBits, numWakeupFromWB = 16, numEnq = 4),
    ),
      numPregs = numPregs,
      numRfReadWrite = None,
      numDeqOutside = 0,
      schdType = schdType,
      rfDataWidth = rfDataWidth,
      numUopIn = numUopIn,
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

  def pregIdxWidth: Int = log2Up(numPregs)

  def numWakeupFromWB: Int = schdType match {
    case IntScheduler() | VfScheduler() => numRfWrite
    case MemScheduler() => 0 // Todo
    case _ => 0
  }

  def numTotalIntRfRead: Int = issueBlockParams.map(_.exuBlockParams.map(_.numIntSrc).sum).sum
  def numTotalIntRfWrite: Int = issueBlockParams.map(_.exuBlockParams.length).sum

  // Todo: 14R8W
  def numRfRead : Int = numTotalIntRfRead
  def numRfWrite: Int = numTotalIntRfWrite
}

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
)(implicit
  // top down
  val schdType       : SchedulerType,
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
  def dataBitsMax   : Int = if (numVecSrc > 0) VLEN else XLEN
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

  def genIqParams: IssueQueueParams = {
    IssueQueueParams(
      exuParams = exuBlockParams,
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
      vaddrBits = vaddrBits
    )
  }

  def getFuCfgs: Seq[FuConfig] = exuBlockParams.flatMap(_.fuConfigs).distinct
  def canAccept(fuType: UInt): Bool = {
    Cat(getFuCfgs.map(_.fuType.U === fuType)).orR
  }
}

trait WBPortConfig {
  val port: Int
  val priority: Int
}

case class IntWB(
  override val port    : Int,
  override val priority: Int,
) extends WBPortConfig

case class VfWB(
  override val port    : Int,
  override val priority: Int,
) extends WBPortConfig

object WBSeq {
  def apply(elems: WBPortConfig*): Seq[WBPortConfig] = {
    elems
  }
}

case class ExeUnitParams(
  fuConfigs: Seq[FuConfig],
  wbPortConfigs: Seq[WBPortConfig],
)(implicit
  val schdType: SchedulerType,
) {
  val numIntSrc     : Int = fuConfigs.map(_.numIntSrc).max
  val numFpSrc      : Int = fuConfigs.map(_.numFpSrc ).max
  val numVecSrc     : Int = fuConfigs.map(_.numVecSrc).max
  val numSrc        : Int = numIntSrc max numFpSrc max numVecSrc
  val dataBits      : Int = fuConfigs.map(_.dataBits ).max
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
    Cat(fuConfigs.map(_.fuType.U === fuType)).orR
  }

  def hasUncertainLatency: Boolean = fuConfigs.map(_.latency.latencyVal.isEmpty).reduce(_ || _)

  def getWbParamsInner: Seq[Seq[WriteBackConfig]] = {
    this.fuConfigs.map(cfg => {
      val res = Seq()
      if (cfg.writeIntRf) res :+ WriteBackConfig(getWBSource, IntWB(-1, 0))
      if (cfg.writeFpRf)  res :+ WriteBackConfig(getWBSource, VfWB(-1, 0))
      if (cfg.writeVecRf) res :+ WriteBackConfig(getWBSource, VfWB(-1, 0))
      res
    })
  }

  def getWbParamsOuter: Seq[WriteBackConfig] = {
    val res = Seq()
    if (writeIntRf)
      res :+ WriteBackConfig(getWBSource, wbPortConfigs.filter(_.isInstanceOf[IntWB]).head)
    if (writeFpRf || writeVecRf)
      res :+ WriteBackConfig(getWBSource, wbPortConfigs.filter(_.isInstanceOf[VfWB]).head)
    res
  }

  def getRfReadDataCfgSet: Seq[Set[DataConfig]] = {
    val fuSrcsCfgSet: Seq[Seq[Set[DataConfig]]] = fuConfigs.map(_.getRfReadDataCfgSet)
    val alignedFuSrcsCfgSet: Seq[Seq[Set[DataConfig]]] = fuSrcsCfgSet.map(x => x ++ Seq.fill(numSrc - x.length)(Set[DataConfig]()))

    val exuSrcsCfgSet = alignedFuSrcsCfgSet.reduce((x, y) => (x zip y).map { case (cfg1, cfg2) => cfg1 union cfg2 } )

    exuSrcsCfgSet
  }

  def genExuInputBundle: ExuInput = {
    Output(new ExuInput(this))
  }

  def genExuOutputBundle: ExuOutput = {
    Output(new ExuOutput(this))
  }

  def hasBrhFu = fuConfigs.map(_.fuType == FuType.brh).reduce(_ || _)
  def hasJmpFu = fuConfigs.map(_.fuType == FuType.jmp).reduce(_ || _)
  def hasLoadFu = fuConfigs.map(_.fuType == FuType.ldu).reduce(_ || _)
  def hasStoreFu = fuConfigs.map(_.fuType == FuType.stu).reduce(_ || _)
}



