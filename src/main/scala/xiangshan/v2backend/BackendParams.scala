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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.v2backend.Bundles.{ExuInput, ExuOutput, IssueQueueIssueBundle, WriteBackBundle}
import xiangshan.v2backend.exu._
import xiangshan.v2backend.issue.IssueQueueParams

abstract class PregParams {
  val numEntries: Int
  val numRead: Int
  val numWrite: Int
  val dataCfg: DataConfig
  def addrWidth = log2Up(numEntries)
}

case class IntPregParams(
  numEntries: Int,
  numRead   : Int,
  numWrite  : Int,
) extends PregParams {
  override val dataCfg: DataConfig = IntData()
}

case class VfPregParams(
  numEntries: Int,
  numRead   : Int,
  numWrite  : Int,
) extends PregParams {
  override val dataCfg: DataConfig = VecData()
}

case class WbArbiterParams(
  wbCfgs: Seq[WriteBackConfig],
  pregParams: PregParams,
) {

  def numIn = wbCfgs.length

  def numOut = pregParams.numWrite

  def dataWidth = pregParams.dataCfg.dataWidth

  def addrWidth = log2Up(pregParams.numEntries)

  def genInput(implicit p: Parameters) = {
    MixedVec(wbCfgs.map(x => DecoupledIO(new WriteBackBundle(x))))
  }

  def genOutput(implicit p: Parameters): MixedVec[ValidIO[WriteBackBundle]] = {
    Output(MixedVec(Seq.tabulate(numOut) {
      x => ValidIO(new WriteBackBundle(
        wbCfgs.head.dataCfg match {
          case IntData() => IntWB(port = x)
          case FpData() => FpWB(port = x)
          case VecData() => VecWB(port = x)
        }
      ))
    }))
  }
}

object BackendParams {
  def dummyParams()(implicit p: Parameters): BackendParams = {
    new BackendParams(Map(
      IntScheduler() -> SchdBlockParams.dummyIntParams(),
      VfScheduler() -> SchdBlockParams.dummyVfParams(),
      MemScheduler() -> SchdBlockParams.dummyMemParams(),
    ), Seq(
      IntPregParams(160, 14, 8),
      VfPregParams(160, 14, 8),
    ))
  }
}

case class BackendParams(
  schdParams : Map[SchedulerType, SchdBlockParams],
  pregParams : Seq[PregParams],
) {
  def intSchdParams = schdParams.get(IntScheduler())
  def vfSchdParams = schdParams.get(VfScheduler())
  def memSchdParams = schdParams.get(MemScheduler())
  def allSchdParams: Seq[SchdBlockParams] =
    (Seq(intSchdParams) :+ vfSchdParams :+ memSchdParams)
    .filter(_.nonEmpty)
    .map(_.get)
  def allIssueParams: Seq[IssueBlockParams] =
    allSchdParams.map(_.issueBlockParams).flatten
  def allExuParams: Seq[ExeUnitParams] =
    allIssueParams.map(_.exuBlockParams).flatten

  def intPregParams: IntPregParams = pregParams.collectFirst { case x: IntPregParams => x }.get
  def vfPregParams: VfPregParams = pregParams.collectFirst { case x: VfPregParams => x }.get

  def AluCnt = allSchdParams.map(_.AluCnt).sum
  def StaCnt = allSchdParams.map(_.StaCnt).sum
  def StdCnt = allSchdParams.map(_.StdCnt).sum
  def LduCnt = allSchdParams.map(_.LduCnt).sum
  def LsExuCnt = StaCnt + LduCnt
  def JmpCnt = allSchdParams.map(_.JmpCnt).sum
  def BrhCnt = allSchdParams.map(_.BrhCnt).sum
  def IqCnt = allSchdParams.map(_.issueBlockParams.length).sum

  def numPcReadPort = allSchdParams.map(_.numPcReadPort).sum

  def numIntWb = intPregParams.numWrite
  def numVfWb = vfPregParams.numWrite
  def numNoDataWB = allSchdParams.map(_.numNoDataWB).sum
  def numExu = allSchdParams.map(_.numExu).sum

  def numException = allExuParams.count(_.exceptionOut.nonEmpty)

  def numRedirect = allSchdParams.map(_.numRedirect).sum

  def genIntWriteBackBundle(implicit p: Parameters) = {
    // Todo: limit write port
    Seq.tabulate(numIntWb)(x => new RfWritePortWithConfig(IntData(), intPregParams.addrWidth))
  }

  def genVfWriteBackBundle(implicit p: Parameters) = {
    // Todo: limit write port
    Seq.tabulate(numVfWb)(x => new RfWritePortWithConfig(VecData(), intPregParams.addrWidth))
  }

  def genWriteBackBundles(implicit p: Parameters): Seq[RfWritePortWithConfig] = {
    genIntWriteBackBundle ++ genVfWriteBackBundle
  }

  def genWrite2CtrlBundles(implicit p: Parameters): MixedVec[ValidIO[ExuOutput]] = {
    MixedVec(allSchdParams.map(_.genExuOutputValidBundle.flatten).reduce(_ ++ _))
  }

  def getIntWbArbiterParams: WbArbiterParams = {
    val intWbCfgs: Seq[WriteBackConfig] = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(_.writeInt))
    WbArbiterParams(intWbCfgs, intPregParams)
  }

  def getVfWbArbiterParams: WbArbiterParams = {
    val vfWbCfgs = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(x => x.writeVec || x.writeFp))
    WbArbiterParams(vfWbCfgs, vfPregParams)
  }
}

object SchdBlockParams {
  def dummyIntParams(numDeqOutside: Int = 0)(implicit p: Parameters): SchdBlockParams = {
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
        ExeUnitParams(Seq(AluCfg, DivCfg, I2fCfg), Seq(IntWB(port = 2, 0), VecWB(port = 0, Int.MaxValue))),
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

  def dummyVfParams(numDeqOutside: Int = 0)(implicit p: Parameters): SchdBlockParams = {
    implicit val schdType: SchedulerType = VfScheduler()
    val numUopIn = 6
    val numRfRead = 12
    val numRfWrite = 8
    val numPregs = 160
    val pregIdxWidth = log2Up(numPregs)
    val rfDataWidth = 128

    var params = SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(VipuCfg), Seq(VecWB(port = 0, 0))),
        ExeUnitParams(Seq(VipuCfg), Seq(VecWB(port = 0, 0))),
      ), numEntries = 16, pregBits = pregIdxWidth, numWakeupFromWB = numRfWrite, numEnq = 4),
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(VfpuCfg, F2fCfg), Seq(VecWB(port = 0, 0))),
        ExeUnitParams(Seq(VfpuCfg, F2fCfg, F2iCfg), Seq(IntWB(port = 5, Int.MaxValue), VecWB(port = 0, 0))),
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

  def dummyMemParams()(implicit p: Parameters): SchdBlockParams = {
    implicit val schdType: SchedulerType = MemScheduler()
    val numUopIn = 6
    val numPregs = 160
    val pregBits = log2Up(numPregs)
    val rfDataWidth = 64

    var params = SchdBlockParams(Seq(
      IssueBlockParams(Seq(
        ExeUnitParams(Seq(LduCfg), WBSeq(IntWB(6, 0), VecWB(6, 0))),
        ExeUnitParams(Seq(LduCfg), WBSeq(IntWB(7, 0), VecWB(7, 0))),
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
  def isMemSchd   : Boolean = schdType == MemScheduler()
  def isIntSchd   : Boolean = schdType == IntScheduler()
  def isVfSchd    : Boolean = schdType == VfScheduler()

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
  def FDivSqrtCnt :Int = issueBlockParams.map(_.fDivSqrtCnt).sum

  def LduCnt      :Int = issueBlockParams.map(_.LduCnt).sum
  def StaCnt      :Int = issueBlockParams.map(_.StaCnt).sum
  def StdCnt      :Int = issueBlockParams.map(_.StdCnt).sum
  def MouCnt      :Int = issueBlockParams.map(_.MouCnt).sum

  def VipuCnt     :Int = issueBlockParams.map(_.VipuCnt).sum
  def VfpuCnt     :Int = issueBlockParams.map(_.VfpuCnt).sum
  def VlduCnt     :Int = issueBlockParams.map(_.VlduCnt).sum
  def VstuCnt     :Int = issueBlockParams.map(_.VstuCnt).sum

  def numExu      : Int = issueBlockParams.map(_.exuBlockParams.count(!_.hasStdFu)).sum

  def hasCSR = CsrCnt > 0
  def hasFence = FenceCnt > 0

  def numWriteIntRf: Int = issueBlockParams.map(_.numWriteIntRf).sum
  def numWriteFpRf : Int = issueBlockParams.map(_.numWriteFpRf ).sum
  def numWriteVecRf: Int = issueBlockParams.map(_.numWriteVecRf).sum
  def numWriteVfRf : Int = issueBlockParams.map(_.numWriteVfRf ).sum
  def numNoDataWB  : Int = issueBlockParams.map(_.numNoDataWB).sum

  def numPcReadPort = {
    val bjIssueQueues = issueBlockParams.filter(x => (x.JmpCnt + x.BrhCnt + x.FenceCnt) > 0)
    if(bjIssueQueues.map(x => x.numEnq).sum > 0) numUopIn else 0
  }
  def needSrcFrm: Boolean = issueBlockParams.map(_.needSrcFrm).reduce(_ || _)

  def numRedirect: Int = issueBlockParams.map(_.numRedirect).sum

  def pregIdxWidth: Int = log2Up(numPregs)

  def numWakeupFromWB: Int = schdType match {
    case IntScheduler() | VfScheduler() => 8
    case MemScheduler() => 16 // Todo
    case _ => 0
  }

  def numTotalIntRfRead: Int = issueBlockParams.map(_.exuBlockParams.map(_.numIntSrc).sum).sum
//  def numTotalVfRfRead: Int = issueBlockParams.map(_.exuBlockParams.map(x => x.numFpSrc + x.).sum).sum

  // Todo: 14R8W
  def numIntRfRead : Int = numTotalIntRfRead

  def genExuInputBundle(implicit p: Parameters): MixedVec[MixedVec[DecoupledIO[ExuInput]]] = {
    MixedVec(this.issueBlockParams.map(_.genExuInputDecoupledBundle))
  }

  def genExuOutputDecoupledBundle(implicit p: Parameters): MixedVec[MixedVec[DecoupledIO[ExuOutput]]] = {
    MixedVec(this.issueBlockParams.map(_.genExuOutputDecoupledBundle))
  }

  def genExuOutputValidBundle(implicit p: Parameters): MixedVec[MixedVec[ValidIO[ExuOutput]]] = {
    MixedVec(this.issueBlockParams.map(_.genExuOutputValidBundle))
  }

  // cfgs(issueIdx)(exuIdx)(set of exu's wb)
  def getWbCfgs: Seq[Seq[Set[WriteBackConfig]]] = {
    this.issueBlockParams.map(_.getWbCfgs)
  }
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
  def inMemSchd     : Boolean = schdType == MemScheduler()
  def inIntSchd     : Boolean = schdType == IntScheduler()
  def inVfSchd      : Boolean = schdType == VfScheduler()
  def isMemAddrIQ   : Boolean = inMemSchd && StdCnt == 0
  def isLdAddrIQ    : Boolean = inMemSchd && LduCnt > 0
  def isStAddrIQ    : Boolean = inMemSchd && StaCnt > 0

  def numExu        : Int = exuBlockParams.length
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
  def needPc        : Boolean = JmpCnt + BrhCnt + FenceCnt > 0
  def needSrcFrm    : Boolean = exuBlockParams.map(_.needSrcFrm).reduce(_ || _)
  def numPcReadPort : Int = (if (needPc) 1 else 0) * numEnq

  def numWriteIntRf : Int = exuBlockParams.count(_.writeIntRf)
  def numWriteFpRf  : Int = exuBlockParams.count(_.writeFpRf)
  def numWriteVecRf : Int = exuBlockParams.count(_.writeVecRf)
  def numWriteVfRf  : Int = exuBlockParams.count(_.writeVfRf)
  def numNoDataWB   : Int = exuBlockParams.count(_.hasNoDataWB)

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

  def LduCnt      :Int = exuBlockParams.map(_.fuConfigs.count(_.name == "ldu")).sum
  def StaCnt      :Int = exuBlockParams.map(_.fuConfigs.count(_.name == "sta")).sum
  def MouCnt      :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.mou)).sum
  def StdCnt      :Int = exuBlockParams.map(_.fuConfigs.count(_.name == "std")).sum

  def VipuCnt     :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.vipu)).sum
  def VfpuCnt     :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.vfpu)).sum
  def VlduCnt     :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.vldu)).sum
  def VstuCnt     :Int = exuBlockParams.map(_.fuConfigs.count(_.fuType == FuType.vstu)).sum

  def numRedirect: Int = exuBlockParams.count(_.hasRedirect)

  def numAllWakeUp = numWakeupFromWB + numWakeupFromIQ + numWakeupFromOthers
//  def getWbParams: Seq[Seq[WriteBackConfig]] = exuBlockParams.map(params => params.getWbParamsOuter)

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

  // cfgs(exuIdx)(set of exu's wb)
  def getWbCfgs: Seq[Set[WriteBackConfig]] = {
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

  def getIQName = {
    "IssueQueue" ++ getFuCfgs.map(_.name).distinct.map(_.capitalize).reduce(_ ++ _)
  }
}

//trait WBPortConfig {
//  val port: Int
//  val priority: Int
//}
//
//case class AnyWB(
//  override val port    : Int,
//) extends WBPortConfig {
//  override val priority: Int = Int.MaxValue
//}
//
//case class IntWB(
//  override val port    : Int,
//  override val priority: Int = Int.MaxValue,
//) extends WBPortConfig
//
//case class FpWB(
//  override val port    : Int,
//  override val priority: Int = Int.MaxValue,
//) extends WBPortConfig
//
//case class VfWB(
//  override val port    : Int,
//  override val priority: Int = Int.MaxValue,
//) extends WBPortConfig

object WBSeq {
  def apply(elems: WriteBackConfig*): Seq[WriteBackConfig] = {
    elems
  }
}

case class ExeUnitParams(
  fuConfigs: Seq[FuConfig],
  wbPortConfigs: Seq[WriteBackConfig],
)(implicit
  val schdType: SchedulerType,
) {
  val numIntSrc     : Int = fuConfigs.map(_.numIntSrc).max
  val numFpSrc      : Int = fuConfigs.map(_.numFpSrc ).max
  val numVecSrc     : Int = fuConfigs.map(_.numVecSrc).max
  val numSrc        : Int = numIntSrc max numFpSrc max numVecSrc
  val dataBitsMax   : Int = fuConfigs.map(_.dataBits ).max
  val readIntRf     : Boolean = numIntSrc > 0
  val readFpRf      : Boolean = numFpSrc  > 0
  val readVecRf     : Boolean = numVecSrc > 0
  val writeIntRf    : Boolean = fuConfigs.map(_.writeIntRf).reduce(_ || _)
  val writeFpRf     : Boolean = fuConfigs.map(_.writeFpRf ).reduce(_ || _)
  val writeVecRf    : Boolean = fuConfigs.map(_.writeVecRf).reduce(_ || _)
  val writeVfRf     : Boolean = writeFpRf || writeVecRf
  val writeFflags   : Boolean = fuConfigs.map(_.writeFflags).reduce(_ || _)
  val hasNoDataWB   : Boolean = fuConfigs.map(_.hasNoDataWB).reduce(_ || _)
  val hasRedirect   : Boolean = fuConfigs.map(_.hasRedirect).reduce(_ || _)
  val hasPredecode  : Boolean = fuConfigs.map(_.hasPredecode).reduce(_ || _)
  val exceptionOut  : Seq[Int] = fuConfigs.map(_.exceptionOut).reduce(_ ++ _).distinct.sorted
  val hasLoadError  : Boolean = fuConfigs.map(_.hasLoadError).reduce(_ || _)
  val flushPipe     : Boolean = fuConfigs.map(_.flushPipe).reduce(_ ||_)
  val replayInst    : Boolean = fuConfigs.map(_.replayInst).reduce(_ || _)
  val trigger       : Boolean = fuConfigs.map(_.trigger).reduce(_ || _)
  val needExceptionGen: Boolean = exceptionOut.nonEmpty || flushPipe || replayInst || trigger
  val needPc        : Boolean = fuConfigs.map(_.needPc).reduce(_ || _)
  val needSrcFrm    : Boolean = fuConfigs.map(_.needSrcFrm).reduce(_ || _)
  val needFPUCtrl   : Boolean = fuConfigs.map(_.needFPUCtrl).reduce(_ || _)
  val wbPregIdxWidth = if (wbPortConfigs.nonEmpty) wbPortConfigs.map(_.pregIdxWidth).max else 0

  def hasCSR: Boolean = fuConfigs.map(_.isCsr).reduce(_ || _)

  def hasFence: Boolean = fuConfigs.map(_.isFence).reduce(_ || _)

  def hasBrhFu = fuConfigs.map(_.fuType == FuType.brh).reduce(_ || _)

  def hasJmpFu = fuConfigs.map(_.fuType == FuType.jmp).reduce(_ || _)

  def hasLoadFu = fuConfigs.map(_.fuType == FuType.ldu).reduce(_ || _)

  def hasStoreFu = fuConfigs.map(_.name == "sta").reduce(_ || _)

  def hasStdFu = fuConfigs.map(_.name == "std").reduce(_ || _)

  def isMemAddrFu = hasLoadFu || hasStoreFu

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

  def getFpWBPort = {
    wbPortConfigs.collectFirst {
      case x: FpWB => x
    }
  }

  def getRfReadDataCfgSet: Seq[Set[DataConfig]] = {
    val fuSrcsCfgSet: Seq[Seq[Set[DataConfig]]] = fuConfigs.map(_.getRfReadDataCfgSet)
    val alignedFuSrcsCfgSet: Seq[Seq[Set[DataConfig]]] = fuSrcsCfgSet.map(x => x ++ Seq.fill(numSrc - x.length)(Set[DataConfig]()))

    val exuSrcsCfgSet = alignedFuSrcsCfgSet.reduce((x, y) => (x zip y).map { case (cfg1, cfg2) => cfg1 union cfg2 } )

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



