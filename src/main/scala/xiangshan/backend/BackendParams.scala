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

package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.RdConfig._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.datapath.{WakeUpConfig, WbArbiterParams}
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.issue._
import xiangshan.backend.regfile._

import scala.reflect.{ClassTag, classTag}

case class BackendParams(
  schdParams : Map[SchedulerType, SchdBlockParams],
  pregParams : Seq[PregParams],
  iqWakeUpParams : Seq[WakeUpConfig],
) {

  configChecks

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
  def getPregParams: Map[DataConfig, PregParams] = {
    pregParams.map(x => (x.dataCfg, x)).toMap
  }

  def pregIdxWidth = pregParams.map(_.addrWidth).max

  def numSrc      : Int = allSchdParams.map(_.issueBlockParams.map(_.numSrc).max).max
  def numRegSrc   : Int = allSchdParams.map(_.issueBlockParams.map(_.numRegSrc).max).max
  def numVecRegSrc: Int = allSchdParams.map(_.issueBlockParams.map(_.numVecSrc).max).max


  def AluCnt = allSchdParams.map(_.AluCnt).sum
  def StaCnt = allSchdParams.map(_.StaCnt).sum
  def StdCnt = allSchdParams.map(_.StdCnt).sum
  def LduCnt = allSchdParams.map(_.LduCnt).sum
  def VlduCnt = allSchdParams.map(_.VlduCnt).sum
  def LsExuCnt = StaCnt + LduCnt
  def JmpCnt = allSchdParams.map(_.JmpCnt).sum
  def BrhCnt = allSchdParams.map(_.BrhCnt).sum
  def CsrCnt = allSchdParams.map(_.CsrCnt).sum
  def IqCnt = allSchdParams.map(_.issueBlockParams.length).sum

  def numPcReadPort = allSchdParams.map(_.numPcReadPort).sum
  def numTargetReadPort = allExuParams.count(x => x.needTarget)

  def numPregRd(dataCfg: DataConfig) = this.getRfReadSize(dataCfg)
  def numPregWb(dataCfg: DataConfig) = this.getRfWriteSize(dataCfg)

  def numNoDataWB = allSchdParams.map(_.numNoDataWB).sum
  def numExu = allSchdParams.map(_.numExu).sum
  def vconfigPort = 0 // Todo: remove it

  def numException = allExuParams.count(_.exceptionOut.nonEmpty)

  def numRedirect = allSchdParams.map(_.numRedirect).sum

  def genIntWriteBackBundle(implicit p: Parameters) = {
    Seq.fill(this.getIntRfWriteSize)(new RfWritePortWithConfig(IntData(), intPregParams.addrWidth))
  }

  def genVfWriteBackBundle(implicit p: Parameters) = {
    Seq.fill(this.getVfRfWriteSize)(new RfWritePortWithConfig(VecData(), vfPregParams.addrWidth))
  }

  def genWriteBackBundles(implicit p: Parameters): Seq[RfWritePortWithConfig] = {
    genIntWriteBackBundle ++ genVfWriteBackBundle
  }

  def genWrite2CtrlBundles(implicit p: Parameters): MixedVec[ValidIO[ExuOutput]] = {
    MixedVec(allSchdParams.map(_.genExuOutputValidBundle.flatten).flatten)
  }

  def getIntWbArbiterParams: WbArbiterParams = {
    val intWbCfgs: Seq[IntWB] = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(_.writeInt)).map(_.asInstanceOf[IntWB])
    datapath.WbArbiterParams(intWbCfgs, intPregParams, this)
  }

  def getVfWbArbiterParams: WbArbiterParams = {
    val vfWbCfgs: Seq[VfWB] = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(x => x.writeVec || x.writeFp)).map(_.asInstanceOf[VfWB])
    datapath.WbArbiterParams(vfWbCfgs, vfPregParams, this)
  }

  /**
    * Get regfile read port params
    *
    * @param dataCfg [[IntData]] or [[VecData]]
    * @return Seq[port->Seq[(exuIdx, priority)]
    */
  def getRdPortParams(dataCfg: DataConfig) = {
    // port -> Seq[exuIdx, priority]
    val cfgs: Seq[(Int, Seq[(Int, Int)])] = allExuParams
      .flatMap(x => x.rfrPortConfigs.flatten.map(xx => (xx, x.exuIdx)))
      .filter { x => x._1.getDataConfig == dataCfg }
      .map(x => (x._1.port, (x._2, x._1.priority)))
      .groupBy(_._1)
      .map(x => (x._1, x._2.map(_._2).sortBy({ case (priority, _) => priority })))
      .toSeq
      .sortBy(_._1)
    cfgs
  }

  /**
    * Get regfile write back port params
    *
    * @param dataCfg [[IntData]] or [[VecData]]
    * @return Seq[port->Seq[(exuIdx, priority)]
    */
  def getWbPortParams(dataCfg: DataConfig) = {
    val cfgs: Seq[(Int, Seq[(Int, Int)])] = allExuParams
      .flatMap(x => x.wbPortConfigs.map(xx => (xx, x.exuIdx)))
      .filter { x => x._1.dataCfg == dataCfg }
      .map(x => (x._1.port, (x._2, x._1.priority)))
      .groupBy(_._1)
      .map(x => (x._1, x._2.map(_._2)))
      .toSeq
      .sortBy(_._1)
    cfgs
  }

  def getRdPortIndices(dataCfg: DataConfig) = {
    this.getRdPortParams(dataCfg).map(_._1)
  }

  def getWbPortIndices(dataCfg: DataConfig) = {
    this.getWbPortParams(dataCfg).map(_._1)
  }

  def getRdCfgs[T <: RdConfig](implicit tag: ClassTag[T]): Seq[Seq[Seq[RdConfig]]] = {
    val rdCfgs: Seq[Seq[Seq[RdConfig]]] = allIssueParams.map(
      _.exuBlockParams.map(
        _.rfrPortConfigs.map(
          _.collectFirst{ case x: T => x }
            .getOrElse(NoRD())
        )
      )
    )
    rdCfgs
  }

  def getAllWbCfgs: Seq[Seq[Set[PregWB]]] = {
    allIssueParams.map(_.exuBlockParams.map(_.wbPortConfigs.toSet))
  }

  def getWbCfgs[T <: PregWB](implicit tag: ClassTag[T]): Seq[Seq[PregWB]] = {
    val wbCfgs: Seq[Seq[PregWB]] = allIssueParams.map(_.exuBlockParams.map(_.wbPortConfigs.collectFirst{ case x: T => x }.getOrElse(NoWB())))
    wbCfgs
  }

  /**
    * Get size of read ports of int regfile
    *
    * @return if [[IntPregParams.numRead]] is [[None]], get size of ports in [[IntRD]]
    */
  def getIntRfReadSize = {
    this.intPregParams.numRead.getOrElse(this.getRdPortIndices(IntData()).size)
  }

  /**
    * Get size of write ports of vf regfile
    *
    * @return if [[IntPregParams.numWrite]] is [[None]], get size of ports in [[IntWB]]
    */
  def getIntRfWriteSize = {
    this.intPregParams.numWrite.getOrElse(this.getWbPortIndices(IntData()).size)
  }

  /**
    * Get size of read ports of int regfile
    *
    * @return if [[VfPregParams.numRead]] is [[None]], get size of ports in [[VfRD]]
    */
  def getVfRfReadSize = {
    this.vfPregParams.numRead.getOrElse(this.getRdPortIndices(VecData()).size)
  }

  /**
    * Get size of write ports of vf regfile
    *
    * @return if [[VfPregParams.numWrite]] is [[None]], get size of ports in [[VfWB]]
    */
  def getVfRfWriteSize = {
    this.vfPregParams.numWrite.getOrElse(this.getWbPortIndices(VecData()).size)
  }

  def getRfReadSize(dataCfg: DataConfig) = {
    this.getPregParams(dataCfg).numRead.getOrElse(this.getRdPortIndices(dataCfg).size)
  }

  def getRfWriteSize(dataCfg: DataConfig) = {
    this.getPregParams(dataCfg).numWrite.getOrElse(this.getWbPortIndices(dataCfg).size)
  }

  def getExuIdx(name: String): Int = {
    val exuParams = allExuParams
    if (name != "WB") {
      val foundExu = exuParams.find(_.name == name)
      require(foundExu.nonEmpty, s"exu $name not find")
      foundExu.get.exuIdx
    } else
      -1
  }

  def getExuName(idx: Int): String = {
    val exuParams = allExuParams
    exuParams(idx).name
  }

  def getIntWBExeGroup: Map[Int, Seq[ExeUnitParams]] = allExuParams.groupBy(x => x.getIntWBPort.getOrElse(IntWB(port = -1)).port).filter(_._1 != -1)
  def getVfWBExeGroup: Map[Int, Seq[ExeUnitParams]] = allExuParams.groupBy(x => x.getVfWBPort.getOrElse(VfWB(port = -1)).port).filter(_._1 != -1)

  private def isContinuous(portIndices: Seq[Int]): Boolean = {
    val portIndicesSet = portIndices.toSet
    portIndicesSet.min == 0 && portIndicesSet.max == portIndicesSet.size - 1
  }

  def configChecks = {
    checkReadPortContinuous
    checkWritePortContinuous
    configCheck
  }

  def checkReadPortContinuous = {
    pregParams.foreach { x =>
      if (x.numRead.isEmpty) {
        val portIndices: Seq[Int] = getRdPortIndices(x.dataCfg)
        require(isContinuous(portIndices),
          s"The read ports of ${x.getClass.getSimpleName} should be continuous, " +
            s"when numRead of ${x.getClass.getSimpleName} is None. The read port indices are $portIndices")
      }
    }
  }

  def checkWritePortContinuous = {
    pregParams.foreach { x =>
      if (x.numWrite.isEmpty) {
        val portIndices: Seq[Int] = getWbPortIndices(x.dataCfg)
        require(
          isContinuous(portIndices),
          s"The write ports of ${x.getClass.getSimpleName} should be continuous, " +
            s"when numWrite of ${x.getClass.getSimpleName} is None. The write port indices are $portIndices"
        )
      }
    }
  }

  def configCheck = {
    // check 0
    val maxPortSource = 4

    allExuParams.map {
      case exuParam => exuParam.wbPortConfigs.collectFirst { case x: IntWB => x }
    }.filter(_.isDefined).groupBy(_.get.port).foreach {
      case (wbPort, priorities) => assert(priorities.size <= maxPortSource, "There has " + priorities.size + " exu's " + "Int WBport is " + wbPort + ", but the maximum is " + maxPortSource + ".")
    }
    allExuParams.map {
      case exuParam => exuParam.wbPortConfigs.collectFirst { case x: VfWB => x }
    }.filter(_.isDefined).groupBy(_.get.port).foreach {
      case (wbPort, priorities) => assert(priorities.size <= maxPortSource, "There has " + priorities.size + " exu's " + "Vf  WBport is " + wbPort + ", but the maximum is " + maxPortSource + ".")
    }

    // check 1
    val wbTypes = Seq(IntWB(), VfWB())
    val rdTypes = Seq(IntRD(), VfRD())
    for(wbType <- wbTypes){
      for(rdType <- rdTypes){
        allExuParams.map {
          case exuParam =>
            val wbPortConfigs = exuParam.wbPortConfigs
            val wbConfigs = wbType match{
              case _: IntWB => wbPortConfigs.collectFirst { case x: IntWB => x }
              case _: VfWB  => wbPortConfigs.collectFirst { case x: VfWB => x }
              case _        => None
            }
            val rfReadPortConfigs = exuParam.rfrPortConfigs
            val rdConfigs = rdType match{
              case _: IntRD => rfReadPortConfigs.flatten.filter(_.isInstanceOf[IntRD])
              case _: VfRD  => rfReadPortConfigs.flatten.filter(_.isInstanceOf[VfRD])
              case _        => Seq()
            }
            (wbConfigs, rdConfigs)
        }.filter(_._1.isDefined)
          .sortBy(_._1.get.priority)
          .groupBy(_._1.get.port).map {
            case (_, intWbRdPairs) =>
              intWbRdPairs.map(_._2).flatten
        }.map(rdCfgs => rdCfgs.groupBy(_.port).foreach {
          case (_, rdCfgs) =>
            rdCfgs.zip(rdCfgs.drop(1)).foreach { case (cfg0, cfg1) => assert(cfg0.priority <= cfg1.priority) }
        })
      }
    }
  }
}




