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
import xiangshan.backend.datapath.WbArbiterParams
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.issue._
import xiangshan.backend.regfile._

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

  def numSrc    : Int = allSchdParams.map(_.issueBlockParams.map(_.numSrc).max).max
  def numRegSrc : Int = allSchdParams.map(_.issueBlockParams.map(_.numRegSrc).max).max

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
  def numRfRead  = 14
  def numRfWrite = 8
  def vconfigPort = 11

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
    val intWbCfgs: Seq[WbConfig] = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(_.writeInt))
    datapath.WbArbiterParams(intWbCfgs, intPregParams)
  }

  def getVfWbArbiterParams: WbArbiterParams = {
    val vfWbCfgs = allSchdParams.flatMap(_.getWbCfgs.flatten.flatten.filter(x => x.writeVec || x.writeFp))
    datapath.WbArbiterParams(vfWbCfgs, vfPregParams)
  }
}




