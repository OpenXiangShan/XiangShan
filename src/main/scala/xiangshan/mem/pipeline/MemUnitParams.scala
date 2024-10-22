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
package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.mem.Bundles.LsPipelineBundle
import javax.sound.midi.Sequence

sealed trait MemUnitType

case class StoreDataUnit() extends MemUnitType

case class MemUnitParams(
  name:             String        = "MemUnit",
  unitType:         MemUnitType,
  dataBits:         Int           = 128, // ignore it, don't change it
  issueParams:      Seq[MemIssueParams],
  hasPrefetch:      Boolean       = false
) {

  def isStoreDataUnit: Boolean = unitType == StoreDataUnit()

  def exceptionOut: Seq[Int] = issueParams.map(_.exceptionOut).reduce(_++_).distinct

  def numWBPorts: Int = issueParams.filter(_.getPort() >= 0).length

  def hasTrigger: Boolean = issueParams.filter(_.trigger).length > 0

  def hasMisalignExe: Boolean = issueParams.filter(_.isMisalignBuf).length > 0

  def hasStoreAddrExe: Boolean = issueParams.filter(_.hasStoreAddrExe).length > 0

  def hasLoadExe: Boolean = issueParams.filter(_.hasLoadExe).length > 0

  def hasDCacheQuery: Boolean = issueParams.filter(_.hasDCacheQuery).length > 0

  def iqIssueParams: Seq[MemIssueParams] = issueParams.filter(_.isIq)

  def numIqIssue: Int = iqIssueParams.length

  def iqWbPorts: Seq[Int] = iqIssueParams.map(_.getPort()).distinct

  def prefetchIssueParams: Seq[MemIssueParams] = issueParams.filter(_.isPrefetch)

  def numPrefetchIssue: Int = prefetchIssueParams.length

  def misalignBufIssueParams: Seq[MemIssueParams] = issueParams.filter(_.isMisalignBuf)

  def misalignBufWbPorts: Seq[Int] = misalignBufIssueParams.map(_.getPort()).distinct

  def numMisalignBufIssue: Int = misalignBufIssueParams.length

  def vectorIssueParams: Seq[MemIssueParams] = issueParams.filter(_.isVector)

  def vectorWbPorts: Seq[Int] = vectorIssueParams.map(_.getPort()).distinct

  def numVectorIssue: Int = vectorIssueParams.length

  def loadIssueParams: Seq[MemIssueParams] = issueParams.filter(_.isLoad)

  def loadWbPorts: Seq[Int] = loadIssueParams.map(_.getPort()).distinct

  def numLoadIssue: Int = loadIssueParams.length

  def storeIssueParams: Seq[MemIssueParams] = issueParams.filter(_.isStore)

  def storeWbPorts: Seq[Int] = storeIssueParams.map(_.getPort()).distinct

  def numStoreIssue: Int = storeIssueParams.length

  def atomicsParams: Seq[MemIssueParams] = issueParams.filter(_.isAtomics)

  def numAtomicsIssue: Int = atomicsParams.length

  def atomicsWbPorts: Seq[Int] = atomicsParams.map(_.getPort()).distinct

  def genLsPipelineSourceDecoupledBundle(implicit p: Parameters): MixedVec[DecoupledIO[LsPipelineBundle]] = {
    // Warning: the name of the same port index which will be overrided by Map.++
    val wbPortMap = issueParams.map(_.wbPort).reduce(_++_).filter(x => x._1 >= 0).toSeq.sortBy(_._1)
    MixedVec(wbPortMap.map {
      case (port, portName) =>
        DecoupledIO(new LsPipelineBundle()).suggestName(portName.getOrElse("out" + port))
    })
  }

  def genLsPipelineSinkDecoupledBundle(implicit p: Parameters): MixedVec[DecoupledIO[LsPipelineBundle]] = {
    MixedVec(issueParams.map(x => Flipped(DecoupledIO(new LsPipelineBundle().suggestName(x.name)))))
  }

  def unitTypeString: String = unitType match {
    case StoreDataUnit() => "Store Data"
    case _               => "unknown"
  }
}