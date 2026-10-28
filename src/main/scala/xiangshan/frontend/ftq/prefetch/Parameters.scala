// Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.ftq.prefetch

import chisel3._
import chisel3.util._
import utils.AddrField
import xiangshan.frontend.Pc
import xiangshan.frontend.PcInit
import xiangshan.frontend.ftq.HasFtqParameters

case class PrefetchQueueParameters(
    Size: Int = 8,
    // when distanceBetween(pfPtr, fetchPtr), prefer 1-fdip over 2- or 1-queued
    PreferFdipDistance: Int = 2
) {
  require(isPow2(Size), "Size must is power of 2")
  require(PreferFdipDistance >= 1, "PreferFdipDistance must >= 1")
}

trait HasPrefetchQueueParameters extends HasFtqParameters {
  def pqParameters: PrefetchQueueParameters = ftqParameters.pqParameters

  def Size: Int = pqParameters.Size

  def PreferFdipDistance: Int = pqParameters.PreferFdipDistance

  def EnqueueSwNum:   Int = backendParams.LduCnt // every load unit may send software prefetch.i request to pq
  def EnqueuePdipNum: Int = ftqParameters.pdipParameters.NumAdjacent
  def EnqueueNum:     Int = EnqueueSwNum         // TODO: more prefetch source
}

case class PdipParameters(
    NumSet:      Int = 512,
    NumWay:      Int = 2,
    TagWidth:    Int = 10,
    NumAdjacent: Int = 2
) {
  require(isPow2(NumSet), "NumSet must is power of 2")
  require(NumWay >= 1, "NumWay must >= 1")
  require(TagWidth >= 1, "TagWidth must >= 1")
  require(NumAdjacent >= 1, "NumAdjacent must >= 1")
}

trait HasPdipParameters extends HasFtqParameters {
  def pdipParameters: PdipParameters = ftqParameters.pdipParameters

  def BlockOffsetWidth: Int = log2Ceil(frontendParameters.icacheParameters.blockBytes)

  def NumSet:      Int = pdipParameters.NumSet
  def SetIdxWidth: Int = log2Ceil(NumSet)
  def NumWay:      Int = pdipParameters.NumWay
  def TagWidth:    Int = pdipParameters.TagWidth
  def NumAdjacent: Int = pdipParameters.NumAdjacent

  def BlockVAddrWidth: Int = VAddrBits - BlockOffsetWidth

  val addrFields = AddrField(
    Seq(
      ("blockOffset", BlockOffsetWidth),
      ("setIdx", SetIdxWidth),
      ("tag", TagWidth)
    ),
    maxWidth = Option(VAddrBits),
    extraFields = Seq(
      ("blockVAddr", BlockOffsetWidth, BlockVAddrWidth)
    )
  )

  def getSetIdx(pc: Pc): UInt = addrFields.extract("setIdx", pc)

  def getTag(pc: Pc): UInt = addrFields.extract("tag", pc)

  def getBlockVAddr(pc: Pc): UInt = addrFields.extract("blockVAddr", pc)

  def getPc(blockVAddr: UInt): Pc = PcInit(Cat(blockVAddr, 0.U(BlockOffsetWidth.W)))
}
