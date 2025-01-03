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
import scala.collection.mutable

object MemIssueType {
  private val types: mutable.Set[(Int, String)] = mutable.Set()

  private var initVal = 0

  private def addType(name: String): Int = {
    val newType = initVal
    types += (initVal -> name)
    initVal += 1
    newType
  }

  val Std  = addType(name = "store data")
  val Sta  = addType(name = "store addr")
  val Ld   = addType(name = "scalar load")
  val Vld  = addType(name = "vector load")
  val Vst  = addType(name = "vector store")
  val Amo  = addType(name = "atomics")
  val Mmio = addType(name = "mmio")
  val Nc   = addType(name = "non-cacheable")
  val Pf   = addType(name = "prefetch")
  val Mb   = addType(name = "misalign buffer")
  val Fp   = addType(name = "fast path")
  val Fr   = addType(name = "fast replay")
  val Lqr  = addType(name = "load queue replay")

  val numTypes: Int = types.size

  def apply(): UInt = UInt(numTypes.W)

  def apply(_type: Int): UInt = _type.U(numTypes.W)

  def isOneOf(_type: Int, types: Int*): Boolean = types.map(t => t == _type).reduce(_|_)

  def isStd(_type: Int): Boolean = _type == Std

  def isSta(_type: Int): Boolean = _type == Sta

  def isLd(_type: Int): Boolean = _type == Ld

  def isVld(_type: Int): Boolean = _type == Vld

  def isVst(_type: Int): Boolean = _type == Vst

  def isAmo(_type: Int): Boolean = _type == Amo

  def isMmio(_type: Int): Boolean = _type == Mmio

  def isNc(_type: Int): Boolean = _type == Nc

  def isPf(_type: Int): Boolean = _type == Pf

  def isMb(_type: Int): Boolean = _type == Mb

  def isFp(_type: Int): Boolean = _type == Fp

  def isFr(_type: Int): Boolean = _type == Fr

  def isLqr(_type: Int): Boolean = _type == Lqr

  def needVAddr(_type: Int): Boolean = !isOneOf(_type, Mmio, Std)

  def needTlbVAddr(_type: Int): Boolean = !isOneOf(_type, Mmio, Fr, Nc, Std)

  def needDCacheVAddr(_type: Int): Boolean = !isOneOf(_type, Mmio, Nc, Std)

  def needPAddr(_type: Int): Boolean = isOneOf(_type, Fr, Nc)
}

case class MemIssueParams(
  name:           String,
  issueType:      Int,
  trigger:        Boolean = false,
) {
  def hasAGen: Boolean = MemIssueType.isLd(issueType) || MemIssueType.isSta(issueType)

  def hasTlbQuery: Boolean = !(MemIssueType.isStd(issueType) || MemIssueType.isMmio(issueType))

  def hasDCacheQuery: Boolean = hasTlbQuery || MemIssueType.isPf(issueType)
}