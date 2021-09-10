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

package xiangshan.cache.mmu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._

case class TLBParameters
(
  name: String = "none",
  fetchi: Boolean = false, // TODO: remove it
  useDmode: Boolean = true,
  sameCycle: Boolean = false,
  normalNSets: Int = 1, // when da or sa
  normalNWays: Int = 8, // when fa or sa
  superNSets: Int = 1,
  superNWays: Int = 2,
  normalReplacer: Option[String] = Some("random"),
  superReplacer: Option[String] = Some("plru"),
  normalAssociative: String = "fa", // "fa", "sa", "da", "sa" is not supported
  superAssociative: String = "fa", // must be fa
  normalAsVictim: Boolean = false, // when get replace from fa, store it into sram
  outReplace: Boolean = false,
  shouldBlock: Boolean = false // only for perf, not support for io
)

case class L2TLBParameters
(
  name: String = "l2tlb",
  // l1
  l1Size: Int = 16,
  l1Associative: String = "fa",
  l1Replacer: Option[String] = Some("plru"),
  // l2
  l2nSets: Int = 8,
  l2nWays: Int = 4,
  l2Replacer: Option[String] = Some("setplru"),
  // l3
  l3nSets: Int = 128,
  l3nWays: Int = 4,
  l3Replacer: Option[String] = Some("setplru"),
  // sp
  spSize: Int = 16,
  spReplacer: Option[String] = Some("plru"),
  // miss queue
  missQueueSize: Int = 9,
  // way size
  blockBytes: Int = 64
)

trait HasTlbConst extends HasXSParameter {
  val Level = 3

  val offLen  = 12
  val ppnLen  = PAddrBits - offLen
  val vpnnLen = 9
  val vpnLen  = VAddrBits - offLen
  val flagLen = 8
  val pteResLen = XLEN - ppnLen - 2 - flagLen
  val asidLen = 16

  val sramSinglePort = true

  val timeOutThreshold = 2000

  def get_idx(vpn: UInt, nSets: Int): UInt = {
    vpn(log2Up(nSets)-1, 0)
  }

  def replaceWrapper(v: UInt, lruIdx: UInt): UInt = {
    val width = v.getWidth
    val emptyIdx = ParallelPriorityMux((0 until width).map( i => (!v(i), i.U)))
    val full = Cat(v).andR
    Mux(full, lruIdx, emptyIdx)
  }

  def replaceWrapper(v: Seq[Bool], lruIdx: UInt): UInt = {
    replaceWrapper(VecInit(v).asUInt, lruIdx)
  }

}

trait HasPtwConst extends HasTlbConst with MemoryOpConstants{
  val PtwWidth = 2
  val blockBits = l2tlbParams.blockBytes * 8

  val bPtwWidth = log2Up(PtwWidth)

  // ptwl1: fully-associated
  val PtwL1TagLen = vpnnLen

  /* +-------+----------+-------------+
   * |  Tag  |  SetIdx  |  SectorIdx  |
   * +-------+----------+-------------+
   */
  // ptwl2: 8-way group-associated
  val l2tlbParams.l2nWays = l2tlbParams.l2nWays
  val PtwL2SetNum = l2tlbParams.l2nSets
  val PtwL2SectorSize = blockBits /XLEN
  val PtwL2IdxLen = log2Up(PtwL2SetNum * PtwL2SectorSize)
  val PtwL2SectorIdxLen = log2Up(PtwL2SectorSize)
  val PtwL2SetIdxLen = log2Up(PtwL2SetNum)
  val PtwL2TagLen = vpnnLen * 2 - PtwL2IdxLen

  // ptwl3: 16-way group-associated
  val l2tlbParams.l3nWays = l2tlbParams.l3nWays
  val PtwL3SetNum = l2tlbParams.l3nSets
  val PtwL3SectorSize =  blockBits / XLEN
  val PtwL3IdxLen = log2Up(PtwL3SetNum * PtwL3SectorSize)
  val PtwL3SectorIdxLen = log2Up(PtwL3SectorSize)
  val PtwL3SetIdxLen = log2Up(PtwL3SetNum)
  val PtwL3TagLen = vpnnLen * 3 - PtwL3IdxLen

  // super page, including 1GB and 2MB page
  val SPTagLen = vpnnLen * 2

  val MSHRSize = l2tlbParams.missQueueSize
  val MemReqWidth = MSHRSize + 1
  val bMemID = log2Up(MSHRSize + 1)

  def genPtwL2Idx(vpn: UInt) = {
    (vpn(vpnLen - 1, vpnnLen))(PtwL2IdxLen - 1, 0)
  }

  def genPtwL2SectorIdx(vpn: UInt) = {
    genPtwL2Idx(vpn)(PtwL2SectorIdxLen - 1, 0)
  }

  def genPtwL2SetIdx(vpn: UInt) = {
    genPtwL2Idx(vpn)(PtwL2SetIdxLen + PtwL2SectorIdxLen - 1, PtwL2SectorIdxLen)
  }

  def genPtwL3Idx(vpn: UInt) = {
    vpn(PtwL3IdxLen - 1, 0)
  }

  def genPtwL3SectorIdx(vpn: UInt) = {
    genPtwL3Idx(vpn)(PtwL3SectorIdxLen - 1, 0)
  }

  def dropL3SectorBits(vpn: UInt) = {
    vpn(vpn.getWidth-1, PtwL3SectorIdxLen)
  }

  def genPtwL3SetIdx(vpn: UInt) = {
    genPtwL3Idx(vpn)(PtwL3SetIdxLen + PtwL3SectorIdxLen - 1, PtwL3SectorIdxLen)
  }

  def MakeAddr(ppn: UInt, off: UInt) = {
    require(off.getWidth == 9)
    Cat(ppn, off, 0.U(log2Up(XLEN/8).W))(PAddrBits-1, 0)
  }

  def getVpnn(vpn: UInt, idx: Int): UInt = {
    vpn(vpnnLen*(idx+1)-1, vpnnLen*idx)
  }

  def getVpnClip(vpn: UInt, level: Int) = {
    // level 0  /* vpnn2 */
    // level 1  /* vpnn2 * vpnn1 */
    // level 2  /* vpnn2 * vpnn1 * vpnn0*/
    vpn(vpnLen - 1, (2 - level) * vpnnLen)
  }

  def printVec[T <: Data](x: Seq[T]): Printable = {
    (0 until x.length).map(i => p"(${i.U})${x(i)} ").reduce(_+_)
  }

}
