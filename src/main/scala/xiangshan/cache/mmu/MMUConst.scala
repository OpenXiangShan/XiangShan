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
  missSameCycle: Boolean = false,
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
  shouldBlock: Boolean = false, // only for perf, not support for io
  partialStaticPMP: Boolean = false, // partila static pmp result stored in entries
  saveLevel: Boolean = false
)

case class L2TLBParameters
(
  name: String = "l2tlb",
  // l1
  l1Size: Int = 16,
  l1Associative: String = "fa",
  l1Replacer: Option[String] = Some("plru"),
  // l2
  l2nSets: Int = 32,
  l2nWays: Int = 2,
  l2Replacer: Option[String] = Some("setplru"),
  // l3
  l3nSets: Int = 128,
  l3nWays: Int = 4,
  l3Replacer: Option[String] = Some("setplru"),
  // sp
  spSize: Int = 16,
  spReplacer: Option[String] = Some("plru"),
  // dtlb filter
  filterSize: Int = 8,
  // miss queue, add more entries than 'must require'
  // 0 for easier bug trigger, please set as big as u can, 8 maybe
  missqueueExtendSize: Int = 0,
  // llptw
  llptwsize: Int = 6,
  // way size
  blockBytes: Int = 64,
  // prefetch
  enablePrefetch: Boolean = true,
  // ecc
  ecc: Option[String] = Some("secded")
)

trait HasTlbConst extends HasXSParameter {
  val Level = 3

  val offLen  = 12
  val ppnLen  = PAddrBits - offLen
  val vpnnLen = 9
  val vpnLen  = VAddrBits - offLen
  val flagLen = 8
  val pteResLen = XLEN - ppnLen - 2 - flagLen

  val sramSinglePort = true

  val timeOutThreshold = 10000

  def get_set_idx(vpn: UInt, nSets: Int): UInt = {
    require(nSets >= 1)
    vpn(log2Up(nSets)-1, 0)
  }

  def drop_set_idx(vpn: UInt, nSets: Int): UInt = {
    require(nSets >= 1)
    require(vpn.getWidth > log2Ceil(nSets))
    vpn(vpn.getWidth-1, log2Ceil(nSets))
  }

  def drop_set_equal(vpn1: UInt, vpn2: UInt, nSets: Int): Bool = {
    require(nSets >= 1)
    require(vpn1.getWidth == vpn2.getWidth)
    if (vpn1.getWidth <= log2Ceil(nSets)) {
      true.B
    } else {
      drop_set_idx(vpn1, nSets) === drop_set_idx(vpn2, nSets)
    }
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
  val sourceWidth = { if (l2tlbParams.enablePrefetch) PtwWidth + 1 else PtwWidth}
  val prefetchID = PtwWidth
  val maxPrefetchNum = l2tlbParams.filterSize

  val blockBits = l2tlbParams.blockBytes * 8

  val bPtwWidth = log2Up(PtwWidth)
  val bSourceWidth = log2Up(sourceWidth)
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

  // miss queue
  val MSHRBaseSize = 1 + l2tlbParams.filterSize + l2tlbParams.missqueueExtendSize
  val MSHRSize =  { if (l2tlbParams.enablePrefetch) (MSHRBaseSize + 1) else MSHRBaseSize }
  val MemReqWidth = l2tlbParams.llptwsize + 1
  val FsmReqID = l2tlbParams.llptwsize
  val bMemID = log2Up(MemReqWidth)

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

  def get_next_line(vpn: UInt) = {
    Cat(dropL3SectorBits(vpn) + 1.U, 0.U(PtwL3SectorIdxLen.W))
  }

  def same_l2entry(vpn1: UInt, vpn2: UInt) = {
    vpn1(vpnLen-1, vpnnLen) === vpn2(vpnLen-1, vpnnLen)
  }

  def from_pre(source: UInt) = {
    (source === prefetchID.U)
  }

  def printVec[T <: Data](x: Seq[T]): Printable = {
    (0 until x.length).map(i => p"(${i.U})${x(i)} ").reduce(_+_)
  }
}
