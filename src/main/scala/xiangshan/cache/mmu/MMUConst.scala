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

trait HasTlbConst extends HasXSParameter {
  val Level = 3

  val offLen  = 12
  val ppnLen  = PAddrBits - offLen
  val vpnnLen = 9
  val vpnLen  = VAddrBits - offLen
  val flagLen = 8
  val pteResLen = XLEN - ppnLen - 2 - flagLen
  val asidLen = 16

  def vaBundle = new Bundle {
    val vpn  = UInt(vpnLen.W)
    val off  = UInt(offLen.W)
  }
  def pteBundle = new Bundle {
    val reserved  = UInt(pteResLen.W)
    val ppn  = UInt(ppnLen.W)
    val rsw  = UInt(2.W)
    val perm = new Bundle {
      val d    = Bool()
      val a    = Bool()
      val g    = Bool()
      val u    = Bool()
      val x    = Bool()
      val w    = Bool()
      val r    = Bool()
      val v    = Bool()
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
  val MemBandWidth  = 256 // TODO: change to IO bandwidth param
  val SramSinglePort = true // NOTE: ptwl2, ptwl3 sram single port or not

  val bPtwWidth = log2Up(PtwWidth)

  // ptwl1: fully-associated
  val PtwL1TagLen = vpnnLen
  val ptwl1Replacer = Some("plru")

  /* +-------+----------+-------------+
   * |  Tag  |  SetIdx  |  SectorIdx  |
   * +-------+----------+-------------+
   */
  // ptwl2: 8-way group-associated
  val PtwL2WayNum = 8
  val PtwL2WaySize = PtwL2EntrySize / PtwL2WayNum
  val PtwL2SectorSize = MemBandWidth/XLEN
  val PtwL2LineSize = PtwL2SectorSize * PtwL2WayNum
  val PtwL2LineNum  = PtwL2EntrySize / PtwL2LineSize
  val PtwL2IdxLen = log2Up(PtwL2WaySize)
  val PtwL2SectorIdxLen = log2Up(PtwL2SectorSize)
  val PtwL2SetIdxLen = log2Up(PtwL2LineNum)
  val PtwL2TagLen = vpnnLen * 2 - PtwL2IdxLen
  val ptwl2Replacer = Some("setplru")

  // ptwl3: 16-way group-associated
  val PtwL3WayNum = 16
  val PtwL3WaySize = PtwL3EntrySize / PtwL3WayNum
  val PtwL3SectorSize = MemBandWidth / XLEN
  val PtwL3LineSize = PtwL3SectorSize * PtwL3WayNum
  val PtwL3LineNum  = PtwL3EntrySize / PtwL3LineSize
  val PtwL3IdxLen = log2Up(PtwL3WaySize)
  val PtwL3SectorIdxLen = log2Up(PtwL3SectorSize)
  val PtwL3SetIdxLen = log2Up(PtwL3LineNum)
  val PtwL3TagLen = vpnnLen * 3 - PtwL3IdxLen
  val ptwl3Replacer = Some("setplru")

  // super page, including 1GB and 2MB page
  val SPTagLen = vpnnLen * 2
  val spReplacer = Some("plru")

  val MSHRSize = PtwMissQueueSize

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

  def genPtwL3SetIdx(vpn: UInt) = {
    genPtwL3Idx(vpn)(PtwL3SetIdxLen + PtwL3SectorIdxLen - 1, PtwL3SectorIdxLen)
  }

  def MakeAddr(ppn: UInt, off: UInt) = {
    require(off.getWidth == 9)
    Cat(ppn, off, 0.U(log2Up(XLEN/8).W))(PAddrBits-1, 0)
  }

  def getVpnn(vpn: UInt, idx: Int) = {
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