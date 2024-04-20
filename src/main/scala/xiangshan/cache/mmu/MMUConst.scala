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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import utility._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._


case class TLBParameters
(
  name: String = "none",
  fetchi: Boolean = false, // TODO: remove it
  fenceDelay: Int = 2,
  useDmode: Boolean = true,
  NSets: Int = 1,
  NWays: Int = 2,
  Replacer: Option[String] = Some("plru"),
  Associative: String = "fa", // must be fa
  outReplace: Boolean = false,
  partialStaticPMP: Boolean = false, // partial static pmp result stored in entries
  outsideRecvFlush: Boolean = false, // if outside moudle waiting for tlb recv flush pipe
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
  l2nSets: Int = 8,
  l2nWays: Int = 4,
  l2Replacer: Option[String] = Some("setplru"),
  // l3
  l3nSets: Int = 32,
  l3nWays: Int = 8,
  l3Replacer: Option[String] = Some("setplru"),
  // sp
  spSize: Int = 16,
  spReplacer: Option[String] = Some("plru"),
  // filter
  ifilterSize: Int = 8,
  dfilterSize: Int = 32,
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
  ecc: Option[String] = Some("secded"),
  // enable ecc
  enablePTWECC: Boolean = false
)

trait HasTlbConst extends HasXSParameter {
  val Level = 3

  val offLen  = 12
  val ppnLen  = PAddrBits - offLen
  val vpnnLen = 9
  val extendVpnnBits = if (HasHExtension) 2 else 0
  val vpnLen  = VAddrBits - offLen // when opening H extention, vpnlen broaden two bits
  val flagLen = 8
  val pteResLen = XLEN - 44 - 2 - flagLen
  val ppnHignLen = 44 - ppnLen

  val tlbcontiguous = 8
  val sectortlbwidth = log2Up(tlbcontiguous)
  val sectorppnLen = ppnLen - sectortlbwidth
  val sectorvpnLen = vpnLen - sectortlbwidth

  val loadfiltersize = 16 // 4*3(LduCnt:2 + HyuCnt:1) + 4(prefetch:1)
  val storefiltersize = if (StorePipelineWidth >= 3) 16 else 8
  val prefetchfiltersize = 8

  val sramSinglePort = true

  val timeOutThreshold = 10000

  def noS2xlate = "b00".U
  def allStage = "b11".U
  def onlyStage1 = "b01".U
  def onlyStage2 = "b10".U

  def get_pn(addr: UInt) = {
    require(addr.getWidth > offLen)
    addr(addr.getWidth-1, offLen)
  }
  def get_off(addr: UInt) = {
    require(addr.getWidth > offLen)
    addr(offLen-1, 0)
  }

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
    val emptyIdx = ParallelPriorityMux((0 until width).map( i => (!v(i), i.U(log2Up(width).W))))
    val full = Cat(v).andR
    Mux(full, lruIdx, emptyIdx)
  }

  def replaceWrapper(v: Seq[Bool], lruIdx: UInt): UInt = {
    replaceWrapper(VecInit(v).asUInt, lruIdx)
  }

  implicit def hptwresp_to_tlbperm(hptwResp: HptwResp): TlbPermBundle = {
    val tp = Wire(new TlbPermBundle)
    val ptePerm = hptwResp.entry.perm.get.asTypeOf(new PtePermBundle().cloneType)
    tp.pf := hptwResp.gpf
    tp.af := hptwResp.gaf
    tp.d := ptePerm.d
    tp.a := ptePerm.a
    tp.g := ptePerm.g
    tp.u := ptePerm.u
    tp.x := ptePerm.x
    tp.w := ptePerm.w
    tp.r := ptePerm.r
    tp
  }

  implicit def ptwresp_to_tlbperm(ptwResp: PtwSectorResp): TlbPermBundle = {
    val tp = Wire(new TlbPermBundle)
    val ptePerm = ptwResp.entry.perm.get.asTypeOf(new PtePermBundle().cloneType)
    tp.pf := ptwResp.pf
    tp.af := ptwResp.af
    tp.d := ptePerm.d
    tp.a := ptePerm.a
    tp.g := ptePerm.g
    tp.u := ptePerm.u
    tp.x := ptePerm.x
    tp.w := ptePerm.w
    tp.r := ptePerm.r
    tp
  }
}

trait HasPtwConst extends HasTlbConst with MemoryOpConstants{
  val PtwWidth = 2
  val sourceWidth = { if (l2tlbParams.enablePrefetch) PtwWidth + 1 else PtwWidth}
  val prefetchID = PtwWidth

  val blockBits = l2tlbParams.blockBytes * 8

  val bPtwWidth = log2Up(PtwWidth)
  val bSourceWidth = log2Up(sourceWidth)
  // ptwl1: fully-associated
  val PtwL1TagLen = vpnnLen + extendVpnnBits

  /* +-------+----------+-------------+
   * |  Tag  |  SetIdx  |  SectorIdx  |
   * +-------+----------+-------------+
   */
  // ptwl2: 8-way group-associated
  val l2tlbParams.l2nWays = l2tlbParams.l2nWays
  val PtwL2SetNum = l2tlbParams.l2nSets
  val PtwL2SectorSize = blockBits / XLEN
  val PtwL2IdxLen = log2Up(PtwL2SetNum * PtwL2SectorSize)
  val PtwL2SectorIdxLen = log2Up(PtwL2SectorSize)
  val PtwL2SetIdxLen = log2Up(PtwL2SetNum)
  val PtwL2TagLen = vpnnLen * 2 - PtwL2IdxLen + extendVpnnBits

  // ptwl3: 16-way group-associated
  val l2tlbParams.l3nWays = l2tlbParams.l3nWays
  val PtwL3SetNum = l2tlbParams.l3nSets
  val PtwL3SectorSize =  blockBits / XLEN
  val PtwL3IdxLen = log2Up(PtwL3SetNum * PtwL3SectorSize)
  val PtwL3SectorIdxLen = log2Up(PtwL3SectorSize)
  val PtwL3SetIdxLen = log2Up(PtwL3SetNum)
  val PtwL3TagLen = vpnnLen * 3 - PtwL3IdxLen + extendVpnnBits

  // super page, including 1GB and 2MB page
  val SPTagLen = vpnnLen * 2 + extendVpnnBits

  // miss queue
  val MissQueueSize = l2tlbParams.ifilterSize + l2tlbParams.dfilterSize
  val MemReqWidth = l2tlbParams.llptwsize + 1 + 1
  val HptwReqId = l2tlbParams.llptwsize + 1
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

  def MakeGPAddr(ppn: UInt, off: UInt) = {
    require(off.getWidth == 9 || off.getWidth == 11)
    (Cat(ppn, 0.U(offLen.W)) + Cat(off, 0.U(log2Up(XLEN / 8).W)))(GPAddrBits - 1, 0)
  }

  def getVpnn(vpn: UInt, idx: Int): UInt = {
    vpn(vpnnLen*(idx+1)-1, vpnnLen*idx)
  }

  def getVpnn(vpn: UInt, idx: UInt): UInt = {
    Mux(idx === 0.U, vpn(vpnnLen - 1, 0), Mux(idx === 1.U, vpn(vpnnLen * 2 - 1, vpnnLen), vpn(vpnnLen * 3 - 1, vpnnLen * 2)))
  }

  def getGVpnn(vpn: UInt, idx: UInt): UInt = {
    Mux(idx === 0.U, vpn(vpnnLen - 1, 0), Mux(idx === 1.U, vpn(vpnnLen * 2 - 1, vpnnLen), vpn(vpnnLen * 3 + 1, vpnnLen * 2)))
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

  def sel_data(data: UInt, index: UInt): UInt = {
    val inner_data = data.asTypeOf(Vec(data.getWidth / XLEN, UInt(XLEN.W)))
    inner_data(index)
  }

  // vpn1 and vpn2 is at same cacheline
  def dup(vpn1: UInt, vpn2: UInt): Bool = {
    dropL3SectorBits(vpn1) === dropL3SectorBits(vpn2)
  }


  def printVec[T <: Data](x: Seq[T]): Printable = {
    (0 until x.length).map(i => p"(${i.U})${x(i)} ").reduce(_+_)
  }
}
