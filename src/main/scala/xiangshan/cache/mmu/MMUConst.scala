/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
  saveLevel: Boolean = false,
  lgMaxSize: Int = 3
)

case class L2TLBParameters
(
  name: String = "l2tlb",
  // l3
  l3Size: Int = 16,
  l3Associative: String = "fa",
  l3Replacer: Option[String] = Some("plru"),
  // l2
  l2Size: Int = 16,
  l2Associative: String = "fa",
  l2Replacer: Option[String] = Some("plru"),
  // l1
  l1nSets: Int = 8,
  l1nWays: Int = 4,
  l1Replacer: Option[String] = Some("setplru"),
  // l0
  l0nSets: Int = 32,
  l0nWays: Int = 8,
  l0Replacer: Option[String] = Some("setplru"),
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
  val Level = if (EnableSv48) 3 else 2

  val offLen  = 12
  val ppnLen  = PAddrBits - offLen
  val vpnnLen = 9
  val extendVpnnBits = if (HasHExtension) 2 else 0
  val vpnLen  = VAddrBits - offLen // when opening H extention, vpnlen broaden two bits
  /*
    Sv39 page table entry
    +--+------+--------+----------------------+-----+--------+
    |63|62  61|60    54|53                  10|9   8|7      0|
    +--+------+--------+----------------------+-----+--------+
    |N | PBMT |Reserved|        PPNs          | RSW |  FALG  |
    +--+------+--------+----------------------+-----+--------+
  */
  val pteFlagLen = 8
  val pteRswLen = 2
  val ptePPNLen = 44
  val pteResLen = 7
  val ptePbmtLen = 2
  val pteNLen = 1
  val ppnHignLen = ptePPNLen - ppnLen
  val gvpnLen = GPAddrBits - offLen

  val tlbcontiguous = 8
  val sectortlbwidth = log2Up(tlbcontiguous)
  val sectorppnLen = ppnLen - sectortlbwidth
  val sectorgvpnLen = gvpnLen - sectortlbwidth
  val sectorvpnLen = vpnLen - sectortlbwidth
  val sectorptePPNLen = ptePPNLen - sectortlbwidth

  val loadfiltersize = 16 // 4*3(LduCnt:2 + HyuCnt:1) + 4(prefetch:1)
  val storefiltersize = if (StorePipelineWidth >= 3) 16 else 8
  val prefetchfiltersize = 8

  val sramSinglePort = true

  val timeOutThreshold = 10000

  def noS2xlate = "b00".U
  def allStage = "b11".U
  def onlyStage1 = "b01".U
  def onlyStage2 = "b10".U

  def Sv39 = "h8".U
  def Sv48 = "h9".U

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

  import scala.language.implicitConversions

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
  // ptwl3: fully-associated
  val PtwL3TagLen = if (EnableSv48) vpnnLen + extendVpnnBits else 0
  // ptwl2: fully-associated
  val PtwL2TagLen = if (EnableSv48) vpnnLen * 2 + extendVpnnBits else vpnnLen + extendVpnnBits

  /* +-------+----------+-------------+
   * |  Tag  |  SetIdx  |  SectorIdx  |
   * +-------+----------+-------------+
   */
  // ptwl1: 8-way group-associated
  val PtwL1SetNum = l2tlbParams.l1nSets
  val PtwL1SectorSize = blockBits / XLEN
  val PtwL1IdxLen = log2Up(PtwL1SetNum * PtwL1SectorSize)
  val PtwL1SectorIdxLen = log2Up(PtwL1SectorSize)
  val PtwL1SetIdxLen = log2Up(PtwL1SetNum)
  val PtwL1TagLen = if (EnableSv48) vpnnLen * 3 - PtwL1IdxLen + extendVpnnBits else vpnnLen * 2 - PtwL1IdxLen + extendVpnnBits

  // ptwl0: 16-way group-associated
  val PtwL0SetNum = l2tlbParams.l0nSets
  val PtwL0SectorSize =  blockBits / XLEN
  val PtwL0IdxLen = log2Up(PtwL0SetNum * PtwL0SectorSize)
  val PtwL0SectorIdxLen = log2Up(PtwL0SectorSize)
  val PtwL0SetIdxLen = log2Up(PtwL0SetNum)
  val PtwL0TagLen = if (EnableSv48) vpnnLen * 4 - PtwL0IdxLen + extendVpnnBits else vpnnLen * 3 - PtwL0IdxLen + extendVpnnBits

  // super page, including 1GB and 2MB page
  val SPTagLen = if (EnableSv48) vpnnLen * 3 + extendVpnnBits else vpnnLen * 2 + extendVpnnBits

  // miss queue
  val MissQueueSize = l2tlbParams.ifilterSize + l2tlbParams.dfilterSize
  val MemReqWidth = l2tlbParams.llptwsize + 1 + 1
  val HptwReqId = l2tlbParams.llptwsize + 1
  val FsmReqID = l2tlbParams.llptwsize
  val bMemID = log2Up(MemReqWidth)

  def genPtwL1Idx(vpn: UInt) = {
    (vpn(vpnLen - 1, vpnnLen))(PtwL1IdxLen - 1, 0)
  }

  def genPtwL1SectorIdx(vpn: UInt) = {
    genPtwL1Idx(vpn)(PtwL1SectorIdxLen - 1, 0)
  }

  def genPtwL1SetIdx(vpn: UInt) = {
    genPtwL1Idx(vpn)(PtwL1SetIdxLen + PtwL1SectorIdxLen - 1, PtwL1SectorIdxLen)
  }

  def genPtwL0Idx(vpn: UInt) = {
    vpn(PtwL0IdxLen - 1, 0)
  }

  def genPtwL0SectorIdx(vpn: UInt) = {
    genPtwL0Idx(vpn)(PtwL0SectorIdxLen - 1, 0)
  }

  def dropL0SectorBits(vpn: UInt) = {
    vpn(vpn.getWidth-1, PtwL0SectorIdxLen)
  }

  def genPtwL0SetIdx(vpn: UInt) = {
    genPtwL0Idx(vpn)(PtwL0SetIdxLen + PtwL0SectorIdxLen - 1, PtwL0SectorIdxLen)
  }

  def MakeAddr(ppn: UInt, off: UInt) = {
    require(off.getWidth == 9)
    Cat(ppn, off, 0.U(log2Up(XLEN/8).W))
  }

  def MakeGPAddr(ppn: UInt, off: UInt) = {
    require(off.getWidth == 9 || off.getWidth == 11)
    (Cat(ppn, 0.U(offLen.W)) + Cat(off, 0.U(log2Up(XLEN / 8).W)))(GPAddrBits - 1, 0)
  }

  def getVpnn(vpn: UInt, idx: Int): UInt = {
    vpn(vpnnLen*(idx+1)-1, vpnnLen*idx)
  }

  def getVpnn(vpn: UInt, idx: UInt): UInt = {
    MuxLookup(idx, 0.U)(Seq(
      0.U -> vpn(vpnnLen - 1, 0),
      1.U -> vpn(vpnnLen * 2 - 1, vpnnLen),
      2.U -> vpn(vpnnLen * 3 - 1, vpnnLen * 2),
      3.U -> vpn(vpnnLen * 4 - 1, vpnnLen * 3))
    )
  }

  def getGVpnn(vpn: UInt, idx: UInt, mode: UInt): UInt = {
    MuxLookup(idx, 0.U)(Seq(
      0.U -> vpn(vpnnLen - 1, 0),
      1.U -> vpn(vpnnLen * 2 - 1, vpnnLen),
      2.U -> Mux(mode === Sv48, vpn(vpnnLen * 3 - 1, vpnnLen * 2), vpn(vpnnLen * 3 + 1, vpnnLen * 2)),
      3.U -> vpn(vpnnLen * 4 + 1, vpnnLen * 3))
    )
  }

  def getVpnClip(vpn: UInt, level: Int) = {
    // level 2  /* vpnn2 */
    // level 1  /* vpnn2 * vpnn1 */
    // level 0  /* vpnn2 * vpnn1 * vpnn0*/
    vpn(vpnLen - 1, level * vpnnLen)
  }

  def get_next_line(vpn: UInt) = {
    Cat(dropL0SectorBits(vpn) + 1.U, 0.U(PtwL0SectorIdxLen.W))
  }

  def same_l1entry(vpn1: UInt, vpn2: UInt) = {
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
    dropL0SectorBits(vpn1) === dropL0SectorBits(vpn2)
  }


  def printVec[T <: Data](x: Seq[T]): Printable = {
    (0 until x.length).map(i => p"(${i.U})${x(i)} ").reduce(_+_)
  }
}
