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

package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.util.HasCSRConst
import utils._

trait PMPConst {
  val OffBits = 2 // minimal 4bytes
}

abstract class PMPBundle(implicit p: Parameters) extends XSBundle with PMPConst {
  val CoarserGrain: Boolean = PlatformGrain > OffBits
}

abstract class PMPModule(implicit p: Parameters) extends XSModule with PMPConst with HasCSRConst

@chiselName
class PMPConfig(implicit p: Parameters) extends PMPBundle {
  val l = Bool()
  val res = UInt(2.W)
  val a = UInt(2.W)
  val x = Bool()
  val w = Bool()
  val r = Bool()

  def off = a === 0.U
  def tor = a === 1.U
  def na4 = { if (CoarserGrain) false.B else a === 2.U }
  def napot = { if (CoarserGrain) a(1).asBool else a === 3.U }
  def off_tor = !a(1)
  def na4_napot = a(1)

  def locked = l
  def addr_locked: Bool = locked
  def addr_locked(next: PMPConfig): Bool = locked || (next.locked && next.tor)

  def write_cfg_vec(cfgs: UInt): UInt = {
    val cfgVec = WireInit(Vec(cfgs.getWidth/8, new PMPConfig))
    for (i <- cfgVec.indices) {
      val tmp = cfgs((i+1)*8-1, (i+1)*8).asUInt.asTypeOf(new PMPConfig)
      cfgVec(i) := tmp
      cfgVec(i).w := tmp.w && tmp.r
      if (CoarserGrain) { cfgVec(i).a := Cat(tmp.a(1), tmp.a.orR) }
    }
    cfgVec.asUInt
  }
}

@chiselName
class PMPEntry(implicit p: Parameters) extends PMPBundle {
  val cfg = new PMPConfig
  val addr = UInt((PAddrBits - OffBits).W)

  def mask = match_mask(addr) // help to match in napot

  /** compare_addr is used to compare with input addr */
  def compare_addr = ((addr << OffBits) & ~(((1 << PlatformGrain) - 1).U(PAddrBits.W))).asUInt

  /** In general, the PMP grain is 2**{G+2} bytes. when G >= 1, na4 is not selectable.
    * When G >= 2 and cfg.a(1) is set(then the mode is napot), the bits addr(G-2, 0) read as zeros.
    * When G >= 1 and cfg.a(1) is clear(the mode is off or tor), the addr(G-1, 0) read as zeros.
    * The low OffBits is dropped
    */
  def read_addr() = {
    val G = PlatformGrain - OffBits
    require(G >= 0)
    if (G == 0) {
      addr
    } else if (G >= 2) {
      Mux(cfg.napot, mask_low_bits(addr, G-1), mask_low_bits(addr, G))
    } else { // G is 1
      Mux(cfg.off_tor, mask_low_bits(addr, G), addr)
    }
  }

  def read_addr(cfg: PMPConfig)(addr: UInt): UInt = {
    val G = PlatformGrain - OffBits
    require(G >= 0)
    if (G == 0) {
      addr
    } else if (G >= 2) {
      Mux(cfg.napot, mask_low_bits(addr, G-1), mask_low_bits(addr, G))
    } else { // G is 1
      Mux(cfg.off_tor, mask_low_bits(addr, G), addr)
    }
  }
  /** addr for inside addr, drop OffBits with.
    * compare_addr for inside addr for comparing.
    * paddr for outside addr.
    */
  def write_addr(next: PMPEntry)(paddr: UInt) = {
    Mux(!cfg.addr_locked(next.cfg), paddr(PAddrBits-1, OffBits), addr)
  }
  def write_addr(paddr: UInt) = {
    Mux(!cfg.addr_locked, paddr(PAddrBits-1, OffBits), addr)
  }

  /** size and maxSize are all log2 Size
    * for dtlb, the maxSize is bXLEN which is 8
    * for itlb and ptw, the maxSize is log2(512) ?
    * but we may only need the 64 bytes? how to prevent the bugs?
    * TODO: handle the special case that itlb & ptw & dcache access wider size than XLEN
    */
  def is_match(paddr: UInt, lgSize: UInt, lgMaxSize: Int, last_pmp: PMPEntry): Bool = {
    Mux(cfg.na4_napot, napotMatch(paddr, lgSize, lgMaxSize),
      Mux(cfg.tor, torMatch(paddr, lgSize, lgMaxSize, last_pmp), false.B))
  }

  /** the below methods are used to generate the above methods */

  /** mask the data's low num bits (lsb) */
  def mask_low_bits(data: UInt, num: Int): UInt = {
    require(num >= 0)
    // use Cat instead of & with mask to avoid "Signal Width" problem
    if (num == 0) { data }
    else { Cat(data(data.getWidth-1, num), 0.U((num-1).U)) }
  }

  /** generate match mask to help match in napot mode */
  def match_mask(paddr: UInt) = {
    val tmp_addr = Cat(paddr, cfg.a(0)) | (((1 << PlatformGrain) - 1) >> OffBits).U((paddr.getWidth + 1).W)
    ~Cat(tmp_addr & ~(tmp_addr + 1.U), ((1 << OffBits) - 1).U(OffBits.W))
  }

  def boundMatch(paddr: UInt, lgSize: UInt, lgMaxSize: Int) = {
    if (lgMaxSize <= PlatformGrain) {
      paddr < compare_addr
    } else {
      val highLess = (paddr >> lgMaxSize) < (compare_addr >> lgMaxSize)
      val highEqual = (paddr >> lgMaxSize) === (compare_addr >> lgMaxSize)
      val lowLess = (paddr(lgMaxSize-1, 0) | OneHot.UIntToOH1(lgSize, lgMaxSize))  < compare_addr(lgMaxSize-1, 0)
      highLess || (highEqual && lowLess)
    }
  }

  def lowerBoundMatch(paddr: UInt, lgSize: UInt, lgMaxSize: Int) = {
    !boundMatch(paddr, lgSize, lgMaxSize)
  }

  def higherBoundMatch(paddr: UInt, lgMaxSize: Int) = {
    boundMatch(paddr, 0.U, lgMaxSize)
  }

  def torMatch(paddr: UInt, lgSize: UInt, lgMaxSize: Int, last_pmp: PMPEntry) = {
    last_pmp.lowerBoundMatch(paddr, lgSize, lgMaxSize) && higherBoundMatch(paddr, lgMaxSize)
  }

  def maskEqual(a: UInt, b: UInt, m: UInt) = {
    (a & m) === (b & m)
  }

  def napotMatch(paddr: UInt, lgSize: UInt, lgMaxSize: Int) = {
    if (lgMaxSize <= PlatformGrain) {
      maskEqual(paddr, compare_addr, mask)
    } else {
      val lowMask = ~mask | OneHot.UIntToOH1(lgSize, lgMaxSize)
      val highMatch = maskEqual(paddr >> lgMaxSize, compare_addr >> lgMaxSize, mask >> lgMaxSize)
      val lowMatch = maskEqual(paddr(lgMaxSize-1, 0), compare_addr(lgMaxSize-1, 0), ~lowMask(lgMaxSize-1, 0))
      highMatch && lowMatch
    }
  }

  def aligned(paddr: UInt, lgSize: UInt, lgMaxSize: Int, last: PMPEntry) = {
    if (lgMaxSize <= PlatformGrain) {
      true.B
    } else {
      val lowBitsMask = OneHot.UIntToOH1(lgSize, lgMaxSize)
      val lowerBound = ((paddr >> lgMaxSize) === (last.compare_addr >> lgMaxSize)) &&
        ((~paddr(lgMaxSize-1, 0) & last.compare_addr(lgMaxSize-1, 0)) =/= 0.U)
      val upperBound = ((paddr >> lgMaxSize) === (compare_addr >> lgMaxSize)) &&
        ((compare_addr(lgMaxSize-1, 0) & (paddr(lgMaxSize-1, 0) | lowBitsMask)) =/= 0.U)
      val torAligned = !(lowerBound || upperBound)
      val napotAligned = (lowBitsMask & mask(lgMaxSize-1, 0)) === 0.U
      Mux(cfg.na4_napot, napotAligned, torAligned)
    }
  }

  def reset() = {
    cfg.l := 0.U
    cfg.a := 0.U
  }
}

@chiselName
class PMPChecker(lgMaxSize: Int)(implicit p: Parameters) extends PMPModule {
  val io = IO(new Bundle{
    val env = Input(new Bundle {
      val priv = Input(UInt(2.W))
      val pmp = Input(Vec(NumPMP, new PMPEntry()))
    })
    val req = Input(new Bundle {
      val addr = Input(UInt(PAddrBits.W))
      val size = Input(UInt(log2Ceil(lgMaxSize+1).W))
    })
    val resp = Output(new Bundle {
      val r = Bool()
      val w = Bool()
      val x = Bool()
    })
  })

  val passThrough = if (io.env.pmp.isEmpty) true.B else (io.env.priv > ModeS)
  val pmpMinuxOne = WireInit(0.U.asTypeOf(new PMPEntry()))
  pmpMinuxOne.cfg.r := passThrough
  pmpMinuxOne.cfg.w := passThrough
  pmpMinuxOne.cfg.x := passThrough

  val res = io.env.pmp.zip(pmpMinuxOne +: io.env.pmp.take(NumPMP-1))
    .reverse.foldLeft(pmpMinuxOne) { case (prev, (pmp, last_pmp)) =>
    val is_match = pmp.is_match(io.req.addr, io.req.size, lgMaxSize, last_pmp)
    val ignore = passThrough && !pmp.cfg.l
    val aligned = pmp.aligned(io.req.addr, io.req.size, lgMaxSize, last_pmp)

    val cur = WireInit(pmp)
    cur.cfg.r := aligned && (pmp.cfg.r || ignore)
    cur.cfg.w := aligned && (pmp.cfg.w || ignore)
    cur.cfg.x := aligned && (pmp.cfg.x || ignore)

    Mux(is_match, cur, prev)
  }

  io.resp.r := res.cfg.r
  io.resp.w := res.cfg.w
  io.resp.x := res.cfg.x
}