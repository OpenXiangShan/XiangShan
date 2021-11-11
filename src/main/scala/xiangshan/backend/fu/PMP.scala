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

// See LICENSE.SiFive for license details.

package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import utils.MaskedRegMap.WritableMask
import xiangshan._
import xiangshan.backend.fu.util.HasCSRConst
import utils._
import xiangshan.cache.mmu.{TlbCmd, TlbExceptionBundle}

trait PMPConst {
  val PMPOffBits = 2 // minimal 4bytes
}

abstract class PMPBundle(implicit p: Parameters) extends XSBundle with PMPConst {
  val CoarserGrain: Boolean = PlatformGrain > PMPOffBits
}

abstract class PMPModule(implicit p: Parameters) extends XSModule with PMPConst with HasCSRConst

@chiselName
class PMPConfig(implicit p: Parameters) extends PMPBundle {
  val l = Bool()
  val c = Bool() // res(1), unuse in pmp
  val atomic = Bool() // res(0), unuse in pmp
  val a = UInt(2.W)
  val x = Bool()
  val w = Bool()
  val r = Bool()

  def res: UInt = Cat(c, atomic) // in pmp, unused
  def off = a === 0.U
  def tor = a === 1.U
  def na4 = { if (CoarserGrain) false.B else a === 2.U }
  def napot = { if (CoarserGrain) a(1).asBool else a === 3.U }
  def off_tor = !a(1)
  def na4_napot = a(1)

  def locked = l
  def addr_locked: Bool = locked
  def addr_locked(next: PMPConfig): Bool = locked || (next.locked && next.tor)
}

trait PMPReadWriteMethod extends PMPConst { this: PMPBase =>
  def write_cfg_vec(cfgs: UInt): UInt = {
    val cfgVec = Wire(Vec(cfgs.getWidth/8, new PMPConfig))
    for (i <- cfgVec.indices) {
      val cfg_w_tmp = cfgs((i+1)*8-1, i*8).asUInt.asTypeOf(new PMPConfig)
      cfgVec(i) := cfg_w_tmp
      cfgVec(i).w := cfg_w_tmp.w && cfg_w_tmp.r
      if (CoarserGrain) { cfgVec(i).a := Cat(cfg_w_tmp.a(1), cfg_w_tmp.a.orR) }
    }
    cfgVec.asUInt
  }

  def write_cfg_vec(mask: Vec[UInt], addr: Vec[UInt], index: Int)(cfgs: UInt): UInt = {
    val cfgVec = Wire(Vec(cfgs.getWidth/8, new PMPConfig))
    for (i <- cfgVec.indices) {
      val cfg_w_m_tmp = cfgs((i+1)*8-1, i*8).asUInt.asTypeOf(new PMPConfig)
      cfgVec(i) := cfg_w_m_tmp
      cfgVec(i).w := cfg_w_m_tmp.w && cfg_w_m_tmp.r
      if (CoarserGrain) { cfgVec(i).a := Cat(cfg_w_m_tmp.a(1), cfg_w_m_tmp.a.orR) }
      when (cfgVec(i).na4_napot) {
        mask(index + i) := new PMPEntry().match_mask(cfgVec(i), addr(index + i))
      }
    }
    cfgVec.asUInt
  }

  /** In general, the PMP grain is 2**{G+2} bytes. when G >= 1, na4 is not selectable.
   * When G >= 2 and cfg.a(1) is set(then the mode is napot), the bits addr(G-2, 0) read as zeros.
   * When G >= 1 and cfg.a(1) is clear(the mode is off or tor), the addr(G-1, 0) read as zeros.
   * The low OffBits is dropped
   */
  def read_addr(): UInt = {
    read_addr(cfg)(addr)
  }

  def read_addr(cfg: PMPConfig)(addr: UInt): UInt = {
    val G = PlatformGrain - PMPOffBits
    require(G >= 0)
    if (G == 0) {
      addr
    } else if (G >= 2) {
      Mux(cfg.na4_napot, set_low_bits(addr, G-1), clear_low_bits(addr, G))
    } else { // G is 1
      Mux(cfg.off_tor, clear_low_bits(addr, G), addr)
    }
  }
  /** addr for inside addr, drop OffBits with.
   * compare_addr for inside addr for comparing.
   * paddr for outside addr.
   */
  def write_addr(next: PMPBase)(paddr: UInt) = {
    Mux(!cfg.addr_locked(next.cfg), paddr, addr)
  }
  def write_addr(paddr: UInt) = {
    Mux(!cfg.addr_locked, paddr, addr)
  }

  def set_low_bits(data: UInt, num: Int): UInt = {
    require(num >= 0)
    data | ((1 << num)-1).U
  }

  /** mask the data's low num bits (lsb) */
  def clear_low_bits(data: UInt, num: Int): UInt = {
    require(num >= 0)
    // use Cat instead of & with mask to avoid "Signal Width" problem
    if (num == 0) { data }
    else { Cat(data(data.getWidth-1, num), 0.U(num.W)) }
  }
}

/** PMPBase for CSR unit
  * with only read and write logic
  */
@chiselName
class PMPBase(implicit p: Parameters) extends PMPBundle with PMPReadWriteMethod {
  val cfg = new PMPConfig
  val addr = UInt((PAddrBits - PMPOffBits).W)

  def gen(cfg: PMPConfig, addr: UInt) = {
    require(addr.getWidth == this.addr.getWidth)
    this.cfg := cfg
    this.addr := addr
  }
}

trait PMPMatchMethod extends PMPConst { this: PMPEntry =>
  /** compare_addr is used to compare with input addr */
  def compare_addr: UInt = ((addr << PMPOffBits) & ~(((1 << PlatformGrain) - 1).U(PAddrBits.W))).asUInt

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

  /** generate match mask to help match in napot mode */
  def match_mask(paddr: UInt) = {
    val match_mask_addr: UInt = Cat(paddr, cfg.a(0)).asUInt() | (((1 << PlatformGrain) - 1) >> PMPOffBits).U((paddr.getWidth + 1).W)
    Cat(match_mask_addr & ~(match_mask_addr + 1.U), ((1 << PMPOffBits) - 1).U(PMPOffBits.W))
  }

  def match_mask(cfg: PMPConfig, paddr: UInt) = {
    val match_mask_c_addr = Cat(paddr, cfg.a(0)) | (((1 << PlatformGrain) - 1) >> PMPOffBits).U((paddr.getWidth + 1).W)
    Cat(match_mask_c_addr & ~(match_mask_c_addr + 1.U), ((1 << PMPOffBits) - 1).U(PMPOffBits.W))
  }

  def boundMatch(paddr: UInt, lgSize: UInt, lgMaxSize: Int): Bool = {
    if (lgMaxSize <= PlatformGrain) {
      (paddr < compare_addr)
    } else {
      val highLess = (paddr >> lgMaxSize) < (compare_addr >> lgMaxSize)
      val highEqual = (paddr >> lgMaxSize) === (compare_addr >> lgMaxSize)
      val lowLess = (paddr(lgMaxSize-1, 0) | OneHot.UIntToOH1(lgSize, lgMaxSize))  < compare_addr(lgMaxSize-1, 0)
      highLess || (highEqual && lowLess)
    }
  }

  def lowerBoundMatch(paddr: UInt, lgSize: UInt, lgMaxSize: Int): Bool = {
    !boundMatch(paddr, lgSize, lgMaxSize)
  }

  def higherBoundMatch(paddr: UInt, lgMaxSize: Int) = {
    boundMatch(paddr, 0.U, lgMaxSize)
  }

  def torMatch(paddr: UInt, lgSize: UInt, lgMaxSize: Int, last_pmp: PMPEntry): Bool = {
    last_pmp.lowerBoundMatch(paddr, lgSize, lgMaxSize) && higherBoundMatch(paddr, lgMaxSize)
  }

  def unmaskEqual(a: UInt, b: UInt, m: UInt) = {
    (a & ~m) === (b & ~m)
  }

  def napotMatch(paddr: UInt, lgSize: UInt, lgMaxSize: Int) = {
    if (lgMaxSize <= PlatformGrain) {
      unmaskEqual(paddr, compare_addr, mask)
    } else {
      val lowMask = mask | OneHot.UIntToOH1(lgSize, lgMaxSize)
      val highMatch = unmaskEqual(paddr >> lgMaxSize, compare_addr >> lgMaxSize, mask >> lgMaxSize)
      val lowMatch = unmaskEqual(paddr(lgMaxSize-1, 0), compare_addr(lgMaxSize-1, 0), lowMask(lgMaxSize-1, 0))
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
      val napotAligned = (lowBitsMask & ~mask(lgMaxSize-1, 0)) === 0.U
      Mux(cfg.na4_napot, napotAligned, torAligned)
    }
  }
}

/** PMPEntry for outside pmp copies
  * with one more elements mask to help napot match
  * TODO: make mask an element, not an method, for timing opt
  */
@chiselName
class PMPEntry(implicit p: Parameters) extends PMPBase with PMPMatchMethod {
  val mask = UInt(PAddrBits.W) // help to match in napot

  def write_addr(next: PMPBase, mask: UInt)(paddr: UInt) = {
    mask := Mux(!cfg.addr_locked(next.cfg), match_mask(paddr), mask)
    Mux(!cfg.addr_locked(next.cfg), paddr, addr)
  }

  def write_addr(mask: UInt)(paddr: UInt) = {
    mask := Mux(!cfg.addr_locked, match_mask(paddr), mask)
    Mux(!cfg.addr_locked, paddr, addr)
  }

  def gen(cfg: PMPConfig, addr: UInt, mask: UInt) = {
    require(addr.getWidth == this.addr.getWidth)
    this.cfg := cfg
    this.addr := addr
    this.mask := mask
  }
}

trait PMPMethod extends HasXSParameter with PMPConst { this: XSModule =>
  def pmp_init() : (Vec[UInt], Vec[UInt], Vec[UInt])= {
    val cfg = WireInit(0.U.asTypeOf(Vec(NumPMP/8, UInt(XLEN.W))))
    val addr = Wire(Vec(NumPMP, UInt((PAddrBits-PMPOffBits).W)))
    val mask = Wire(Vec(NumPMP, UInt(PAddrBits.W)))
    addr := DontCare
    mask := DontCare
    (cfg, addr, mask)
  }

  def pmp_gen_mapping
  (
    init: () => (Vec[UInt], Vec[UInt], Vec[UInt]),
    num: Int = 16,
    cfgBase: Int,
    addrBase: Int,
    entries: Vec[PMPEntry]
  ) = {
    val pmpCfgPerCSR = XLEN / new PMPConfig().getWidth
    def pmpCfgIndex(i: Int) = (XLEN / 32) * (i / pmpCfgPerCSR)
    val init_value = init()
    /** to fit MaskedRegMap's write, declare cfgs as Merged CSRs and split them into each pmp */
    val cfgMerged = RegInit(init_value._1) //(Vec(num / pmpCfgPerCSR, UInt(XLEN.W))) // RegInit(VecInit(Seq.fill(num / pmpCfgPerCSR)(0.U(XLEN.W))))
    val cfgs = WireInit(cfgMerged).asTypeOf(Vec(num, new PMPConfig()))
    val addr = RegInit(init_value._2) // (Vec(num, UInt((PAddrBits-PMPOffBits).W)))
    val mask = RegInit(init_value._3) // (Vec(num, UInt(PAddrBits.W)))

    for (i <- entries.indices) {
      entries(i).gen(cfgs(i), addr(i), mask(i))
    }



    val cfg_mapping = (0 until num by pmpCfgPerCSR).map(i => {Map(
      MaskedRegMap(
        addr = cfgBase + pmpCfgIndex(i),
        reg = cfgMerged(i/pmpCfgPerCSR),
        wmask = WritableMask,
        wfn = new PMPBase().write_cfg_vec(mask, addr, i)
      ))
    }).fold(Map())((a, b) => a ++ b) // ugly code, hit me if u have better codes

    val addr_mapping = (0 until num).map(i => {Map(
      MaskedRegMap(
        addr = addrBase + i,
        reg = addr(i),
        wmask = WritableMask,
        wfn = { if (i != num-1) entries(i).write_addr(entries(i+1), mask(i)) else entries(i).write_addr(mask(i)) },
        rmask = WritableMask,
        rfn = new PMPBase().read_addr(entries(i).cfg)
      ))
    }).fold(Map())((a, b) => a ++ b) // ugly code, hit me if u have better codes.



    cfg_mapping ++ addr_mapping
  }
}

@chiselName
class PMP(implicit p: Parameters) extends PMPModule with PMPMethod with PMAMethod {
  val io = IO(new Bundle {
    val distribute_csr = Flipped(new DistributedCSRIO())
    val pmp = Output(Vec(NumPMP, new PMPEntry()))
    val pma = Output(Vec(NumPMA, new PMPEntry()))
  })

  val w = io.distribute_csr.w

  val pmp = Wire(Vec(NumPMP, new PMPEntry()))
  val pma = Wire(Vec(NumPMA, new PMPEntry()))

  val pmpMapping = pmp_gen_mapping(pmp_init, NumPMP, PmpcfgBase, PmpaddrBase, pmp)
  val pmaMapping = pmp_gen_mapping(pma_init, NumPMA, PmacfgBase, PmaaddrBase, pma)
  val mapping = pmpMapping ++ pmaMapping

  val rdata = Wire(UInt(XLEN.W))
  MaskedRegMap.generate(mapping, w.bits.addr, rdata, w.valid, w.bits.data)

  io.pmp := pmp
  io.pma := pma
}

class PMPReqBundle(lgMaxSize: Int = 3)(implicit p: Parameters) extends PMPBundle {
  val addr = Output(UInt(PAddrBits.W))
  val size = Output(UInt(log2Ceil(lgMaxSize+1).W))
  val cmd = Output(TlbCmd())

  override def cloneType = (new PMPReqBundle(lgMaxSize)).asInstanceOf[this.type]
}

class PMPRespBundle(implicit p: Parameters) extends TlbExceptionBundle {
  val mmio = Output(Bool())

  def |(resp: PMPRespBundle): PMPRespBundle = {
    val res = Wire(new PMPRespBundle())
    res.ld := this.ld || resp.ld
    res.st := this.st || resp.st
    res.instr := this.instr || resp.instr
    res.mmio := this.mmio || resp.mmio
    res
  }
}

trait PMPCheckMethod extends HasXSParameter with HasCSRConst { this: PMPChecker =>
  def pmp_check(cmd: UInt, cfg: PMPConfig)(implicit p: Parameters) = {
    val resp = Wire(new PMPRespBundle)
    resp.ld := TlbCmd.isRead(cmd) && !TlbCmd.isAtom(cmd) && !cfg.r
    resp.st := (TlbCmd.isWrite(cmd) || TlbCmd.isAtom(cmd)) && !cfg.w
    resp.instr := TlbCmd.isExec(cmd) && !cfg.x
    resp.mmio := false.B
    resp
  }

  def pmp_match_res(leaveHitMux: Boolean = false, valid: Bool = true.B)(
    addr: UInt,
    size: UInt,
    pmpEntries: Vec[PMPEntry],
    mode: UInt,
    lgMaxSize: Int
  ) = {
    val num = pmpEntries.size
    require(num == NumPMP)

    val passThrough = if (pmpEntries.isEmpty) true.B else (mode > ModeS)
    val pmpDefault = WireInit(0.U.asTypeOf(new PMPEntry()))
    pmpDefault.cfg.r := passThrough
    pmpDefault.cfg.w := passThrough
    pmpDefault.cfg.x := passThrough

    val match_vec = Wire(Vec(num+1, Bool()))
    val cfg_vec = Wire(Vec(num+1, new PMPEntry()))

    pmpEntries.zip(pmpDefault +: pmpEntries.take(num-1)).zipWithIndex.foreach{ case ((pmp, last_pmp), i) =>
      val is_match = pmp.is_match(addr, size, lgMaxSize, last_pmp)
      val ignore = passThrough && !pmp.cfg.l
      val aligned = pmp.aligned(addr, size, lgMaxSize, last_pmp)

      val cur = WireInit(pmp)
      cur.cfg.r := aligned && (pmp.cfg.r || ignore)
      cur.cfg.w := aligned && (pmp.cfg.w || ignore)
      cur.cfg.x := aligned && (pmp.cfg.x || ignore)

//      Mux(is_match, cur, prev)
      match_vec(i) := is_match
      cfg_vec(i) := cur
    }

    // default value
    match_vec(num) := true.B
    cfg_vec(num) := pmpDefault

    if (leaveHitMux) {
      ParallelPriorityMux(match_vec.map(RegEnable(_, init = false.B, valid)), RegEnable(cfg_vec, valid))
    } else {
      ParallelPriorityMux(match_vec, cfg_vec)
    }
  }
}

@chiselName
class PMPChecker
(
  lgMaxSize: Int = 3,
  sameCycle: Boolean = false,
  leaveHitMux: Boolean = false
)(implicit p: Parameters)
  extends PMPModule
  with PMPCheckMethod
  with PMACheckMethod
{
  val io = IO(new Bundle{
    val env = Input(new Bundle {
      val mode = Input(UInt(2.W))
      val pmp = Input(Vec(NumPMP, new PMPEntry()))
      val pma = Input(Vec(NumPMA, new PMPEntry()))
    })
    val req = Flipped(Valid(new PMPReqBundle(lgMaxSize))) // usage: assign the valid to fire signal
    val resp = new PMPRespBundle()
  })
  require(!(leaveHitMux && sameCycle))

  val req = io.req.bits

  val res_pmp = pmp_match_res(leaveHitMux, io.req.valid)(req.addr, req.size, io.env.pmp, io.env.mode, lgMaxSize)
  val res_pma = pma_match_res(leaveHitMux, io.req.valid)(req.addr, req.size, io.env.pma, io.env.mode, lgMaxSize)

  val resp_pmp = pmp_check(req.cmd, res_pmp.cfg)
  val resp_pma = pma_check(req.cmd, res_pma.cfg)
  val resp = resp_pmp | resp_pma

  if (sameCycle || leaveHitMux) {
    io.resp := resp
  } else {
    io.resp := RegEnable(resp, io.req.valid)
  }
}