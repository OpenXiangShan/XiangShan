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
import chisel3.util._
import utility.MaskedRegMap.WritableMask
import xiangshan._
import xiangshan.backend.fu.util.HasCSRConst
import utils._
import utility._
import xiangshan.cache.mmu.{TlbCmd, TlbExceptionBundle}

trait PMPConst extends HasPMParameters {
  val PMPOffBits = 2 // minimal 4bytes
  val CoarserGrain: Boolean = PlatformGrain > PMPOffBits
}

abstract class PMPBundle(implicit val p: Parameters) extends Bundle with PMPConst
abstract class PMPModule(implicit val p: Parameters) extends Module with PMPConst
abstract class PMPXSModule(implicit p: Parameters) extends XSModule with PMPConst

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

object PMPConfigUInt {
  def apply(
    l: Boolean = false,
    c: Boolean = false,
    atomic: Boolean = false,
    a: Int = 0,
    x: Boolean = false,
    w: Boolean = false,
    r: Boolean = false)(implicit p: Parameters): UInt = {
    var config = 0
    if (l) { config += (1 << 7) }
    if (c) { config += (1 << 6) }
    if (atomic) { config += (1 << 5) }
    if (a > 0) { config += (a << 3) }
    if (x) { config += (1 << 2) }
    if (w) { config += (1 << 1) }
    if (r) { config += (1 << 0) }
    config.U(8.W)
  }
}
trait PMPReadWriteMethodBare extends PMPConst {
  def match_mask(cfg: PMPConfig, paddr: UInt) = {
    val match_mask_c_addr = Cat(paddr, cfg.a(0)) | (((1 << PlatformGrain) - 1) >> PMPOffBits).U((paddr.getWidth + 1).W)
    Cat(match_mask_c_addr & ~(match_mask_c_addr + 1.U), ((1 << PMPOffBits) - 1).U(PMPOffBits.W))
  }

  def write_cfg_vec(mask: Vec[UInt], addr: Vec[UInt], index: Int)(cfgs: UInt): UInt = {
    val cfgVec = Wire(Vec(cfgs.getWidth/8, new PMPConfig))
    for (i <- cfgVec.indices) {
      val cfg_w_m_tmp = cfgs((i+1)*8-1, i*8).asUInt.asTypeOf(new PMPConfig)
      cfgVec(i) := cfg_w_m_tmp
      when (!cfg_w_m_tmp.l) {
        cfgVec(i).w := cfg_w_m_tmp.w && cfg_w_m_tmp.r
        if (CoarserGrain) { cfgVec(i).a := Cat(cfg_w_m_tmp.a(1), cfg_w_m_tmp.a.orR) }
        when (cfgVec(i).na4_napot) {
          mask(index + i) := match_mask(cfgVec(i), addr(index + i))
        }
      }
    }
    cfgVec.asUInt
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

  def write_addr(next: PMPConfig, mask: UInt)(paddr: UInt, cfg: PMPConfig, addr: UInt): UInt = {
    val locked = cfg.addr_locked(next)
    mask := Mux(!locked, match_mask(cfg, paddr), mask)
    Mux(!locked, paddr, addr)
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

trait PMPReadWriteMethod extends PMPReadWriteMethodBare  { this: PMPBase =>
  def write_cfg_vec(cfgs: UInt): UInt = {
    val cfgVec = Wire(Vec(cfgs.getWidth/8, new PMPConfig))
    for (i <- cfgVec.indices) {
      val cfg_w_tmp = cfgs((i+1)*8-1, i*8).asUInt.asTypeOf(new PMPConfig)
      cfgVec(i) := cfg_w_tmp
      when (!cfg_w_tmp.l) {
        cfgVec(i).w := cfg_w_tmp.w && cfg_w_tmp.r
        if (CoarserGrain) { cfgVec(i).a := Cat(cfg_w_tmp.a(1), cfg_w_tmp.a.orR) }
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

  /** addr for inside addr, drop OffBits with.
   * compare_addr for inside addr for comparing.
   * paddr for outside addr.
   */
  def write_addr(next: PMPConfig)(paddr: UInt): UInt = {
    Mux(!cfg.addr_locked(next), paddr, addr)
  }
  def write_addr(paddr: UInt): UInt = {
    Mux(!cfg.addr_locked, paddr, addr)
  }
}

/** PMPBase for CSR unit
  * with only read and write logic
  */
class PMPBase(implicit p: Parameters) extends PMPBundle with PMPReadWriteMethod {
  val cfg = new PMPConfig
  val addr = UInt((PMPAddrBits - PMPOffBits).W)

  def gen(cfg: PMPConfig, addr: UInt) = {
    require(addr.getWidth == this.addr.getWidth)
    this.cfg := cfg
    this.addr := addr
  }
}

trait PMPMatchMethod extends PMPConst { this: PMPEntry =>
  /** compare_addr is used to compare with input addr */
  def compare_addr: UInt = ((addr << PMPOffBits) & ~(((1 << PlatformGrain) - 1).U(PMPAddrBits.W))).asUInt

  /** size and maxSize are all log2 Size
   * for dtlb, the maxSize is bPMXLEN which is 8
   * for itlb and ptw, the maxSize is log2(512) ?
   * but we may only need the 64 bytes? how to prevent the bugs?
   * TODO: handle the special case that itlb & ptw & dcache access wider size than PMXLEN
   */
  def is_match(paddr: UInt, lgSize: UInt, lgMaxSize: Int, last_pmp: PMPEntry): Bool = {
    Mux(cfg.na4_napot, napotMatch(paddr, lgSize, lgMaxSize),
      Mux(cfg.tor, torMatch(paddr, lgSize, lgMaxSize, last_pmp), false.B))
  }

  /** generate match mask to help match in napot mode */
  def match_mask(paddr: UInt): UInt = {
    match_mask(cfg, paddr)
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
class PMPEntry(implicit p: Parameters) extends PMPBase with PMPMatchMethod {
  val mask = UInt(PMPAddrBits.W) // help to match in napot

  def write_addr(next: PMPConfig, mask: UInt)(paddr: UInt) = {
    mask := Mux(!cfg.addr_locked(next), match_mask(paddr), mask)
    Mux(!cfg.addr_locked(next), paddr, addr)
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

trait PMPMethod extends PMPConst {
  def pmp_init() : (Vec[UInt], Vec[UInt], Vec[UInt])= {
    val cfg = WireInit(0.U.asTypeOf(Vec(NumPMP/8, UInt(PMXLEN.W))))
    val addr = Wire(Vec(NumPMP, UInt((PMPAddrBits-PMPOffBits).W)))
    val mask = Wire(Vec(NumPMP, UInt(PMPAddrBits.W)))
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
    val pmpCfgPerCSR = PMXLEN / new PMPConfig().getWidth
    def pmpCfgIndex(i: Int) = (PMXLEN / 32) * (i / pmpCfgPerCSR)
    val init_value = init()
    /** to fit MaskedRegMap's write, declare cfgs as Merged CSRs and split them into each pmp */
    val cfgMerged = RegInit(init_value._1) //(Vec(num / pmpCfgPerCSR, UInt(PMXLEN.W))) // RegInit(VecInit(Seq.fill(num / pmpCfgPerCSR)(0.U(PMXLEN.W))))
    val cfgs = WireInit(cfgMerged).asTypeOf(Vec(num, new PMPConfig()))
    val addr = RegInit(init_value._2) // (Vec(num, UInt((PMPAddrBits-PMPOffBits).W)))
    val mask = RegInit(init_value._3) // (Vec(num, UInt(PMPAddrBits.W)))

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
        wfn = { if (i != num-1) entries(i).write_addr(entries(i+1).cfg, mask(i)) else entries(i).write_addr(mask(i)) },
        rmask = WritableMask,
        rfn = new PMPBase().read_addr(entries(i).cfg)
      ))
    }).fold(Map())((a, b) => a ++ b) // ugly code, hit me if u have better codes.

    cfg_mapping ++ addr_mapping
  }
}

class PMP(implicit p: Parameters) extends PMPXSModule with HasXSParameter with PMPMethod with PMAMethod with HasCSRConst {
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

  val rdata = Wire(UInt(PMXLEN.W))
  MaskedRegMap.generate(mapping, w.bits.addr, rdata, w.valid, w.bits.data)

  io.pmp := pmp
  io.pma := pma
}

class PMPReqBundle(lgMaxSize: Int = 3)(implicit p: Parameters) extends PMPBundle {
  val addr = Output(UInt(PMPAddrBits.W))
  val size = Output(UInt(log2Ceil(lgMaxSize+1).W))
  val cmd = Output(TlbCmd())

  def apply(addr: UInt, size: UInt, cmd: UInt) {
    this.addr := addr
    this.size := size
    this.cmd := cmd
  }

  def apply(addr: UInt) { // req minimal permission and req align size
    apply(addr, lgMaxSize.U, TlbCmd.read)
  }

}

class PMPRespBundle(implicit p: Parameters) extends PMPBundle {
  val ld = Output(Bool())
  val st = Output(Bool())
  val instr = Output(Bool())
  val mmio = Output(Bool())
  val atomic = Output(Bool())

  def |(resp: PMPRespBundle): PMPRespBundle = {
    val res = Wire(new PMPRespBundle())
    res.ld := this.ld || resp.ld
    res.st := this.st || resp.st
    res.instr := this.instr || resp.instr
    res.mmio := this.mmio || resp.mmio
    res.atomic := this.atomic || resp.atomic    
    res
  }
}

trait PMPCheckMethod extends PMPConst {
  def pmp_check(cmd: UInt, cfg: PMPConfig) = {
    val resp = Wire(new PMPRespBundle)
    resp.ld := TlbCmd.isRead(cmd) && !TlbCmd.isAmo(cmd) && !cfg.r
    resp.st := (TlbCmd.isWrite(cmd) || TlbCmd.isAmo(cmd)) && !cfg.w
    resp.instr := TlbCmd.isExec(cmd) && !cfg.x
    resp.mmio := false.B
    resp.atomic := false.B
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

    val passThrough = if (pmpEntries.isEmpty) true.B else (mode > 1.U)
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
      ParallelPriorityMux(match_vec.map(RegEnable(_, false.B, valid)), RegEnable(cfg_vec, valid))
    } else {
      ParallelPriorityMux(match_vec, cfg_vec)
    }
  }
}

class PMPCheckerEnv(implicit p: Parameters) extends PMPBundle {
  val mode = UInt(2.W)
  val pmp = Vec(NumPMP, new PMPEntry())
  val pma = Vec(NumPMA, new PMPEntry())

  def apply(mode: UInt, pmp: Vec[PMPEntry], pma: Vec[PMPEntry]): Unit = {
    this.mode := mode
    this.pmp := pmp
    this.pma := pma
  }
}

class PMPCheckIO(lgMaxSize: Int)(implicit p: Parameters) extends PMPBundle {
  val check_env = Input(new PMPCheckerEnv())
  val req = Flipped(Valid(new PMPReqBundle(lgMaxSize))) // usage: assign the valid to fire signal
  val resp = new PMPRespBundle()

  def apply(mode: UInt, pmp: Vec[PMPEntry], pma: Vec[PMPEntry], req: Valid[PMPReqBundle]) = {
    check_env.apply(mode, pmp, pma)
    this.req := req
    resp
  }

  def req_apply(valid: Bool, addr: UInt): Unit = {
    this.req.valid := valid
    this.req.bits.apply(addr)
  }

  def apply(mode: UInt, pmp: Vec[PMPEntry], pma: Vec[PMPEntry], valid: Bool, addr: UInt) = {
    check_env.apply(mode, pmp, pma)
    req_apply(valid, addr)
    resp
  }
}

class PMPCheckv2IO(lgMaxSize: Int)(implicit p: Parameters) extends PMPBundle {
  val check_env = Input(new PMPCheckerEnv())
  val req = Flipped(Valid(new PMPReqBundle(lgMaxSize))) // usage: assign the valid to fire signal
  val resp = Output(new PMPConfig())

  def apply(mode: UInt, pmp: Vec[PMPEntry], pma: Vec[PMPEntry], req: Valid[PMPReqBundle]) = {
    check_env.apply(mode, pmp, pma)
    this.req := req
    resp
  }

  def req_apply(valid: Bool, addr: UInt): Unit = {
    this.req.valid := valid
    this.req.bits.apply(addr)
  }

  def apply(mode: UInt, pmp: Vec[PMPEntry], pma: Vec[PMPEntry], valid: Bool, addr: UInt) = {
    check_env.apply(mode, pmp, pma)
    req_apply(valid, addr)
    resp
  }
}

class PMPChecker
(
  lgMaxSize: Int = 3,
  sameCycle: Boolean = false,
  leaveHitMux: Boolean = false,
  pmpUsed: Boolean = true
)(implicit p: Parameters) extends PMPModule
  with PMPCheckMethod
  with PMACheckMethod
{
  require(!(leaveHitMux && sameCycle))
  val io = IO(new PMPCheckIO(lgMaxSize))

  val req = io.req.bits

  val res_pmp = pmp_match_res(leaveHitMux, io.req.valid)(req.addr, req.size, io.check_env.pmp, io.check_env.mode, lgMaxSize)
  val res_pma = pma_match_res(leaveHitMux, io.req.valid)(req.addr, req.size, io.check_env.pma, io.check_env.mode, lgMaxSize)

  val resp_pmp = pmp_check(req.cmd, res_pmp.cfg)
  val resp_pma = pma_check(req.cmd, res_pma.cfg)
  val resp = if (pmpUsed) (resp_pmp | resp_pma) else resp_pma

  if (sameCycle || leaveHitMux) {
    io.resp := resp
  } else {
    io.resp := RegEnable(resp, io.req.valid)
  }
}

/* get config with check */
class PMPCheckerv2
(
  lgMaxSize: Int = 3,
  sameCycle: Boolean = false,
  leaveHitMux: Boolean = false
)(implicit p: Parameters) extends PMPModule
  with PMPCheckMethod
  with PMACheckMethod
{
  require(!(leaveHitMux && sameCycle))
  val io = IO(new PMPCheckv2IO(lgMaxSize))

  val req = io.req.bits

  val res_pmp = pmp_match_res(leaveHitMux, io.req.valid)(req.addr, req.size, io.check_env.pmp, io.check_env.mode, lgMaxSize)
  val res_pma = pma_match_res(leaveHitMux, io.req.valid)(req.addr, req.size, io.check_env.pma, io.check_env.mode, lgMaxSize)

  val resp = and(res_pmp, res_pma)

  if (sameCycle || leaveHitMux) {
    io.resp := resp
  } else {
    io.resp := RegEnable(resp, io.req.valid)
  }

  def and(pmp: PMPEntry, pma: PMPEntry): PMPConfig = {
    val tmp_res = Wire(new PMPConfig)
    tmp_res.l := DontCare
    tmp_res.a := DontCare
    tmp_res.r := pmp.cfg.r && pma.cfg.r
    tmp_res.w := pmp.cfg.w && pma.cfg.w
    tmp_res.x := pmp.cfg.x && pma.cfg.x
    tmp_res.c := pma.cfg.c
    tmp_res.atomic := pma.cfg.atomic
    tmp_res
  }
}
