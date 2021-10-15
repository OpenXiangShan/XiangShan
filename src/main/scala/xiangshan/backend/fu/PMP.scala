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
    val cfgVec = Wire(Vec(cfgs.getWidth/8, new PMPConfig))
    for (i <- cfgVec.indices) {
      val tmp = cfgs((i+1)*8-1, i*8).asUInt.asTypeOf(new PMPConfig)
      cfgVec(i) := tmp
      cfgVec(i).w := tmp.w && tmp.r
      if (CoarserGrain) { cfgVec(i).a := Cat(tmp.a(1), tmp.a.orR) }
    }
    cfgVec.asUInt
  }

  def write_cfg_vec(mask: Vec[UInt], addr: Vec[UInt], index: Int)(cfgs: UInt): UInt = {
    val cfgVec = Wire(Vec(cfgs.getWidth/8, new PMPConfig))
    for (i <- cfgVec.indices) {
      val tmp = cfgs((i+1)*8-1, i*8).asUInt.asTypeOf(new PMPConfig)
      cfgVec(i) := tmp
      cfgVec(i).w := tmp.w && tmp.r
      if (CoarserGrain) { cfgVec(i).a := Cat(tmp.a(1), tmp.a.orR) }
      when (cfgVec(i).na4_napot) {
        mask(index + i) := new PMPEntry().match_mask(cfgVec(i), addr(index + i))
      }
    }
    cfgVec.asUInt
  }

  def reset() = {
    l := false.B
    a := 0.U
  }
}

/** PMPBase for CSR unit
  * with only read and write logic
  */
@chiselName
class PMPBase(implicit p: Parameters) extends PMPBundle {
  val cfg = new PMPConfig
  val addr = UInt((PAddrBits - PMPOffBits).W)

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

  def gen(cfg: PMPConfig, addr: UInt) = {
    require(addr.getWidth == this.addr.getWidth)
    this.cfg := cfg
    this.addr := addr
  }
}

/** PMPEntry for outside pmp copies
  * with one more elements mask to help napot match
  * TODO: make mask an element, not an method, for timing opt
  */
@chiselName
class PMPEntry(implicit p: Parameters) extends PMPBase {
  val mask = UInt(PAddrBits.W) // help to match in napot

  /** compare_addr is used to compare with input addr */
  def compare_addr = ((addr << PMPOffBits) & ~(((1 << PlatformGrain) - 1).U(PAddrBits.W))).asUInt

  def write_addr(next: PMPBase, mask: UInt)(paddr: UInt) = {
    mask := Mux(!cfg.addr_locked(next.cfg), match_mask(paddr), mask)
    Mux(!cfg.addr_locked(next.cfg), paddr, addr)
  }

  def write_addr(mask: UInt)(paddr: UInt) = {
    mask := Mux(!cfg.addr_locked, match_mask(paddr), mask)
    Mux(!cfg.addr_locked, paddr, addr)
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

  /** generate match mask to help match in napot mode */
  def match_mask(paddr: UInt) = {
    val tmp_addr = Cat(paddr, cfg.a(0)) | (((1 << PlatformGrain) - 1) >> PMPOffBits).U((paddr.getWidth + 1).W)
    Cat(tmp_addr & ~(tmp_addr + 1.U), ((1 << PMPOffBits) - 1).U(PMPOffBits.W))
  }

  def match_mask(cfg: PMPConfig, paddr: UInt) = {
    val tmp_addr = Cat(paddr, cfg.a(0)) | (((1 << PlatformGrain) - 1) >> PMPOffBits).U((paddr.getWidth + 1).W)
    Cat(tmp_addr & ~(tmp_addr + 1.U), ((1 << PMPOffBits) - 1).U(PMPOffBits.W))
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

  def gen(cfg: PMPConfig, addr: UInt, mask: UInt) = {
    require(addr.getWidth == this.addr.getWidth)
    this.cfg := cfg
    this.addr := addr
    this.mask := mask
  }

  def reset() = {
    cfg.l := 0.U
    cfg.a := 0.U
  }
}

@chiselName
class PMP(implicit p: Parameters) extends PMPModule {
  val io = IO(new Bundle {
    val distribute_csr = Flipped(new DistributedCSRIO())
    val pmp = Output(Vec(NumPMP, new PMPEntry()))
  })

  val w = io.distribute_csr.w

  val pmp = Wire(Vec(NumPMP, new PMPEntry()))

  val pmpCfgPerCSR = XLEN / new PMPConfig().getWidth
  def pmpCfgIndex(i: Int) = (XLEN / 32) * (i / pmpCfgPerCSR)

  /** to fit MaskedRegMap's write, declare cfgs as Merged CSRs and split them into each pmp */
  val cfgMerged = RegInit(VecInit(Seq.fill(NumPMP / pmpCfgPerCSR)(0.U(XLEN.W))))
  val cfgs = WireInit(cfgMerged).asTypeOf(Vec(NumPMP, new PMPConfig()))
  val addr = Reg(Vec(NumPMP, UInt((PAddrBits-PMPOffBits).W)))
  val mask = Reg(Vec(NumPMP, UInt(PAddrBits.W)))

  for (i <- pmp.indices) {
    pmp(i).gen(cfgs(i), addr(i), mask(i))
  }

  val cfg_mapping = (0 until NumPMP by pmpCfgPerCSR).map(i => {Map(
    MaskedRegMap(
      addr = PmpcfgBase + pmpCfgIndex(i),
      reg = cfgMerged(i/pmpCfgPerCSR),
      wmask = WritableMask,
      wfn = new PMPConfig().write_cfg_vec(mask, addr, i)
    ))
  }).fold(Map())((a, b) => a ++ b) // ugly code, hit me if u have better codes

  val addr_mapping = (0 until NumPMP).map(i => {Map(
    MaskedRegMap(
      addr = PmpaddrBase + i,
      reg = addr(i),
      wmask = WritableMask,
      wfn = { if (i != NumPMP-1) pmp(i).write_addr(pmp(i+1), mask(i)) else pmp(i).write_addr(mask(i)) },
      rmask = WritableMask,
      rfn = new PMPBase().read_addr(pmp(i).cfg)
    ))
  }).fold(Map())((a, b) => a ++ b) // ugly code, hit me if u have better codes.
  val pmpMapping =  cfg_mapping ++ addr_mapping

  val rdata = Wire(UInt(XLEN.W))
  MaskedRegMap.generate(pmpMapping, w.bits.addr, rdata, w.valid, w.bits.data)

  io.pmp := pmp
}

class PMPReqBundle(lgMaxSize: Int = 3)(implicit p: Parameters) extends PMPBundle {
  val addr = Output(UInt(PAddrBits.W))
  val size = Output(UInt(log2Ceil(lgMaxSize+1).W))
  val cmd = Output(TlbCmd())

  override def cloneType = (new PMPReqBundle(lgMaxSize)).asInstanceOf[this.type]
}

class PMPRespBundle(implicit p: Parameters) extends TlbExceptionBundle

@chiselName
class PMPChecker(lgMaxSize: Int = 3, sameCycle: Boolean = false)(implicit p: Parameters) extends PMPModule {
  val io = IO(new Bundle{
    val env = Input(new Bundle {
      val mode = Input(UInt(2.W))
      val pmp = Input(Vec(NumPMP, new PMPEntry()))
    })
    val req = Flipped(Valid(new PMPReqBundle(lgMaxSize))) // usage: assign the valid to fire signal
    val resp = Output(new PMPRespBundle())
  })

  val req = io.req.bits

  val passThrough = if (io.env.pmp.isEmpty) true.B else (io.env.mode > ModeS)
  val pmpMinuxOne = WireInit(0.U.asTypeOf(new PMPEntry()))
  pmpMinuxOne.cfg.r := passThrough
  pmpMinuxOne.cfg.w := passThrough
  pmpMinuxOne.cfg.x := passThrough

  val match_wave = Wire(Vec(NumPMP, Bool()))
  val ignore_wave = Wire(Vec(NumPMP, Bool()))
  val aligned_wave = Wire(Vec(NumPMP, Bool()))
  val prev_wave = Wire(Vec(NumPMP, new PMPEntry()))
  val cur_wave = Wire(Vec(NumPMP, new PMPEntry()))

  dontTouch(match_wave)
  dontTouch(ignore_wave)
  dontTouch(aligned_wave)
  dontTouch(prev_wave)
  dontTouch(cur_wave)

  val res = io.env.pmp.zip(pmpMinuxOne +: io.env.pmp.take(NumPMP-1)).zipWithIndex
    .reverse.foldLeft(pmpMinuxOne) { case (prev, ((pmp, last_pmp), i)) =>
    val is_match = pmp.is_match(req.addr, req.size, lgMaxSize, last_pmp)
    val ignore = passThrough && !pmp.cfg.l
    val aligned = pmp.aligned(req.addr, req.size, lgMaxSize, last_pmp)

    val cur = WireInit(pmp)
    cur.cfg.r := aligned && (pmp.cfg.r || ignore)
    cur.cfg.w := aligned && (pmp.cfg.w || ignore)
    cur.cfg.x := aligned && (pmp.cfg.x || ignore)

    match_wave(i) := is_match
    ignore_wave(i) := ignore
    aligned_wave(i) := aligned
    cur_wave(i) := cur
    prev_wave(i) := prev

    XSDebug(p"pmp${i.U} cfg:${Hexadecimal(pmp.cfg.asUInt)} addr:${Hexadecimal(pmp.addr)} mask:${Hexadecimal(pmp.mask)} is_match:${is_match} aligned:${aligned}")

    Mux(is_match, cur, prev)
  }

  // NOTE: if itlb or dtlb may get blocked, this may also need do it
  val ld = TlbCmd.isRead(req.cmd) && !TlbCmd.isAtom(req.cmd) && !res.cfg.r
  val st = (TlbCmd.isWrite(req.cmd) || TlbCmd.isAtom(req.cmd)) && !res.cfg.w
  val instr = TlbCmd.isExec(req.cmd) && !res.cfg.x
  if (sameCycle) {
    io.resp.ld := ld
    io.resp.st := st
    io.resp.instr := instr
  } else {
    io.resp.ld := RegEnable(ld, io.req.valid)
    io.resp.st := RegEnable(st, io.req.valid)
    io.resp.instr := RegEnable(instr, io.req.valid)
  }
}
