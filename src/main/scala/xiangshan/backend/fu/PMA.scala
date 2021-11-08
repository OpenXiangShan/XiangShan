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
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, SimpleDevice}
import freechips.rocketchip.diplomaticobjectmodel.model.OMRegFieldGroup
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegFieldGroup, RegReadFn, RegWriteFn}
import freechips.rocketchip.tilelink.{TLAdapterNode, TLRegisterNode}
import system.AXI4Spliter
import utils.ParallelPriorityMux
import xiangshan.{DistributedCSRIO, HasXSParameter, XSModule}
import xiangshan.backend.fu.util.HasCSRConst
import xiangshan.cache.mmu.TlbCmd
/*
class TLPMA()(implicit p: Parameters)
  extends LazyModule with HasXSParameter
{
  val pmaNode = TLRegisterNode(
    address = Seq(AddressSet(/*TODO*/, 0xffff)),
    device = new SimpleDevice("tl-pma", Nil),
    concurrency = 1,
    beatBytes = 8
  )
  lazy val module = new PMAImp(this)
}
*/
class PMAImp(wrapper: TLRegisterNode)(implicit p: Parameters) extends XSModule with PMAMethod with PMACheckMethod {
  val io = IO(new Bundle {
    val requestor = Vec(2/*Param*/, new Bundle {
      val req = Flipped(Valid(new PMPReqBundle())) // usage: assign the valid to fire signal
      val resp = new PMPRespBundle()

      def req_apply(valid: Bool, addr: UInt): Unit = {
        this.req.valid := valid
        this.req.bits.apply(addr)
      }
    })
  })

  val pmaNode = wrapper

  val num = NumPMA
  val CoarserGrain: Boolean = PlatformGrain > PMPOffBits
  val pmaCfgPerCSR = XLEN / new PMPConfig().getWidth
  def pmpCfgIndex(i: Int) = (XLEN / 32) * (i / pmaCfgPerCSR)

  val pmacfgBase = 0x0000
  val pmaaddrBase = pmacfgBase + (num / 4) * (XLEN / 8)
  // PMA just doesn't care about "PMP aligned if XLEN is 8/4"

  val pma = Wire(Vec(num, new PMPEntry))

  /* pma init value */
  val init_value = pma_init()

  val pmaCfgMerged = RegInit(init_value._1)
  val addr = RegInit(init_value._2)
  val mask = RegInit(init_value._3)
  val cfg = WireInit(pmaCfgMerged).asTypeOf(Vec(num, new PMPConfig()))
//  pmaMask are implicit regs that just used for timing optimization
  for (i <- pma.indices) {
    pma(i).gen(cfg(i), addr(i), mask(i))
  }

  /* pma write */
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

  val blankCfg = XLEN == 32
  val cfg_index_wrapper = (0 until num by 4).zip((0 until num by 4).map(a => blankCfg || (a % 2 == 0)))
  val cfg_map = (cfg_index_wrapper).map{ case(i, notempty) => {
    RegField.apply(n = XLEN, r = RegReadFn((ivalid, oready) =>
      if (notempty) { (true.B, ivalid, pmaCfgMerged(i)) }
      else { (true.B, ivalid, 0.U) }
    ), w = RegWriteFn((valid, data) => {
      if (notempty) { when (valid) { pmaCfgMerged(i) := write_cfg_vec(mask, addr, i)(data) } }
      true.B
    }), desc = RegFieldDesc(s"TLPMA_config_${i}", s"pma config register #${i}"))
  }}

  val addr_map = (0 until num).map{ i => {
    RegField(n = XLEN, r = read_addr(cfg(i))(addr(i)), w = RegWriteFn((valid, data) => {
      when (valid && !cfg(i).l) { addr := data }
      true.B
    }), desc = RegFieldDesc(s"TLPMA_addr_${i}", s"pma addr register #${i}"))
  }}

  pmaNode.regmap(
    0x0000 -> RegFieldGroup(
      "TLPMA Config Register", Some("TL PMA configuation register"),
      cfg_map
    ),
    0x0100 -> RegFieldGroup(
      "TLPMA Address Register", Some("TL PMA Address register"),
      addr_map
    )
  )

  /* pma check */
  for (i <- 0 until 2/* Param */) {
    val sameCycle = true
    val r = io.requestor(i).req.bits
    val res = pma_match_res(r.addr, r.size, pma, 3.U, 3)
    val check = pma_check(r.cmd, res.cfg)

    if (sameCycle) {
      io.requestor(i).resp := check
    } else {
      io.requestor(i).resp := RegEnable(check, io.requestor(i).req.valid)
    }
  }
}

trait PMAMethod extends HasXSParameter with PMPConst {
  /**
  def SimpleMemMapList = List(
      //     Base address      Top address       Width  Description    Mode (RWXIDSAC)
      MemMap("h00_0000_0000", "h00_0FFF_FFFF",   "h0", "Reserved",    "RW"),
      MemMap("h00_1000_0000", "h00_1FFF_FFFF",   "h0", "QSPI_Flash",  "RWX"),
      MemMap("h00_2000_0000", "h00_2FFF_FFFF",   "h0", "Reserved",    "RW"),
      MemMap("h00_3000_0000", "h00_3000_FFFF",   "h0", "DMA",         "RW"),
      MemMap("h00_3001_0000", "h00_3004_FFFF",   "h0", "GPU",         "RWC"),
      MemMap("h00_3005_0000", "h00_3006_FFFF",   "h0", "USB/SDMMC",   "RW"),
      MemMap("h00_3007_0000", "h00_30FF_FFFF",   "h0", "Reserved",    "RW"),
      MemMap("h00_3100_0000", "h00_3111_FFFF",   "h0", "MMIO",        "RW"),
      MemMap("h00_3112_0000", "h00_37FF_FFFF",   "h0", "Reserved",    "RW"), // NOTE: partly used by TLPMA
      MemMap("h00_3800_0000", "h00_3800_FFFF",   "h0", "CLINT",       "RW"),
      MemMap("h00_3801_0000", "h00_3801_FFFF",   "h0", "BEU",         "RW"),
      MemMap("h00_3802_0000", "h00_3802_0FFF",   "h0", "DebugModule",    "RWX"),
      MemMap("h00_3802_1000", "h00_3BFF_FFFF",   "h0", "Reserved",    ""),
      MemMap("h00_3C00_0000", "h00_3FFF_FFFF",   "h0", "PLIC",        "RW"),
      MemMap("h00_4000_0000", "h00_7FFF_FFFF",   "h0", "PCIe",        "RW"),
      MemMap("h00_8000_0000", "h0F_FFFF_FFFF",   "h0", "DDR",         "RWXIDSA"),
    )
   */

  def pma_init() : (Vec[UInt], Vec[UInt], Vec[UInt]) = {
    // the init value is zero
    // from 0 to num(default 16) - 1, lower priority
    // according to simple map, 9 entries is needed, pick 6-14, leave 0-5 & 15 unusedcfgMerged.map(_ := 0.U)

    val num = NumPMA
    require(num >= 16)
    val cfg = WireInit(0.U.asTypeOf(Vec(num, new PMPConfig())))

    val addr = Wire(Vec(num, UInt((PAddrBits-PMPOffBits).W)))
    val mask = Wire(Vec(NumPMP, UInt(PAddrBits.W)))
    addr := DontCare
    mask := DontCare

    addr(15) := 0x3FFFFFFFEL.U
    cfg(15).a := 1.U; cfg(15).r := true.B; cfg(15).w := true.B; cfg(15).x := true.B; cfg(15).c := true.B; cfg(14).atomic := true.B

    // use tor instead of napot, for napot may be confusing and hard to understand
    addr(14) := shift_addr(0xFFFFFFFFFL)
    cfg(14).a := 1.U; cfg(14).r := true.B; cfg(14).w := true.B; cfg(14).x := true.B; cfg(14).c := true.B; cfg(14).atomic := true.B

    addr(13) := shift_addr(0x80000000L)
    cfg(13).a := 1.U; cfg(13).r := true.B; cfg(13).w := true.B

    addr(12) := shift_addr(0x3C000000)
    cfg(12).a := 1.U

    addr(11) := shift_addr(0x38021000)
    cfg(11).a := 1.U; cfg(11).r := true.B; cfg(11).w := true.B; cfg(11).x := true.B

    addr(10) := shift_addr(0x38020000)
    cfg(10).a := 1.U; cfg(10).r := true.B; cfg(10).w := true.B

    addr(9) := shift_addr( 0x30050000)
    cfg(9).a := 1.U; cfg(9).r := true.B; cfg(9).w := true.B; cfg(8).c := true.B

    addr(8) := shift_addr( 0x30010000)
    cfg(8).a := 1.U; cfg(8).r := true.B; cfg(8).w := true.B

    addr(7) := shift_addr( 0x20000000)
    cfg(7).a := 1.U; cfg(7).r := true.B; cfg(7).w := true.B; cfg(7).x := true.B

    addr(6) := shift_addr( 0x10000000)
    cfg(6).a := 1.U; cfg(6).r := true.B; cfg(6).w := true.B

    addr(5) := shift_addr(0)

    val cfgInitMerge = cfg.asTypeOf(Vec(num/8, UInt(XLEN.W)))
    (cfgInitMerge, addr, mask)
  }

  def shift_addr(addr: BigInt) = {
    (addr >> 2).U
  }
}

trait PMACheckMethod extends HasXSParameter with HasCSRConst {
  def pma_check(cmd: UInt, cfg: PMPConfig) = {
    val resp = Wire(new PMPRespBundle)
    resp.ld := TlbCmd.isRead(cmd) && !TlbCmd.isAtom(cmd) && !cfg.r
    resp.st := (TlbCmd.isWrite(cmd) || TlbCmd.isAtom(cmd) && cfg.atomic) && !cfg.w
    resp.instr := TlbCmd.isExec(cmd) && !cfg.x
    resp.mmio := !cfg.c
    resp
  }

  def pma_match_res(addr: UInt, size: UInt, pmaEntries: Vec[PMPEntry], mode: UInt, lgMaxSize: Int) = {
    val num = pmaEntries.size
    require(num == NumPMA)
    // pma should always be checked, could not be ignored
    // like amo and cached, it is the attribute not protection
    // so it must have initialization.
    require(!pmaEntries.isEmpty)

    val pmaDefault = WireInit(0.U.asTypeOf(new PMPEntry()))
    val match_vec = Wire(Vec(num+1, Bool()))
    val cfg_vec = Wire(Vec(num+1, new PMPEntry()))

    pmaEntries.zip(pmaDefault +: pmaEntries.take(num-1)).zipWithIndex.foreach{ case ((pma, last_pma), i) =>
      val is_match = pma.is_match(addr, size, lgMaxSize, last_pma)
      val aligned = pma.aligned(addr, size, lgMaxSize, last_pma)

      val cur = WireInit(pma)
      cur.cfg.r := aligned && pma.cfg.r
      cur.cfg.w := aligned && pma.cfg.w
      cur.cfg.x := aligned && pma.cfg.x
      cur.cfg.atomic := aligned && pma.cfg.atomic
      cur.cfg.c := aligned && pma.cfg.c

      match_vec(i) := is_match
      cfg_vec(i) := cur
    }

    match_vec(num) := true.B
    cfg_vec(num) := pmaDefault

    ParallelPriorityMux(match_vec, cfg_vec)
  }
}