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
import xiangshan.{HasXSParameter, XSModule}
import xiangshan.backend.fu.util.HasCSRConst
import xiangshan.cache.mmu.TlbCmd

trait PMAMethod extends HasXSParameter with PMPConst { this: XSModule =>
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
      MemMap("h00_3112_0000", "h00_37FF_FFFF",   "h0", "Reserved",    "RW"),
      MemMap("h00_3800_0000", "h00_3800_FFFF",   "h0", "CLINT",       "RW"),
      MemMap("h00_3801_0000", "h00_3801_FFFF",   "h0", "BEU",         "RW"),
      MemMap("h00_3802_0000", "h00_3802_0FFF",   "h0", "DebugModule",    "RWX"),
      MemMap("h00_3802_1000", "h00_3BFF_FFFF",   "h0", "Reserved",    ""),
      MemMap("h00_3C00_0000", "h00_3FFF_FFFF",   "h0", "PLIC",        "RW"),
      MemMap("h00_4000_0000", "h00_7FFF_FFFF",   "h0", "PCIe",        "RW"),
      MemMap("h00_8000_0000", "h1F_FFFF_FFFF",   "h0", "DDR",         "RWXIDSA"),
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

    // use tor instead of napot, for napot may be confusing and hard to understand
    addr(14) := shift_addr( 0x2000000000L)
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

trait PMACheckMethod extends HasXSParameter with HasCSRConst { this: PMPChecker =>
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
    val default = if (pmaEntries.isEmpty) true.B else (mode > ModeS)
    val pmpMinuxOne = WireInit(0.U.asTypeOf(new PMPEntry()))

    val res = pmaEntries.zip(pmpMinuxOne +: pmaEntries.take(num-1)).zipWithIndex
      .reverse.foldLeft(pmpMinuxOne) { case (prev, ((pma, last_pma), i)) =>
      val is_match = pma.is_match(addr, size, lgMaxSize, last_pma)
      val aligned = pma.aligned(addr, size, lgMaxSize, last_pma)

      val cur = WireInit(pma)
      cur.cfg.r := aligned && pma.cfg.r
      cur.cfg.w := aligned && pma.cfg.w
      cur.cfg.x := aligned && pma.cfg.x
      cur.cfg.atomic := aligned && pma.cfg.atomic
      cur.cfg.c := aligned && pma.cfg.c

      Mux(is_match, cur, prev)
    }
    res
  }
}