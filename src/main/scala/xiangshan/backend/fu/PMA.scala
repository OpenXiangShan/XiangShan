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

import chisel3._
import chisel3.util._
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegReadFn, RegWriteFn}
import utils.{ParallelPriorityMux, ValidHold, ZeroExt}
import xiangshan.cache.mmu.TlbCmd

import scala.collection.mutable.ListBuffer

/* Memory Mapped PMA */
case class MMPMAConfig
(
  address: BigInt,
  mask: BigInt,
  lgMaxSize: Int,
  sameCycle: Boolean,
  num: Int
)

trait PMAConst extends PMPConst

trait MMPMAMethod extends PMAConst with PMAMethod with PMPReadWriteMethodBare {
  def gen_mmpma_mapping(num: Int) = {
    val pmaCfgPerCSR = PMXLEN / new PMPConfig().getWidth
    def pmaCfgLogicIndex(i: Int) = (PMXLEN / 32) * (i / pmaCfgPerCSR)
    def pmaCfgIndex(i: Int) = (i / pmaCfgPerCSR)

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

    val blankCfg = PMXLEN == 32
    val cfg_index_wrapper = (0 until num by 4).zip((0 until num by 4).map(a => blankCfg || (a % pmaCfgPerCSR == 0)))
    val cfg_map = (cfg_index_wrapper).map{ case(i, notempty) => {
//      println(s"tlbpma i:$i notempty:$notempty")
      RegField.apply(n = PMXLEN, r = RegReadFn{(ivalid, oready) =>
        val r_ready = Wire(Bool())
        val o_valid = Wire(Bool())
        val v_reg = ValidHold(r_ready && ivalid, o_valid && oready, false.B)
        r_ready := !v_reg
        o_valid := v_reg

        if (notempty) { (r_ready, o_valid, pmaCfgMerged(pmaCfgIndex(i))) }
        else { (r_ready, o_valid, 0.U) }
      }, w = RegWriteFn((valid, data) => {
        if (notempty) { when (valid) { pmaCfgMerged(pmaCfgIndex(i)) := write_cfg_vec(mask, addr, i)(data) } }
        true.B
      }), desc = RegFieldDesc(s"MMPMA_config_${i}", s"pma config register #${i}"))
    }}

    val addr_map = (0 until num).map{ i => {
      val next_cfg = if (i == 0) 0.U.asTypeOf(new PMPConfig()) else cfg(i-1)
      RegField(
        n = PMXLEN,
        r = ZeroExt(read_addr(cfg(i))(addr(i)), PMXLEN),
        w = RegWriteFn((valid, data) => {
          when (valid) { addr(i) := write_addr(next_cfg, mask(i))(data(addr(0).getWidth-1, 0), cfg(i), addr(i))}
          true.B
        }),
        desc = RegFieldDesc(s"MMPMA_addr_${i}", s"pma addr register #${i}")
      )
    }}

    (cfg_map, addr_map, pma)
  }

}

trait PMAMethod extends PMAConst {
  // def address_map = List(
  //   MemMap("0x00_8000_0000", "0x00_FFFF_FFFF", "h0", "PCIe Slave Space Low PCIe",   "RWX"),
  //   MemMap("0x01_0000_0000", "0x07_FFFF_FFFF", "h0", "PCIe Slave Space High PCIe",  "RWX"),
  //   MemMap("0x08_0000_0000", "0x1E_FFFF_FFFF", "h0", "Reserved",                    "R"),
  //   MemMap("0x1F_0000_0000", "0x1F_0FFF_FFFF", "h0", "CPUSS Perfipheral",           "RW"),
  //   MemMap("0x1F_1000_0000", "0x1F_1FFF_FFFF", "h0", "Reserved",                    "R"),
  //   MemMap("0x1F_2000_0000", "0x1F_201F_FFFF", "h0", "DDR Config",                  "RW"),
  //   MemMap("0x1F_2020_0000", "0x1F_203F_FFFF", "h0", "PCIe PHY",                    "RW"),
  //   MemMap("0x1F_2040_0000", "0x1F_2047_FFFF", "h0", "APGC Config",                 "RW"),
  //   MemMap("0x1F_2048_0000", "0x1F_2048_FFFF", "h0", "SOC TOP Register",            "RW"),
  //   MemMap("0x1F_2049_0000", "0x1F_2049_FFFF", "h0", "DTS",                         "RW"),
  //   MemMap("0x1F_204A_0000", "0x1F_204A_FFFF", "h0", "GPIO PAR0",                   "RW"),
  //   MemMap("0x1F_204B_0000", "0x1F_204B_FFFF", "h0", "GPIO PAR1",                   "RW"),
  //   MemMap("0x1F_204C_0000", "0x1F_204C_FFFF", "h0", "PLL0",                        "RW"),
  //   MemMap("0x1F_204D_0000", "0x1F_204D_FFFF", "h0", "PLL1",                        "RW"),
  //   MemMap("0x1F_204E_0000", "0x1F_204E_FFFF", "h0", "PLL2",                        "RW"),
  //   MemMap("0x1F_204F_0000", "0x1F_204F_03FF", "h0", "Fuse0",                       "RW"),
  //   MemMap("0x1F_204F_0400", "0x1F_2049_07FF", "h0", "Fuse1",                       "RW"),
  //   MemMap("0x1F_204F_0800", "0x1F_2049_0BFF", "h0", "RTC Register",                "RW"),
  //   MemMap("0x1F_204F_0C00", "0x1F_7FFF_FFFF", "h0", "Reserved",                    "R"), // NOTE: not aligned to 4KB
  //   MemMap("0x1F_8000_0000", "0x1F_BFFF_FFFF", "h0", "Peripheral SS",               "RWX"),
  //   MemMap("0x1F_C000_0000", "0x1F_DFFF_FFFF", "h0", "PCIe Slave Space",            "RW"),
  //   MemMap("0x1F_E000_0000", "0x1F_E1FF_FFFF", "h0", "PCI SS Config Space",         "RW"),
  //   MemMap("0x1F_E200_0000", "0x1F_E21F_FFFF", "h0", "Shared SRAM",                 "RWX"),
  //   MemMap("0x1F_E220_0000", "0x1F_FFF7_FFFF", "h0", "Reserved",                    "R"),
  //   MemMap("0x1F_FFF8_0000", "0x1F_FFFB_FFFF", "h0", "BOOT ROM",                    "RWX"),
  //   MemMap("0x1F_FFFC_0000", "0x1F_FFFF_FFFF", "h0", "Reserved",                    "R"),
  //   MemMap("0x20_0000_0000", "0x23_FFFF_FFFF", "h0", "MEM SS[DDR]",                  "RWX")
  // )
  // TODO: use the address_map to generate pma init list.

  def pma_init() : (Vec[UInt], Vec[UInt], Vec[UInt]) = {
    def genAddr(init_addr: BigInt) = init_addr.U((PMPAddrBits - PMPOffBits).W)
    def genMask(init_addr: BigInt, a: BigInt) = {
      val match_mask_addr = (init_addr << 1) | (a & 0x1) | (((1 << PlatformGrain) - 1) >> PMPOffBits)
      val mask = ((match_mask_addr & ~(match_mask_addr + 1)) << PMPOffBits) | ((1 << PMPOffBits) - 1)
      mask.U(PMPAddrBits.W)
    }

    val num = NumPMA
    require(num >= 16)

    val cfg_list = ListBuffer[UInt]()
    val addr_list = ListBuffer[UInt]()
    val mask_list = ListBuffer[UInt]()
    def addPMA(addr: BigInt,
               l: Boolean = false,
               c: Boolean = false,
               atomic: Boolean = false,
               a: Int = 0,
               x: Boolean = false,
               w: Boolean = false,
               r: Boolean = false) = {
      cfg_list.append(PMPConfigUInt(l, c, atomic, a, x, w, r))
      addr_list.append(genAddr(shift_addr(addr)))
      mask_list.append(genMask(shift_addr(addr), a))
    }

    // addPMA(0x2400000000L, c = true, atomic = true, a = 1, x = true, w = true, r = true)
    // addPMA(0x2000000000L, a = 1, r = true)
    // addPMA(0x1FFFFC0000L, a = 1, r = true, x = true, w = true)
    // addPMA(0x1FFFF80000L, a = 1, r = true)
    // addPMA(0x1FE2200000L, a = 1, x = true, w = true, r = true)
    // addPMA(0x1FE2000000L, a = 1, w = true, r = true)
    // addPMA(0x1FC0000000L, a = 1, x = true, w = true, r = true)
    // addPMA(0x1F80000000L, a = 1, r = true)
    // addPMA(0x1F204F1000L, a = 1, w = true, r = true)
    // // 0x1F_0000_0000 - 0x1F_1FFF_FFFF is marked as reserved in the SoC memory map.
    // // This region is used by the CPU to accommodate internal devices.
    // addPMA(0x1F20000000L, a = 1, w = true, r = true)
    // addPMA(0x1F10021000L, a = 1, w = true, r = true, x = true)
    // addPMA(0x1F10020000L, a = 1, w = true, r = true)
    // addPMA(0x1F00000000L, a = 1, r = true)
    // addPMA(0x800000000L, a = 1, x = true, w = true, r = true)
    // addPMA(0x80000000L, a = 1)
    // addPMA(0)
    // while (cfg_list.length < 16) {
    //   addPMA(0)
    // }
        addPMA(0x480000000L, c = true, atomic = true, a = 1, x = true, w = true, r = true)
    addPMA(0x80000000L, a = 1, w = true, r = true)
    addPMA(0x3c000000L, a = 1)
    addPMA(0x39002000L, a = 1, w = true, r = true)
    addPMA(0x39000000L, a = 1)
    addPMA(0x38021000L, a = 1, w = true, r = true, x = true)
    addPMA(0x38020000L, a = 1)
    addPMA(0x38010000L, a = 1, w = true, r = true)
    addPMA(0x38000000L, a = 1)
    addPMA(0x31310000L, a = 1, w = true, r = true)
    addPMA(0x30000000L, a = 1)
    addPMA(0x20000000L, a = 1, x = true, r = true)
    addPMA(0x10000000L, a = 1)
    addPMA(0)
    while (cfg_list.length < 16) {
      addPMA(0)
    }

    val cfgInitMerge = Seq.tabulate(num / 8)(i => {
      cfg_list.reverse.drop(8 * i).take(8).foldRight(BigInt(0L)) { case (a, result) =>
        (result << a.getWidth) | a.litValue
      }.U(PMXLEN.W)
    })
    val addr = addr_list.reverse
    val mask = mask_list.reverse
    (VecInit(cfgInitMerge), VecInit(addr), VecInit(mask))
  }

  def get_napot(base: BigInt, range: BigInt) = {
    val PlatformGrainBytes = (1 << PlatformGrain)
    if ((base % PlatformGrainBytes) != 0) {
      println("base:%x", base)
    }
    if ((range % PlatformGrainBytes) != 0) {
      println("range: %x", range)
    }
    require((base % PlatformGrainBytes) == 0)
    require((range % PlatformGrainBytes) == 0)

    ((base + (range/2 - 1)) >> PMPOffBits).U
  }

  def match_mask(paddr: UInt, cfg: PMPConfig) = {
    val match_mask_addr: UInt = Cat(paddr, cfg.a(0)).asUInt() | (((1 << PlatformGrain) - 1) >> PMPOffBits).U((paddr.getWidth + 1).W)
    Cat(match_mask_addr & ~(match_mask_addr + 1.U), ((1 << PMPOffBits) - 1).U(PMPOffBits.W))
  }

  def shift_addr(addr: BigInt) = {
    addr >> 2
  }
}

trait PMACheckMethod extends PMPConst {
  def pma_check(cmd: UInt, cfg: PMPConfig) = {
    val resp = Wire(new PMPRespBundle)
    resp.ld := TlbCmd.isRead(cmd) && !TlbCmd.isAtom(cmd) && !cfg.r
    resp.st := (TlbCmd.isWrite(cmd) || TlbCmd.isAtom(cmd) && cfg.atomic) && !cfg.w
    resp.instr := TlbCmd.isExec(cmd) && !cfg.x
    resp.mmio := !cfg.c
    resp
  }

  def pma_match_res(leaveHitMux: Boolean = false, valid: Bool = true.B)(
    addr: UInt,
    size: UInt,
    pmaEntries: Vec[PMPEntry],
    mode: UInt,
    lgMaxSize: Int
  ) = {
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
    if (leaveHitMux) {
      ParallelPriorityMux(match_vec.map(RegEnable(_, init = false.B, valid)), RegEnable(cfg_vec, valid))
    } else {
      ParallelPriorityMux(match_vec, cfg_vec)
    }
  }
}
