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
import chipsalliance.rocketchip.config.Parameters
import system.SoCParamsKey

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
  /**
   * from  CPU
   * BASE            TOP             Size   Description     Attribute
   * 0x00_0000_0000  0x00_0FFF_FFFF         Reserved
   * 0x00_1000_0000  0x00_1FFF_FFFF  256MB  QSPI Flash      RX
   * 0x00_2000_0000  0x00_2FFF_FFFF         Reserved
   * 0x00_3000_0000  0x00_3000_FFFF  64KB   GPU(V550)       RW
   * 0x00_3001_0000  0x00_3001_FFFF  64KB   G71             RW
   * 0x00_3002_0000  0x00_3003_FFFF         Reserved
   * 0x00_3004_0000  0x00_3004_FFFF  64KB   DMA             RW
   * 0x00_3005_0000  0x00_3005_FFFF  64KB   SDMMC           RW
   * 0x00_3006_0000  0x00_3015_FFFF  1MB    USB             RW
   * 0x00_3016_0000  0x00_3025_FFFF  1MB    DATA_CPU_BRIDGE RW
   * 0x00_3026_0000  0x00_30FF_FFFF         Reserved
   * 0x00_3100_0000  0x00_3100_FFFF  64KB   QSPI            RW
   * 0x00_3101_0000  0x00_3101_FFFF  64KB   GMAC            RW
   * 0x00_3102_0000  0x00_3102_FFFF  64KB   HDMI            RW
   * 0x00_3103_0000  0x00_3103_FFFF  64KB   HDMI_PHY        RW
   * 0x00_3104_0000  0x00_3105_FFFF  128KB  DP              RW
   * 0x00_3106_0000  0x00_3106_FFFF  64KB   DDR0            RW
   * 0x00_3107_0000  0x00_3107_FFFF  64KB   DDR0_PHY        RW
   * 0x00_3108_0000  0x00_3108_FFFF  64KB   DDR1            RW
   * 0x00_3109_0000  0x00_3109_FFFF  64KB   DDR1_PHY        RW
   * 0x00_310A_0000  0x00_310A_FFFF  64KB   IIS             RW
   * 0x00_310B_0000  0x00_310B_FFFF  64KB   UART0           RW
   * 0x00_310C_0000  0x00_310C_FFFF  64KB   UART1           RW
   * 0x00_310D_0000  0x00_310D_FFFF  64KB   UART2           RW
   * 0x00_310E_0000  0x00_310E_FFFF  64KB   IIC0            RW
   * 0x00_310F_0000  0x00_310F_FFFF  64KB   IIC1            RW
   * 0x00_3110_0000  0x00_3110_FFFF  64KB   IIC2            RW
   * 0x00_3111_0000  0x00_3111_FFFF  64KB   GPIO            RW
   * 0x00_3112_0000  0x00_3112_FFFF  64KB   CRU             RW
   * 0x00_3113_0000  0x00_3113_FFFF  64KB   WDT             RW
   * 0x00_3114_0000  0x00_3114_FFFF  64KB   USB2_PHY0       RW
   * 0x00_3115_0000  0x00_3115_FFFF  64KB   USB2_PHY1       RW
   * 0x00_3116_0000  0x00_3116_FFFF  64KB   USB2_PHY2       RW
   * 0x00_3117_0000  0x00_3117_FFFF  64KB   USB2_PHY3       RW
   * 0x00_3118_0000  0x00_3118_FFFF  64KB   USB3_PHY0       RW
   * 0x00_3119_0000  0x00_3119_FFFF  64KB   USB3_PHY1       RW
   * 0x00_311a_0000  0x00_311a_FFFF  64KB   USB3_PHY2       RW
   * 0x00_311b_0000  0x00_311b_FFFF  64KB   USB3_PHY3       RW
   * 0x00_311c_0000  0x00_311c_FFFF  64KB   PCIE0_CFG       RW
   * 0x00_311d_0000  0x00_311d_FFFF  64KB   PCIE1_CFG       RW
   * 0x00_311e_0000  0x00_311e_FFFF  64KB   PCIE2_CFG       RW
   * 0x00_311f_0000  0x00_311f_FFFF  64KB   PCIE3_CFG       RW
   * 0x00_3120_0000  0x00_3120_FFFF  64KB   SYSCFG          RW
   * 0x00_3121_0000  0x00_3130_FFFF  1MB    DATA_CPU_BRIDGE RW
   * 0x00_3131_0000  0x00_37FF_FFFF         Reserved
   * 0x00_3800_0000  0x00_3800_FFFF  64KB   CLINT (In cpu)  RW
   * 0x00_3801_0000  0x00_3801_FFFF         Reserved
   * 0x00_3802_0000  0x00_3802_0FFF  4KB    Debug (In cpu)  RW
   * 0x00_3802_1000  0x00_38FF_FFFF         Reserved
   * 0x00_3900_0000  0x00_3900_0FFF  4KB    CacheCtrl       RW
   * 0x00_3900_1000  0x00_3900_1FFF  4KB    Core Reset      RW
   * 0x00_3900_2000  0x00_3BFF_FFFF         Reserved
   * 0x00_3C00_0000  0x00_3FFF_FFFF         PLIC (In cpu)   RW
   * 0x00_4000_0000  0x00_4FFF_FFFF  256MB  PCIe0           RW
   * 0x00_5000_0000  0x00_5FFF_FFFF  256MB  PCIe1           RW
   * 0x00_6000_0000  0x00_6FFF_FFFF  256MB  PCIe2           RW
   * 0x00_7000_0000  0x00_7FFF_FFFF  256MB  PCIe3           RW
   * 0x00_8000_0000  0x1F_FFFF_FFFF  126GB  DDR             RWXIDSA
   */

  implicit val p: Parameters
  def pma_init() : (Vec[UInt], Vec[UInt], Vec[UInt]) = {
    def genAddr(init_addr: BigInt) = {
      init_addr.U((PMPAddrBits - PMPOffBits).W)
    }
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
    def addPMA(base_addr: BigInt,
               range: BigInt = 0L, // only use for napot mode
               l: Boolean = false,
               c: Boolean = false,
               atomic: Boolean = false,
               a: Int = 0,
               x: Boolean = false,
               w: Boolean = false,
               r: Boolean = false) = {
      val addr = if (a < 2) { shift_addr(base_addr) }
        else { get_napot(base_addr, range) }
      cfg_list.append(PMPConfigUInt(l, c, atomic, a, x, w, r))
      addr_list.append(genAddr(addr))
      mask_list.append(genMask(addr, a))
    }

    if (p(SoCParamsKey).LvnaEnable){
      addPMA(0x880000000L, c = true, atomic = true, a = 1, x = true, w = true, r = true)
    }
    else{
      addPMA(0x480000000L, c = true, atomic = true, a = 1, x = true, w = true, r = true)
    }
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
    if (p(SoCParamsKey).LvnaEnable) {
      addPMA(0x10000000L, a = 1, w = true, r = true)
    }
    else {
      addPMA(0x10000000L, a = 1)
    }
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

  def get_napot(base: BigInt, range: BigInt): BigInt = {
    val PlatformGrainBytes = (1 << PlatformGrain)
    if ((base % PlatformGrainBytes) != 0) {
      println("base:%x", base)
    }
    if ((range % PlatformGrainBytes) != 0) {
      println("range: %x", range)
    }
    require((base % PlatformGrainBytes) == 0)
    require((range % PlatformGrainBytes) == 0)

    ((base + (range/2 - 1)) >> PMPOffBits)
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
