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

package xiangshan.cache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property
import chisel3.experimental.SourceInfo
import xiangshan._

trait HasCtrlUnitParameters extends HasXSParameter with HasDCacheParameters {
  def maxBanks: Int = 1
  def bankBytes: Int = 128
  def baseAddress = 0x38022000 + p(XSCoreParamsKey).HartId * bankBytes

  def regWidth = 64
  def regBytes = regWidth / 8
  def ctrlOffset  = 0x0
  def delayOffset = ctrlOffset + regBytes
  def maskOffset  = delayOffset + regBytes

  def size: Int = maxBanks * bankBytes
  def mask: Int = (1 << log2Ceil(size)) - 1

  require(regWidth == DCacheSRAMRowBits, s"regWidth = $regWidth must be equal DCacheSRAMRowBits = $DCacheSRAMRowBits!")
}

class CtrlUnitCtrlBundle extends Bundle {
  val zero0 = UInt(60.W)
  val comp  = UInt(2.W)
  val ece   = Bool()
  val ese   = Bool()
}

class CtrlUnit(implicit p: Parameters) extends LazyModule
  with HasCtrlUnitParameters
  with HasDCacheParameters
{
  val device: SimpleDevice = new SimpleDevice("L1DCacheCtrl", Seq("xiangshan,l1dcache_ctrl"))

  val node: TLRegisterNode = TLRegisterNode(
    address     = Seq(AddressSet(baseAddress, mask)),
    device      = device,
    beatBytes   = 8,
    concurrency = 1,
  )

  lazy val module = new CtrlUnitImp

  class CtrlUnitImp extends LazyModuleImp(this) {
    val io_eccCtrl = IO(Vec(nComponents, DecoupledIO(Vec(DCacheBanks, UInt(regWidth.W)))))

    val ctrlRegs = RegInit(VecInit(Seq.fill(1)(0.U(regWidth.W))))
    val delayRegs = RegInit(VecInit(Seq.fill(1)(0.U(regWidth.W))))
    val maskRegs = RegInit(VecInit(Seq.fill(DCacheBanks)(0.U(regWidth.W))))

    io_eccCtrl.zipWithIndex.foreach {
      case (ctl, i) =>
        val ctrlReg = ctrlRegs(0).asTypeOf(new CtrlUnitCtrlBundle)
        val delayReg = delayRegs(0)
        ctl.valid := ctrlReg.ese && ctrlReg.comp(i) && (!ctrlReg.ece || delayReg === 0.U)
        ctl.bits  := maskRegs

        when (ctl.fire) {
          val newCtrlReg = WireInit(0.U.asTypeOf(ctrlReg))
          newCtrlReg := ctrlReg
          newCtrlReg.ese := false.B

          ctrlRegs(0) := newCtrlReg.asUInt
        }
    }

    ctrlRegs.map(_.asTypeOf(new CtrlUnitCtrlBundle)).zip(delayRegs).foreach {
      case (ctl, delay) =>
        when (ctl.ese && ctl.ece && delay =/= 0.U) {
          delay := delay - 1.U
        }
    }

    def ctrlRegDesc(i: Int) =
      RegFieldDesc(
        name      = s"control_$i",
        desc      = s"Acting control of controller $i",
        group     = Some(s"controll_${i}"),
        groupDesc = Some(s"Acting control of controller ${i}"),
        reset     = Some(0)
      )

    def delayRegDesc(i: Int) =
      RegFieldDesc(
        name      = s"delay_$i",
        desc      = s"ecc inject delay $i",
        group     = Some(s"delay_${i}"),
        groupDesc = Some(s"ecc inject delay ${i}"),
        reset     = Some(0)
      )

    def maskRegDesc(i: Int) =
      RegFieldDesc(
        name      = s"mask_$i",
        desc      = s"ecc inject mask $i",
        group     = Some(s"mask_${i}"),
        groupDesc = Some(s"ecc inject mask ${i}"),
        reset     = Some(0)
      )

    def ctrlRegField(x: UInt, i: Int) = {
      RegField(regWidth, x, ctrlRegDesc(i))
    }

    def delayRegField(x: UInt, i: Int) = {
      RegField(regWidth, x, delayRegDesc(i))
    }

    def maskRegField(x: UInt, i: Int) = {
      RegField(regWidth, x, maskRegDesc(i))
    }

    val ctrlRegFields = ctrlRegs.zipWithIndex.map {
      case (reg, i) =>
        ctrlOffset -> Seq(ctrlRegField(reg, i))
    }
    val delayRegFields = delayRegs.zipWithIndex.map {
      case (reg, i) =>
        delayOffset -> Seq(delayRegField(reg, i))
    }
    val maskRegFields = maskRegs.zipWithIndex.map {
      case (reg, i) =>
        (maskOffset + 8 * i) -> Seq(maskRegField(reg, i))
    }

    node.regmap((ctrlRegFields ++ delayRegFields ++ maskRegFields):_*)
  }
}