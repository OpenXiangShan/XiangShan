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
import chisel3.experimental.SourceInfo
import xiangshan._

case class L1CacheCtrlParams (
  address: AddressSet,
  beatBytes: Int = 8,
) {
  def maxBanks    = 1
  def bankBytes   = 128

  def regWidth    = 64
  def regBytes    = regWidth / 8

  def ctrlOffset  = 0x0
  def delayOffset = ctrlOffset + regBytes
  def maskOffset  = delayOffset + regBytes

  def nSignalComps = 2
}

class CtrlUnitCtrlBundle(implicit p: Parameters) extends XSBundle with HasDCacheParameters {
  val zero0   = UInt((60-DCacheBanks).W)  // padding bits
  val bank    = UInt(DCacheBanks.W) // bank enable
  val comp    = UInt(1.W)   // components: 1'b0 tag, 1'b1 data
  val ede     = Bool()      // error delay enable
  val persist = Bool()  // persist inject
  val ese     = Bool()      // error signaling enable
}

class CtrlUnitSignalingBundle(implicit p: Parameters) extends XSBundle with HasDCacheParameters {
  val valid = Bool()
  val mask  = UInt(DCacheSRAMRowBits.W)
}

class CtrlUnit(params: L1CacheCtrlParams)(implicit p: Parameters) extends LazyModule
  with HasDCacheParameters
{
  val device: SimpleDevice = new SimpleDevice("L1DCacheCtrl", Seq("xiangshan,l1dcache_ctrl"))

  val node: TLRegisterNode = TLRegisterNode(
    address     = Seq(params.address),
    device      = device,
    beatBytes   = params.beatBytes,
    concurrency = 1,
  )

  lazy val module = new CtrlUnitImp

  class CtrlUnitImp extends LazyModuleImp(this) {
    val io_pseudoError = IO(Vec(params.nSignalComps, DecoupledIO(Vec(DCacheBanks, new CtrlUnitSignalingBundle))))

    require(params.maxBanks > 0, "At least one bank!")
    require(params.maxBanks == 1, "Is it necessary to have more than 1 bank?")
    require(params.regWidth == DCacheSRAMRowBits, "regWidth must be equal to DCacheSRAMRowBits!")
    val ctrlRegs  = RegInit(VecInit(Seq.fill(1)(0.U(params.regWidth.W))))
    val delayRegs = RegInit(VecInit(Seq.fill(1)(0.U(params.regWidth.W))))
    val maskRegs  = RegInit(VecInit(Seq.fill(DCacheBanks)(0.U(DCacheSRAMRowBits.W))))
    val counterRegs = RegInit(VecInit(Seq.fill(1)(0.U(params.regWidth.W))))

    io_pseudoError.zipWithIndex.foreach {
      case (inj, i) =>
        val ctrlReg = ctrlRegs.head
        val ctrlRegBundle = ctrlRegs.head.asTypeOf(new CtrlUnitCtrlBundle)
        val delayReg = delayRegs.head
        val counterReg = counterRegs.head

        require(log2Up(io_pseudoError.length) == ctrlRegBundle.comp.getWidth, "io_pseudoError must equal number of components!")
        inj.valid := ctrlRegBundle.ese && (ctrlRegBundle.comp === i.U) && (!ctrlRegBundle.ede || counterReg === 0.U)
        inj.bits.zip(ctrlRegBundle.bank.asBools).zip(maskRegs).map {
          case ((bankOut, bankEnable), mask) =>
            bankOut.valid := bankEnable
            bankOut.mask  := mask
        }

        when (inj.fire) {
          val newCtrlReg = WireInit(0.U.asTypeOf(ctrlRegBundle))
          newCtrlReg := ctrlRegBundle
          newCtrlReg.ese := Mux(ctrlRegBundle.persist, ctrlRegBundle.ese, false.B)

          when (newCtrlReg.ese && newCtrlReg.ede) {
            counterReg := Mux(newCtrlReg.persist, delayReg, 0.U)
          }
          ctrlReg := newCtrlReg.asUInt
        }
    }

    ctrlRegs.map(_.asTypeOf(new CtrlUnitCtrlBundle)).zip(counterRegs).zipWithIndex.foreach {
      case ((ctl, cnt), i) =>
        when (ctl.ese && ctl.ede && cnt =/= 0.U) {
          cnt := cnt - 1.U
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
        desc      = s"pseudo error delay $i",
        group     = Some(s"delay_${i}"),
        groupDesc = Some(s"pseudo error delay ${i}"),
        reset     = Some(0)
      )

    def maskRegDesc(i: Int) =
      RegFieldDesc(
        name      = s"mask_$i",
        desc      = s"pseudo error toggle mask$i",
        group     = Some(s"mask_${i}"),
        groupDesc = Some(s"pseudo error toggle mask ${i}"),
        reset     = Some(0)
      )

    def ctrlRegField(x: UInt, i: Int) = {
      RegField(params.regWidth, x, ctrlRegDesc(i))
    }

    def delayRegField(x: UInt, i: Int) = {
      RegField(params.regWidth,
        RegReadFn { valid =>
          (true.B, x)
        },
        RegWriteFn { (valid, data) =>
          when(valid) { x := data; counterRegs(i) := data }
          true.B
        },
        delayRegDesc(i)
      )
    }

    def maskRegField(x: UInt, i: Int) = {
      RegField(params.regWidth, x, maskRegDesc(i))
    }

    val ctrlRegFields = ctrlRegs.zipWithIndex.map {
      case (reg, i) =>
        params.ctrlOffset -> Seq(ctrlRegField(reg, i))
    }
    val delayRegFields = delayRegs.zipWithIndex.map {
      case (reg, i) =>
        params.delayOffset -> Seq(delayRegField(reg, i))
    }
    val maskRegFields = maskRegs.zipWithIndex.map {
      case (reg, i) =>
        (params.maskOffset + 8 * i) -> Seq(maskRegField(reg, i))
    }

    node.regmap((ctrlRegFields ++ delayRegFields ++ maskRegFields):_*)
  }
}