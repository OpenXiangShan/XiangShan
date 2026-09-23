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

package xiangshan.cache.axi

import chisel3._
import chisel3.util.log2Ceil
import chisel3.util.ImplicitConversions._
import chisel3.DontCare
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.amba.axi4.AXI4RegisterNode
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BusErrorUnitParams, BusErrors}
import freechips.rocketchip.util.property
import xiangshan._

class BusErrorUnitAXI[T <: BusErrors](t: => T, params: BusErrorUnitParams)(implicit p: Parameters) extends LazyModule
  with HasXSParameter {
  val regWidth = 64
  val device = new SimpleDevice("bus-error-unit", Seq("sifive,buserror0"))
  val intNode = IntSourceNode(IntSourcePortSimple(resources = device.int))
  val node = AXI4RegisterNode(
    address   = AddressSet(params.addr, params.size - 1),
    beatBytes = XLEN / 8
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val errors = Flipped(t)
      val interrupt = Output(Bool())
    })

    val sources_and_desc = io.errors.toErrorList
    val sources = sources_and_desc.map(_.map(_._1))
    val sources_enums = sources_and_desc.zipWithIndex.flatMap{case (s, i) => s.map {e => (BigInt(i) -> (e._2, e._3))}}

    val causeWidth = log2Ceil(sources.lastIndexWhere(_.nonEmpty) + 1)
    val (cause, cause_desc) = DescribedReg(UInt(causeWidth.W),
      "cause", "Cause of error event", reset=Some(0.U(causeWidth.W)), volatile=true, enumerations=sources_enums.toMap)

    val (value, value_desc) = DescribedReg(UInt(sources.flatten.map(_.bits.getWidth).max.W),
      "value", "Physical address of error event", reset=None, volatile=true)
    require(value.getWidth <= regWidth)

    val enable = RegInit(VecInit(sources.map(_.nonEmpty.B)))
    val enable_desc =  sources.zipWithIndex.map { case (s, i) =>
      if (s.nonEmpty) RegFieldDesc(s"enable_$i", "", reset=Some(1))
      else RegFieldDesc.reserved
    }

    val global_interrupt = RegInit(VecInit.fill(sources.size)(false.B))
    val global_interrupt_desc = sources.zipWithIndex.map { case (s, i) =>
      if (s.nonEmpty) RegFieldDesc(s"plic_interrupt_$i", "", reset=Some(0))
      else RegFieldDesc.reserved
    }

    val accrued = RegInit(VecInit.fill(sources.size)(false.B))
    val accrued_desc = sources.zipWithIndex.map { case (s, i) =>
      if (s.nonEmpty) RegFieldDesc(s"accrued_$i", "", reset=Some(0), volatile = true)
      else RegFieldDesc.reserved
    }

    val local_interrupt = RegInit(VecInit.fill(sources.size)(false.B))
    val local_interrupt_desc = sources.zipWithIndex.map { case (s, i) =>
      if (s.nonEmpty) RegFieldDesc(s"local_interrupt_$i", "", reset=Some(0))
      else RegFieldDesc.reserved
    }

    val cause_wen = WireDefault(false.B)
    val new_cause = Wire(UInt(causeWidth.W))
    new_cause := DontCare
    val new_value = Wire(UInt(value.getWidth.W))
    new_value := DontCare
    for ((((s, en), acc), i) <- (sources zip enable zip accrued).zipWithIndex; if s.nonEmpty) {
      when (s.get.valid) {
        acc := true
        when (en) {
          cause_wen := true
          new_cause := i
          new_value := s.get.bits
        }
        property.cover(en, s"BusErrorCause_$i", s"Core;;BusErrorCause $i covered")
      }
    }

    when (cause === 0 && cause_wen) {
      cause := new_cause
      value := new_value
    }

    val (int_out, _) = intNode.out(0)
    io.interrupt := (accrued.asUInt & local_interrupt.asUInt).orR
    int_out(0) := (accrued.asUInt & global_interrupt.asUInt).orR

    def reg(r: UInt, gn: String, d: RegFieldDesc) = RegFieldGroup(gn, None, RegField.bytes(r, (r.getWidth + 7)/8, Some(d)))
    def reg(v: Vec[Bool], gn: String, gd: String, d: Seq[RegFieldDesc]) =
      RegFieldGroup(gn, Some(gd), (v zip d).map {case (r, rd) => RegField(1, r, rd)})
    def numberRegs(x: Seq[Seq[RegField]]) = x.zipWithIndex.map {case (f, i) => (i * regWidth / 8) -> f }

    node.regmap(numberRegs(Seq(
      reg(cause, "cause", cause_desc),
      reg(value, "value", value_desc),
      reg(enable, "enable", "Event enable mask", enable_desc),
      reg(global_interrupt, "plic_interrupt", "Platform-level interrupt enable mask", global_interrupt_desc),
      reg(accrued, "accrued", "Accrued event mask" ,accrued_desc),
      reg(local_interrupt,  "local_interrupt", "Hart-local interrupt-enable mask", local_interrupt_desc))):_*)

    // hardwire mask bits for unsupported sources to 0
    for ((s, i) <- sources.zipWithIndex; if s.isEmpty) {
      enable(i) := false
      global_interrupt(i) := false
      accrued(i) := false
      local_interrupt(i) := false
    }
  }
}
