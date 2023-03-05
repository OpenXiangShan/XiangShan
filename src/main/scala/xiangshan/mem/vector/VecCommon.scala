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

package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._

class VecOperand(implicit p: Parameters) extends XSBundleWithMicroOp {
  val vmask = UInt(VLEN.W) // the mask of inst which is readed from reg
  val baseaddr = UInt(VAddrBits.W) // base address from rs1
  val stride = UInt(XLEN.W) // stride from rs2
  val index = UInt(VLEN.W) // index from vs2
  val pvd = UInt(5.W) // physical vector register destination
  val lmul = UInt(3.W)
  val sew = UInt(2.W)
  val vma = Bool()
  val vta = Bool()
  val inner_idx = UInt(3.W) // the number index among 8 lsu_flow
  val vl = UInt(8.W)
  // TODO: How will OOO calculatr vector register numbers?
  //  (EEW / SEW) * LMUL or (vl * EEW) / VLEN ?
  //  So OOO will always use eew ?
  // val eew = UInt(3.W)
  val total_num = UInt(4.W) // An inst to how many uops
}

class VecDecode(implicit p: Parameters) extends XSBundle {
  val uop_segment_num = UInt(3.W)
  val uop_type = UInt(2.W)
  val mask_en = Bool()
  val uop_unit_stride_whole_reg = Bool()
  val uop_unit_stride_mask = Bool()
  val uop_unit_stride_fof = Bool()
  val uop_eew = UInt(3.W) // this is also the index width when the inst is a index load

  def apply(inst: UInt) = {
    this.uop_segment_num := inst(31, 29)
    this.uop_type := inst(27, 26)
    this.mask_en := inst(25)
    this.uop_unit_stride_whole_reg := (inst(24,20) === "b01000".U)
    this.uop_unit_stride_mask := (inst(24,20) === "b01011".U)
    this.uop_unit_stride_fof := (inst(24,20) === "b10000".U)
    this.uop_eew := inst(14, 12)
    this
  }
}

class VecExuOutput(implicit p: Parameters) extends ExuOutput {
  val flow_index = UInt(5.W)
}

object VecGenMask {
  def apply(idx: UInt): UInt = {
    LookupTree(idx, List(
      "b000".U -> Cat(0.U(56.W), Fill(8, true.B)),
      "b001".U -> Cat(0.U(48.W), Fill(8, true.B), 0.U(8.W)),
      "b010".U -> Cat(0.U(40.W), Fill(8, true.B), 0.U(16.W)),
      "b011".U -> Cat(0.U(32.W), Fill(8, true.B), 0.U(24.W)),
      "b100".U -> Cat(0.U(24.W), Fill(8, true.B), 0.U(32.W)),
      "b101".U -> Cat(0.U(16.W), Fill(8, true.B), 0.U(40.W)),
      "b110".U -> Cat(0.U(8.W), Fill(8, true.B), 0.U(48.W)),
      "b111".U -> Cat(Fill(8, true.B), 0.U(56.W))
    ))
  }
}

// TODO: How to merge discrete data together in the future?
object VecGenData{
  def apply(mask: UInt, data: UInt): UInt = {
    val result = WireInit(VecInit(Seq.fill(512)(false.B)))
    var j = 0
    for (i <- 0 until 64) {
      when (mask(i)) {
        result(j) := data(8 * i)
        result(j + 1) := data(8 * i + 1)
        result(j + 2) := data(8 * i + 2)
        result(j + 3) := data(8 * i + 3)
        result(j + 4) := data(8 * i + 4)
        result(j + 5) := data(8 * i + 5)
        result(j + 6) := data(8 * i + 6)
        result(j + 7) := data(8 * i + 7)
        j = j + 8
      }
    }
    result.asUInt(127, 0)
  }
}