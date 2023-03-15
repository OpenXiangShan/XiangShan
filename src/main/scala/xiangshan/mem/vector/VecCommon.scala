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
  val inner_idx = UInt(3.W) // the number index among 8 uop
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
  val vecdata = UInt(VLEN.W)
  val mask = UInt((VLEN/8).W)
  val rob_idx_valid = Vec(2,Bool())
  val rob_idx = Vec(2,UInt(log2Up(RobSize).W))
  val offset = Vec(2,UInt(4.W))
  val reg_offset = Vec(2,UInt(4.W))
}

object VecGenMask {
  def apply(rob_idx_valid: Vec[Bool], reg_offset: Vec[UInt], offset: Vec[UInt], mask: UInt):Vec[UInt] = {
    val vMask = VecInit(Seq.fill(2)(0.U(16.W)))
    for (i <- 0 until 2){
      when (rob_idx_valid(i)) {
        when (offset(i) <= reg_offset(i)) {
          vMask(i) := mask << (reg_offset(i) - offset(i))
        }.otherwise {
          vMask(i) := mask >> (offset(i) - reg_offset(i))
        }
      }
    }
    vMask
  }
}


object VecGenData {
  def apply (rob_idx_valid: Vec[Bool], reg_offset: Vec[UInt], offset: Vec[UInt], data:UInt):Vec[UInt] = {
    val vData = VecInit(Seq.fill(2)(0.U(128.W)))
    for (i <- 0 until 2){
      when (rob_idx_valid(i)) {
        when (offset(i) <= reg_offset(i)) {
          vData(i) := data << ((reg_offset(i) - offset(i)) << 3.U)
        }.otherwise {
          vData(i) := data >> ((offset(i) - reg_offset(i)) << 3.U)
        }
      }
    }
    vData
  }
}
