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
package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.experimental.chiselName
import xiangshan.cache.mmu.CAMTemplate

class JATable (implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    //r
    val r_tgtpc = Input(UInt(VAddrBits.W)) // tgtpc: target block pc
    val r_hit   = Output(Bool())
    val r_endpc = Output(UInt(VAddrBits.W)) // JA end pc
    //w
    val wen     = Input(Bool())
    val w_tgtpc = Input(UInt(VAddrBits.W))
    val w_endpc = Input(UInt(VAddrBits.W))
    val w_blckN = Input(UInt(JABlockNumWidth.W))
  })

  val (sets, ways) = (64, 4)
  val idx1Width = log2Ceil(sets)
  val idx2Width = log2Ceil(ways)
  val tagWidth = VAddrBits - idx1Width - idx2Width - instOffsetBits

  val vld_reg = RegInit(VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(0.B)))))
  val num_reg = RegInit(VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(0.U(JABlockNumWidth.W))))))
  val tag_reg = RegInit(VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(0.U((tagWidth).W))))))
  val end_reg = RegInit(VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(0.U((VAddrBits).W))))))

  // generate the tag and index for both read and write operations
  def getJATableIdx1(stpc: UInt) = stpc(instOffsetBits + idx1Width - 1, instOffsetBits)
  def getJATableIdx2(stpc: UInt) = stpc(instOffsetBits + idx1Width + idx2Width - 1, instOffsetBits + idx1Width)
  def getJATableTag(stpc: UInt) = stpc(VAddrBits - 1, instOffsetBits + idx1Width + idx2Width)
  
  val (rIdx1, rIdx2, rTag) = (getJATableIdx1(io.r_tgtpc), getJATableIdx2(io.r_tgtpc), getJATableTag(io.r_tgtpc))
  val (wIdx1, wIdx2, wTag) = (getJATableIdx1(io.w_tgtpc), getJATableIdx2(io.w_tgtpc), getJATableTag(io.w_tgtpc))

  // write logic
  val invalid = vld_reg(wIdx1)(wIdx2) && (tag_reg(wIdx1)(wIdx2) === wTag) && (io.w_blckN < JAMinBlockNum.U)
  val notWrite = vld_reg(wIdx1)(wIdx2) && (tag_reg(wIdx1)(wIdx2) === wTag) && (io.w_blckN >= num_reg(wIdx1)(wIdx2))
  val realWen = io.wen && !notWrite
  val invalid_en = io.wen && invalid
  assert(!(realWen && (io.w_blckN < JAMinBlockNum.U)))
  assert(!(vld_reg(rIdx1)(rIdx2) && (num_reg(rIdx1)(rIdx2) < JAMinBlockNum.U)))

  when(realWen){
    vld_reg(wIdx1)(wIdx2) := true.B
    num_reg(wIdx1)(wIdx2) := io.w_blckN
    tag_reg(wIdx1)(wIdx2) := wTag
    end_reg(wIdx1)(wIdx2) := io.w_endpc
  }
  when(invalid_en){
    vld_reg(wIdx1)(wIdx2) := false.B
  }
  assert(!(realWen && invalid_en))

  // read logic
  io.r_endpc := end_reg(rIdx1)(rIdx2)
  io.r_hit   := (tag_reg(rIdx1)(rIdx2) === rTag) && vld_reg(rIdx1)(rIdx2)
}
