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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.Bundles.{MemExuInput, MemExuOutput}


class StoreDataUnit(regOut: Boolean = false)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val in = Flipped(DecoupledIO(new MemExuInput))
    val out = DecoupledIO(new MemExuOutput)
  })

  if (regOut) {
    val s1_ready = WireInit(false.B)

    // Pipeline
    // --------------------------------------------------------------------------------
    // stage 0
    // --------------------------------------------------------------------------------
    val s0_valid = io.in.valid
    val s0_out = io.in.bits
    val s0_can_go = s1_ready
    val s0_fire = s0_valid && s0_can_go


    // Pipeline
    // --------------------------------------------------------------------------------
    // stage 1
    // --------------------------------------------------------------------------------
    val s1_valid = RegInit(false.B)
    val s1_can_go = io.out.ready
    val s1_fire = s1_valid && s1_can_go
    val s1_in = RegEnable(s0_out, s0_fire)
    val s1_out = Wire(new MemExuOutput)
    s1_ready := io.out.ready

    when (s0_fire) { s1_valid := true.B }
    .elsewhen (s1_fire) { s1_valid := false.B }

    s1_out := DontCare
    s1_out.uop := s1_in.uop
    s1_out.data := s1_in.src(0)

    io.out.valid := s1_valid
    io.out.bits := s1_out
  } else {
    // Pipeline
    // --------------------------------------------------------------------------------
    // stage 0
    // --------------------------------------------------------------------------------
    val s0_valid = io.in.valid
    val s0_out = Wire(new MemExuOutput)

    s0_out := DontCare
    s0_out.uop := io.in.bits.uop
    s0_out.data := io.in.bits.src(0)

    io.out.valid := s0_valid
    io.out.bits := s0_out

    io.in.ready := io.out.ready
  }

}