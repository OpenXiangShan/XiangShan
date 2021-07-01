/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.backend.exu


import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.fpu.IntToFP
import xiangshan.backend.fu.{CSR, CSRFileIO, Fence, FenceToSbuffer, FunctionUnit, Jump}

class JumpExeUnit(implicit p: Parameters) extends Exu(JumpExeUnitCfg)
{
  val csrio = IO(new CSRFileIO)
  val fenceio = IO(new Bundle {
    val sfence = Output(new SfenceBundle)
    val fencei = Output(Bool())
    val sbuffer = new FenceToSbuffer
  })

  val jmp = supportedFunctionUnits.collectFirst{
    case j: Jump => j
  }.get
  val csr = supportedFunctionUnits.collectFirst{
    case c: CSR => c
  }.get
  val fence = supportedFunctionUnits.collectFirst{
    case f: Fence => f
  }.get
  val i2f = supportedFunctionUnits.collectFirst {
    case i: IntToFP => i
  }.get

  csr.csrio <> csrio

  fenceio.sfence <> fence.sfence
  fenceio.fencei <> fence.fencei
  fenceio.sbuffer <> fence.toSbuffer
  fence.io.out.ready := true.B

  val uop = io.fromInt.bits.uop
  val instr_rm = uop.ctrl.fpu.rm
  i2f.rm := Mux(instr_rm =/= 7.U, instr_rm, csr.csrio.fpu.frm)

  val isDouble = !uop.ctrl.isRVF


  io.out.bits.redirectValid := jmp.redirectOutValid
  io.out.bits.redirect := jmp.redirectOut
}
