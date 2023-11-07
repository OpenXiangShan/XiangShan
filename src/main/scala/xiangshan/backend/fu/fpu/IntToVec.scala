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

// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package xiangshan.backend.fu.fpu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{SignExt, ZeroExt}
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit}
import xiangshan.backend.fu.vector.Bundles.VSew

class IntToVec(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  private val scalaData = in.data.src(0)
  private val vsew = in.ctrl.fuOpType
  private val dataWidth = cfg.dataBits

  private val vecE8Data  = Wire(Vec(dataWidth /  8, UInt( 8.W)))
  private val vecE16Data = Wire(Vec(dataWidth / 16, UInt(16.W)))
  private val vecE32Data = Wire(Vec(dataWidth / 32, UInt(32.W)))
  private val vecE64Data = Wire(Vec(dataWidth / 64, UInt(64.W)))

  vecE8Data   := VecInit(Seq.fill(dataWidth /  8)(scalaData( 7, 0)))
  vecE16Data  := VecInit(Seq.fill(dataWidth / 16)(scalaData(15, 0)))
  vecE32Data  := VecInit(Seq.fill(dataWidth / 32)(scalaData(31, 0)))
  vecE64Data  := VecInit(Seq.fill(dataWidth  / 64)(scalaData(63, 0)))

  out.res.data := Mux1H(Seq(
    (vsew === VSew.e8)  -> vecE8Data.asUInt,
    (vsew === VSew.e16) -> vecE16Data.asUInt,
    (vsew === VSew.e32) -> vecE32Data.asUInt,
    (vsew === VSew.e64) -> vecE64Data.asUInt,
  ))
}
