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

package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class SelectPolicy(config: RSConfig)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    // select for enqueue
    val validVec = Input(UInt(config.numEntries.W))
    val allocate = Vec(config.numEnq, DecoupledIO(UInt(config.numEntries.W)))
    // select for issue
    val request = Input(UInt(config.numEntries.W))
    val grant = Vec(config.numDeq, DecoupledIO(UInt(config.numEntries.W))) //TODO: optimize it
  })

  val policy = if (config.numDeq > 2 && config.numEntries > 32) "oddeven" else if (config.numDeq > 2) "circ" else "naive"

  val emptyVec = VecInit(io.validVec.asBools.map(v => !v))
  val allocate = SelectOne(policy, emptyVec, config.numEnq)
  for (i <- 0 until config.numEnq) {
    val sel = allocate.getNthOH(i + 1)
    io.allocate(i).valid := sel._1
    io.allocate(i).bits := sel._2.asUInt

    XSError(io.allocate(i).valid && PopCount(io.allocate(i).bits) =/= 1.U,
      p"allocate vec ${Binary(io.allocate(i).bits)} is not onehot")
    XSDebug(io.allocate(i).fire(), p"select for allocation: ${Binary(io.allocate(i).bits)}\n")
  }

  // a better one: select from both directions
  val request = io.request.asBools
  val select = SelectOne(policy, request, config.numDeq)
  for (i <- 0 until config.numDeq) {
    val sel = select.getNthOH(i + 1)
    io.grant(i).valid := sel._1
    io.grant(i).bits := sel._2.asUInt

    XSError(io.grant(i).valid && PopCount(io.grant(i).bits.asBools) =/= 1.U,
      p"grant vec ${Binary(io.grant(i).bits)} is not onehot")
    XSDebug(io.grant(i).valid, p"select for issue request: ${Binary(io.grant(i).bits)}\n")
  }

}
