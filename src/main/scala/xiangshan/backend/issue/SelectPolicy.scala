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

  // TODO optimize timing
  var maskedEmptyVec = VecInit(io.validVec.asBools.map(v => !v))
  for (i <- 0 until config.numEnq) {
    io.allocate(i).valid := maskedEmptyVec.asUInt.orR
    io.allocate(i).bits := PriorityEncoderOH(maskedEmptyVec.asUInt)
    maskedEmptyVec = VecInit(maskedEmptyVec.zip(io.allocate(i).bits.asBools).map{ case (m, s) => m && !s })

    XSError(io.allocate(i).valid && PopCount(io.allocate(i).bits) =/= 1.U,
      p"allocate vec ${Binary(io.allocate(i).bits)} is not onehot")
    XSDebug(io.allocate(i).fire(), p"select for allocation: ${Binary(io.allocate(i).bits)}\n")
  }

  // TODO optimize timing
  var maskedRequest = VecInit(io.request.asBools)
  for (i <- 0 until config.numDeq) {
    io.grant(i).valid := maskedRequest.asUInt.orR
    io.grant(i).bits := PriorityEncoderOH(maskedRequest.asUInt)
    maskedRequest = VecInit(maskedRequest.zip(io.grant(i).bits.asBools).map{ case(m, s) => m && !s })

    XSError(io.grant(i).valid && PopCount(io.grant(i).bits.asBools) =/= 1.U,
      p"grant vec ${Binary(io.grant(i).bits)} is not onehot")
    XSDebug(io.grant(i).valid, p"select for issue request: ${Binary(io.grant(i).bits)}\n")
  }

}
