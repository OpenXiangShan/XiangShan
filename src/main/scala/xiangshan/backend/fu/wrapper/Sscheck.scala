/***************************************************************************************
* Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          https://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.fu.wrapper

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xiangshan.ExceptionNO.zicfissSoftwareCheck
import xiangshan.backend.fu.{FuConfig, PipedFuncUnit}

/** Internal shadow-stack return-address comparison executed by ALU0. */
class SscheckUnit(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  private val in = io.in.bits
  private val out = io.out.bits
  private val flushed = in.ctrl.robIdx.needFlush(io.flush)
  private val mismatch = in.data.src(0) =/= in.data.src(1)

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
  out.res.data := Mux(mismatch, 0.U(cfg.destDataBits.W), 8.U(cfg.destDataBits.W))
  connect0LatencyCtrlSingal
  out.ctrl.exceptionVec.zeroInit()
  out.ctrl.exceptionVec(zicfissSoftwareCheck) := io.in.valid && !flushed && mismatch
}
