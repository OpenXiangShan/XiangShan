
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
import xiangshan._
import xiangshan.backend._
import xiangshan.backend.Bundles._
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.backend.fu.FuType

class StdExeUnitIO(val param: ExeUnitParams)(implicit p: Parameters) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect()))
  val in = Flipped(DecoupledIO(new ExuInput(param, hasCopySrc = true)))
  val vstdIn = Flipped(ValidIO(new StoreQueueDataWrite))
  val out = DecoupledIO(new ExuOutput(param)) // std -> wb
  val atomicData = Valid(new ExuInput(param)) // std -> atomicsUnit
  val sqData = Valid(new StoreQueueDataWrite) // std -> sq
}

class StdExeUnit(val param: ExeUnitParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new StdExeUnitIO(param))

  // Arbitrate between scalar std and vector std
  io.in.ready := io.out.ready && !io.vstdIn.valid

  // writeback of scalar stds but not vector stds
  io.out.valid := io.in.valid && !io.vstdIn.valid && !FuType.storeIsAMO(io.in.bits.fuType)
  io.out.bits := 0.U.asTypeOf(io.out.bits)
  io.out.bits.data := VecInit(Seq.fill(param.wbPathNum)(io.in.bits.src(0)))
  io.out.bits.robIdx := io.in.bits.robIdx
  io.out.bits.pdest := io.in.bits.pdest
  io.out.bits.sqIdx.foreach(_ := io.in.bits.sqIdx.get)
  io.out.bits.debugInfo := io.in.bits.perfDebugInfo
  io.out.bits.debug_seqNum := io.in.bits.debug_seqNum

  io.atomicData.valid := io.in.fire && FuType.storeIsAMO(io.in.bits.fuType)
  io.atomicData.bits := io.in.bits

  // sq writeback of both scalar and vector stds
  io.sqData.valid := io.out.fire || io.vstdIn.valid
  io.sqData.bits.fuType := Mux(io.vstdIn.valid, io.vstdIn.bits.fuType, io.in.bits.fuType)
  io.sqData.bits.fuOpType := Mux(io.vstdIn.valid, io.vstdIn.bits.fuOpType, io.in.bits.fuOpType)
  io.sqData.bits.data := Mux(io.vstdIn.valid, io.vstdIn.bits.data, io.in.bits.src(0))
  io.sqData.bits.sqIdx := Mux(io.vstdIn.valid, io.vstdIn.bits.sqIdx, io.in.bits.sqIdx.get)
  io.sqData.bits.vecDebug := io.vstdIn.bits.vecDebug // DontCare for scalar stds
}