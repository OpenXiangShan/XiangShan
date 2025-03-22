/** *************************************************************************************
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
 * ************************************************************************************* */

package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{ParallelPosteriorityEncoder, XSError}
import xiangshan.frontend.HasPdConst

class TraceFakeICacheWrapperIO(implicit p: Parameters) extends TraceBundle {
  val traceInsts = Input(Vec(PredictWidth, new TraceInstrBundle()))
  val icacheDataIn = Input(Valid(new TraceFakeICacheRespBundle()))
  val icacheDataOut = Output(Valid(new TraceFakeICacheRespBundle()))
}

class TraceFakeICacheWrapper(implicit p: Parameters) extends TraceModule {
  val io = IO(new TraceFakeICacheWrapperIO())

  val sliceNum = 512/8/2 /*32*/
  val icacheAddrAlign = Cat(io.icacheDataIn.bits.addr(VAddrBits - 1, 5), 0.U(5.W)) // 2**5 is half cache line size
  val icacheDataOriginVec = io.icacheDataIn.bits.data.asTypeOf(Vec(sliceNum, UInt(16.W)))
  val icacheDataAddrVec = VecInit((0 until sliceNum).map(i => icacheAddrAlign + (i*2).U))
  val icacheDataNewVec = WireInit(icacheDataOriginVec)

  def isRVC(inst: UInt) = (inst(1,0) =/= 3.U)

  for (i <- (PredictWidth - 1) to 0 by -1) {
    for (j <- 0 until sliceNum) {
      when (io.traceInsts(i).pcVA === icacheDataAddrVec(j)) {
        icacheDataNewVec(j) := io.traceInsts(i).inst(15, 0)
      }
      when ((io.traceInsts(i).pcVA + 2.U) === icacheDataAddrVec(j) && !isRVC(io.traceInsts(i).inst)) {
        icacheDataNewVec(j) := io.traceInsts(i).inst(31, 16)
      }
    }
  }

  io.icacheDataOut := io.icacheDataIn
  io.icacheDataOut.bits.data := icacheDataNewVec.asTypeOf(chiselTypeOf(io.icacheDataIn.bits.data))
}