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
import xiangshan.frontend.{HasPdConst, PreDecodeResp}
/**
 * TracePreDecoder
 *
 * Input: traceInst
 * Output: Precoded simple prediction result, keep same with the original PreDecoder of IFU
 * NOTE:
 * For IFU's PreDecoder: the startAddr and nextStartAddr will control inst valid
 * For Trace's PreDecoder: all the inst is at the arch path, but only the ones inside the startAddr and nextStartAddr will be "recv"(accept/handle)-ed now.
 */

class TracePreDecoder(implicit p: Parameters) extends TraceModule
  with TraceParams
  with HasPdConst {
  val io = IO(new Bundle {
    val traceInsts = Input(Vec(PredictWidth, Valid(new TraceInstrBundle())))
    val pdValid = Input(UInt(PredictWidth.W))

    val out = Output(new PreDecodeResp)
  })
  dontTouch(io)

  // Set it to false to ignore
  io.out.hasHalfValid.map(_ := false.B)
  io.out.triggered := 0.U.asTypeOf(io.out.triggered)

  for (i <- 0 until PredictWidth) {
    val pd = io.out.pd(i)
    val trace = io.traceInsts(i).bits

    val curIsRVC = isRVC(trace.inst)
    val brType :: isCall :: isRet :: Nil = brInfo(trace.inst)
    val jalOffset = jal_offset(trace.inst, curIsRVC)
    val brOffset = br_offset(trace.inst, curIsRVC)

    pd.valid := io.pdValid(i) // io.traceInsts(i).valid
    pd.isRVC := curIsRVC
    pd.brType := brType
    pd.isCall := isCall
    pd.isRet := isRet

    io.out.instr(i) := trace.inst
    io.out.jumpOffset(i) := Mux(pd.isBr, brOffset, jalOffset)
  }
}