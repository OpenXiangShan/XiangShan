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

class TraceAlignWrongPathIO(implicit p: Parameters) extends TraceBundle {
  val debug_valid = Input(Bool())

  val traceInsts = Input(Vec(PredictWidth, new TraceInstrBundle()))
  val cutInsts = Output(Vec(PredictWidth, Valid(new TraceInstrBundle)))
  val traceRange = Output(UInt(PredictWidth.W))
}

class TraceAlignWrongPath(implicit p: Parameters) extends TraceModule
  with HasPdConst {
  val io = IO(new TraceAlignWrongPathIO())

  val width = io.traceInsts.size
  val traceRangeVec = Wire(Vec(width, Bool()))
  val lastInstEndVec = Wire(Vec(PredictWidth + 1, Bool()))
  val curInstIdxVec = Wire(Vec(PredictWidth + 1, UInt(log2Ceil(width).W)))
  val stillConsecutiveVec = Wire(Vec(width, Bool()))
  val startPC = io.traceInsts.head.pcVA

  io.traceRange := traceRangeVec.asUInt

  dontTouch(traceRangeVec)
  dontTouch(lastInstEndVec)
  dontTouch(curInstIdxVec)
  dontTouch(stillConsecutiveVec)
  dontTouch(io)

  traceRangeVec.foreach(_ := false.B)
  lastInstEndVec.foreach(_ := false.B)
  curInstIdxVec.foreach(_ := 0.U)
  lastInstEndVec(0) := true.B

  (0 until width).foreach { i =>
    val curPC = startPC + (i * 2).U
    val curTrace = io.traceInsts(curInstIdxVec(i))
    stillConsecutiveVec(i) := traceRangeVec.take(i).foldRight(true.B) { (x, y) => x && y }

    val inst = io.cutInsts(i)

    when (!stillConsecutiveVec(i)) {
      inst.valid := false.B
      inst.bits := (-1.S).asTypeOf(new TraceInstrBundle)

      inst.bits.pcVA := startPC + (i * 2).U
      inst.bits.inst := 0.U
    }.elsewhen(lastInstEndVec(i)) {
      traceRangeVec(i) := curPC === curTrace.pcVA
      inst.valid := traceRangeVec(i)

      when (inst.valid) {
        inst.bits := curTrace
      }.otherwise {
        inst.bits := (-1.S).asTypeOf(new TraceInstrBundle)
        inst.bits.pcVA := startPC + (i * 2).U
        inst.bits.inst := 0.U
      }

      when (inst.valid) {
        lastInstEndVec(i + 1) := isRVC(inst.bits.inst)
        curInstIdxVec(i + 1) := curInstIdxVec(i) + 1.U
      }
    }.elsewhen(stillConsecutiveVec(i)) {
      inst.valid := false.B
      inst.bits := (-1.S).asTypeOf(new TraceInstrBundle)
      inst.bits.pcVA := curPC

      traceRangeVec(i) := true.B

      lastInstEndVec(i + 1) := true.B
      curInstIdxVec(i + 1) := curInstIdxVec(i)
    }.otherwise {
      inst.valid := false.B
      inst.bits := (-1.S).asTypeOf(new TraceInstrBundle)
      inst.bits.pcVA := curPC

      XSError(io.debug_valid, "TraceAlignWrongPath: unexpected path")
    }
  }
  io.cutInsts.foreach(_.bits.isWrongPath := true.B)
}
