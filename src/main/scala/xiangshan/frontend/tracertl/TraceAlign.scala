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
import utility.{ParallelPosteriorityEncoder}
import utils.XSError

class TraceAlignToIFUCutIO(implicit p: Parameters) extends TraceBundle {
  val debug_valid = Input(Bool())

  val traceInsts = Input(Vec(PredictWidth, new TraceInstrBundle()))
  // preDInfo's startAddr
  val predStartAddr = Input(UInt(VAddrBits.W))
  // instRange come from BPU's prediction: 'startAddr to nextStartAddr' & 'ftqOffset'
  val instRange = Input(UInt(PredictWidth.W))
  // When lastHalfValid is true, then the first 2bytes is used by last fetch
  // So set the first TraceInstrBundle to invalid
  val lastHalfValid = Input(Bool())

  val cutInsts = Output(Vec(PredictWidth, Valid(new TraceInstrBundle)))
  val traceRange = Output(UInt(PredictWidth.W))
  val traceRangeTaken2B = Output(Bool())
  val instRangeTaken2B = Output(Bool())
}

class TraceAlignToIFUCut(implicit p: Parameters) extends TraceModule {
  val io = IO(new TraceAlignToIFUCutIO())

  val width = io.traceInsts.size
  require(width == io.instRange.getWidth, f"Width of traceInsts ${width} and instRange ${io.instRange.getWidth} should be the same")
  val traceRangeVec = Wire(Vec(width, Bool()))
  val lastInstEndVec = Wire(Vec(PredictWidth + 1, Bool()))
  val curInstIdxVec = Wire(Vec(PredictWidth + 1, UInt(log2Ceil(width).W)))
  io.traceRange := traceRangeVec.asUInt
  io.traceRangeTaken2B := isTaken2B(io.traceRange)
  io.instRangeTaken2B := isTaken2B(io.instRange)
  val startPC = io.predStartAddr

  def isTaken2B(range: UInt): Bool = {
    val lastIdx = ParallelPosteriorityEncoder(range)
    !isRVC(io.cutInsts(lastIdx).bits.inst) && io.cutInsts(lastIdx).valid
  }

  def isRVC(inst: UInt): Bool = (inst(1, 0) =/= 3.U)

  traceRangeVec.foreach(_ := false.B)
  lastInstEndVec.map(_ := false.B)
  curInstIdxVec.map(_ := 0.U)
  (0 until width).foreach { i =>
    val curPC = startPC + (i * 2).U
    val curTrace = io.traceInsts(curInstIdxVec(i))
    val stillConsecutive = traceRangeVec.take(i).foldRight(true.B)(_ && _)

    val inst = io.cutInsts(i)
    when(!io.instRange(i) || !stillConsecutive) {
      inst.valid := false.B
      inst.bits := (-1.S).asTypeOf(new TraceInstrBundle)
    }.elsewhen(lastInstEndVec(i)) {
      traceRangeVec(i) := curPC === curTrace.pc
      inst.valid := traceRangeVec(i)
      inst.bits := curTrace

      when(inst.valid) {
        lastInstEndVec(i + 1) := isRVC(inst.bits.inst)
        curInstIdxVec(i + 1) := curInstIdxVec(i) + 1.U
      }
    }.elsewhen(stillConsecutive) {
      inst.valid := false.B
      inst.bits := (-1.S).asTypeOf(new TraceInstrBundle)
      inst.bits.pc := curPC

      if (i == 0)
        traceRangeVec(i) := (curPC + 2.U) === curTrace.pc
      else {
        traceRangeVec(i) := true.B
        XSError(io.debug_valid && (curPC =/= (io.traceInsts(curInstIdxVec(i) - 1.U).pc + 2.U)),
          "traceRange should not be true.B at stillConsecutive path?")
      }

      lastInstEndVec(i + 1) := true.B
      curInstIdxVec(i + 1) := curInstIdxVec(i)
    }.otherwise {
      inst.valid := false.B
      inst.bits := (-1.S).asTypeOf(new TraceInstrBundle)

      XSError(true.B, "Should not reach here")
    }
  }
}
