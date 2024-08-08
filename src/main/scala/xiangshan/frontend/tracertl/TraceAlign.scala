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
import xiangshan.frontend.HasPdConst

class TraceAlignToIFUCutIO(implicit p: Parameters) extends TraceBundle {
  val debug_valid = Input(Bool())

  val traceInsts = Input(Vec(PredictWidth, new TraceInstrBundle()))
  // icacheData is align with ICacheHalfLine(256bits)
  val icacheData = Input(Valid(new TraceFakeICacheRespBundle()))
  // preDInfo's startAddr
  val predStartAddr = Input(UInt(VAddrBits.W))
  // instRange come from BPU's prediction: 'startAddr to nextStartAddr' & 'ftqOffset'
  val instRange = Input(UInt(PredictWidth.W))
  // When lastHalfValid is true, then the first 2bytes is used by last fetch
  // So set the first TraceInstrBundle to invalid
  val lastHalfValid = Input(Bool())

  val cutInsts = Output(Vec(PredictWidth, Valid(new TraceInstrBundle)))
  val traceRange = Output(UInt(PredictWidth.W))
  // use by predecode/predChecker/newFtq's false-hit update
  val pdValid = Output(UInt(PredictWidth.W))

  val traceRangeTaken2B = Output(Bool())
  val instRangeTaken2B = Output(Bool())
}

class TraceAlignToIFUCut(implicit p: Parameters) extends TraceModule
  with HasPdConst {
  val io = IO(new TraceAlignToIFUCutIO())

  val width = io.traceInsts.size
  require(width == io.instRange.getWidth, f"Width of traceInsts ${width} and instRange ${io.instRange.getWidth} should be the same")
  val traceRangeVec = Wire(Vec(width, Bool()))
  val lastInstEndVec = Wire(Vec(PredictWidth + 1, Bool()))
  val pdLastInstEndVec = Wire(Vec(PredictWidth + 1, Bool()))
  val curInstIdxVec = Wire(Vec(PredictWidth + 1, UInt(log2Ceil(width).W)))
  val stillConsecutiveVec = Wire(Vec(width, Bool()))
  val pdValidVec = Wire(Vec(PredictWidth, Bool()))
  val startPC = io.predStartAddr

  io.traceRange := traceRangeVec.asUInt
  io.pdValid := pdValidVec.asUInt
  io.traceRangeTaken2B := isTaken2B(io.traceRange)
  io.instRangeTaken2B := isTaken2B(io.instRange)

  val icacheInstVec = Wire(Vec(512/16-1, UInt(32.W)))
  val actualIdxVec = Wire(Vec(PredictWidth, UInt(blockOffBits.W)))
  val fetchInstVec = Wire(Vec(PredictWidth, UInt(32.W)))
  icacheInstVec.zipWithIndex.foreach{ case (inst, idx) =>
    val stride = 16
    val instSize = 32
    val rawData = Cat(io.icacheData.bits.data(1), io.icacheData.bits.data(0))
    inst := rawData(idx*stride + instSize - 1 , idx*stride)
  }
  (0 until PredictWidth).foreach { case idx =>
    actualIdxVec(idx) := Cat(0.U(1.W), startPC(blockOffBits-2, 1)) + idx.U // Require C-Ext
    fetchInstVec(idx) := icacheInstVec(actualIdxVec(idx))
  }
  dontTouch(icacheInstVec)
  dontTouch(actualIdxVec)
  dontTouch(fetchInstVec)

  dontTouch(traceRangeVec)
  dontTouch(lastInstEndVec)
  dontTouch(pdLastInstEndVec)
  dontTouch(curInstIdxVec)
  dontTouch(stillConsecutiveVec)
  dontTouch(io)

  def isTaken2B(range: UInt): Bool = {
    val lastIdx = ParallelPosteriorityEncoder(range)
    !isRVC(io.cutInsts(lastIdx).bits.inst) && io.cutInsts(lastIdx).valid
  }

  // def isRVC(inst: UInt): Bool = (inst(1, 0) =/= 3.U)

  pdValidVec.zipWithIndex.foreach {
    case (v, i) => v := pdLastInstEndVec(i)
  }
  traceRangeVec.foreach(_ := false.B)
  lastInstEndVec.map(_ := false.B)
  pdLastInstEndVec.foreach(_ := false.B)
  curInstIdxVec.map(_ := 0.U)
  lastInstEndVec(0) := !io.lastHalfValid
  pdLastInstEndVec(0) := !io.lastHalfValid

  (0 until width).foreach { i =>
    val curPC = startPC + (i * 2).U
    val curTrace = io.traceInsts(curInstIdxVec(i))
    stillConsecutiveVec(i) := traceRangeVec.take(i).foldRight(true.B)(_ && _)

    val inst = io.cutInsts(i)
    // predecode valid generate
    when (pdLastInstEndVec(i)) {
      pdLastInstEndVec(i + 1) := isRVC(fetchInstVec(i))
    }.otherwise {
      pdLastInstEndVec(i + 1) := true.B
    }

    when (io.debug_valid && io.instRange(i) && stillConsecutiveVec(i)) {
      XSError(lastInstEndVec(i) =/= pdLastInstEndVec(i), "lastCheck from trace and icache should equal")
    }

    when(!io.instRange(i) || !stillConsecutiveVec(i)) {
      inst.valid := false.B
      inst.bits := (-1.S).asTypeOf(new TraceInstrBundle)

      // wrong path dummy value for predecode
      inst.bits.pcVA := startPC + (i*2).U
      inst.bits.inst := fetchInstVec(i)
    }.elsewhen(lastInstEndVec(i)) {
      traceRangeVec(i) := curPC === curTrace.pcVA
      inst.valid := traceRangeVec(i)

      when (inst.valid) {
        inst.bits := curTrace

        when (io.debug_valid) {
          when (isRVC(inst.bits.inst)) {
            XSError(inst.bits.inst(15,0) =/= fetchInstVec(i)(15,0), "FakeICache's result should equal to tracefile")
          }.otherwise {
            XSError(inst.bits.inst(31,0) =/= fetchInstVec(i)(31,0), "FakeICache's result should equal to tracefile")
          }
        }
      }.otherwise {
        inst.bits := (-1.S).asTypeOf(new TraceInstrBundle)
        inst.bits.pcVA := startPC + (i*2).U
        inst.bits.inst := fetchInstVec(i)
      }

      when(inst.valid) {
        lastInstEndVec(i + 1) := isRVC(inst.bits.inst)
        curInstIdxVec(i + 1) := curInstIdxVec(i) + 1.U
      }
    }.elsewhen(stillConsecutiveVec(i)) {
      inst.valid := false.B
      inst.bits := (-1.S).asTypeOf(new TraceInstrBundle)
      inst.bits.pcVA := curPC

      if (i == 0)
        traceRangeVec(i) := (curPC + 2.U) === curTrace.pcVA
      else {
        traceRangeVec(i) := true.B
        XSError(io.debug_valid && (curPC =/= (io.traceInsts(curInstIdxVec(i) - 1.U).pcVA + 2.U)),
          "traceRange should not be true.B at stillConsecutive path?")
      }

      lastInstEndVec(i + 1) := true.B
      curInstIdxVec(i + 1) := curInstIdxVec(i)
    }.otherwise {
      inst.valid := false.B
      inst.bits := (-1.S).asTypeOf(new TraceInstrBundle)

      XSError(io.debug_valid, "Should not reach here")
    }
  }
}
