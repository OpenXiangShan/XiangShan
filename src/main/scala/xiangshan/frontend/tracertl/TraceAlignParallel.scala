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
import xiangshan.cache.mmu.TLBDiffId.lastHartId
import xiangshan.backend.fu.NewCSR.CSRNamedConstant.ContextStatus.off
import utility.ParallelOR

class TraceAlignParallel(implicit p: Parameters) extends TraceModule
  with HasPdConst {
  val io = IO(new TraceAlignToIFUCutIO())

  val width = io.traceInsts.bits.size
  require(width == io.instRange.getWidth, f"Width of traceInsts ${width} and instRange ${io.instRange.getWidth} should be the same")

  val rawInstsValid = io.traceInsts.valid // this should always be true, only can be false in Sim-FPGA mode
  val rawInsts = io.traceInsts.bits
  val startPC = io.predStartAddr
  // consectiveWithLast map with rawInsts
  val consectiveWithLast = Wire(Vec(width, Bool()))
  val stillConsective = Wire(Vec(width, Bool()))
  val addrInRange = Wire(Vec(width, Bool()))
  // offsetVec map with rawInsts
  val offsetVec = Wire(Vec(width, UInt(log2Ceil(PredictWidth).W)))
  // positionVec map with rawInsts, record each inst's position in the cuted Insts
  val positionHotVec = Wire(Vec(width, UInt(PredictWidth.W)))
  val taken2BVec = Wire(Vec(width, Bool()))

  val endPC = startPC + (PopCount(io.instRange) << 1.U)

  dontTouch(startPC)
  dontTouch(consectiveWithLast)
  dontTouch(stillConsective)
  dontTouch(offsetVec)
  dontTouch(positionHotVec)
  dontTouch(taken2BVec)
  dontTouch(addrInRange)

  (0 until width).foreach{i =>
    if (i == 0) {
      consectiveWithLast(i) := rawInsts(i).pcVA === (startPC + Mux(io.lastHalfValid, 2.U, 0.U))
    } else {
      consectiveWithLast(i) := rawInsts(i).pcVA === (rawInsts(i-1).pcVA + Mux(isRVC(rawInsts(i-1).inst), 2.U, 4.U))
    }
    offsetVec(i) := (rawInsts(i).pcVA - startPC) >> 1.U
  }

  val positionHotMerge = positionHotVec.reduce(_ | _)
  dontTouch(positionHotMerge)

  (0 until width).foreach{i =>
    // map to rawInsts
    // check for the startPC
    stillConsective(i) := consectiveWithLast.take(i+1).foldRight(true.B) { (x, y) => x && y }
    // check for the endPC
    addrInRange(i) := rawInsts(i).pcVA < endPC

    positionHotVec(i) := Mux(stillConsective(i) && addrInRange(i), UIntToOH(offsetVec(i), PredictWidth),
      0.U(PredictWidth.W))

    // map to cutInsts
    XSError(PopCount(positionHotVec.map(_(i))) > 1.U, s"Position hot vec has more than one hot bit at index $i")
    io.cutInsts(i).bits := Mux1H(positionHotVec.map(_(i)), rawInsts)
    io.cutInsts(i).valid := positionHotMerge(i) && io.instRange(i)
    taken2BVec(i) := io.cutInsts(i).valid && !isRVC(io.cutInsts(i).bits.inst) && (if (i == (width-1)) true.B else !io.instRange(i+1))
  }
  // FIXME: positionHot has hole, full full hot, unlike instRange, so the traceRange is different with older edition
  io.traceRange := positionHotMerge & io.instRange
  io.pdValid := positionHotMerge & io.instRange

  def isTaken2B(range: UInt): Bool = {
    val lastIdx = ParallelPosteriorityEncoder(range)
    !isRVC(io.cutInsts(lastIdx).bits.inst) && io.cutInsts(lastIdx).valid
  }
  io.traceRangeTaken2B := Cat(taken2BVec).orR
  io.instRangeTaken2B := false.B

  io.traceForceJump := rawInsts.head.isForceJump
  when (io.traceForceJump) {
    io.cutInsts.head.valid := true.B
    io.cutInsts.head.bits := io.traceInsts.bits(0)
    io.pdValid := 1.U
  }
}