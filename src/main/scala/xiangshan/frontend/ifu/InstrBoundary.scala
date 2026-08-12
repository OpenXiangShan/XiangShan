// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.ifu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSError

class InstrBoundary(implicit p: Parameters) extends IfuModule with PreDecodeHelper {
  class InstrBoundaryIO(implicit p: Parameters) extends IfuBundle {
    class Req(implicit p: Parameters) extends IfuBundle {
      val valid:               Bool            = Bool()
      val firstInstrIsHalfRvi: Bool            = Bool()
      val fetchBlock:          Vec[FetchBlock] = Vec(MaxFetchReqNum, new FetchBlock)
      val icacheData:          IfuData         = new IfuData
      val totalEndPos:         UInt            = UInt(FetchBlockInstOffsetWidth.W)
    }
    class Resp(implicit p: Parameters) extends IfuBundle {
      val rawInstrVec:       Vec[Instruction] = Vec(FetchBlockInstNum, new Instruction)
      val instrEndMask:      Vec[Bool]        = Vec(FetchBlockInstNum, Bool())
      val firstEndIsHalfRvi: Bool             = Bool()
      val totalEndIsHalfRvi: Bool             = Bool()
    }
    val req:  Req  = Flipped(new Req)
    val resp: Resp = new Resp
  }
  val io: InstrBoundaryIO = IO(new InstrBoundaryIO)

  private val data     = io.req.icacheData.data
  private val range    = io.req.icacheData.range
  private val maybeRvc = io.req.icacheData.maybeRvcMap
  private val blockSel = io.req.icacheData.blockSel

  // We compute the boundaries of instructions in the first half of the fetch block directly, and compute the boundaries
  // of instructions in the latter half in two cases in parallel. Then we can choose the correct case according to
  // whether the last instruction in the first half is a 16-bit instruction or not.
  private val boundary            = WireInit(VecInit(Seq.fill(FetchBlockInstNum)(false.B)))
  private val latterHalfBoundary1 = WireInit(VecInit(Seq.fill(FetchBlockInstNum)(false.B)))
  private val latterHalfBoundary2 = WireInit(VecInit(Seq.fill(FetchBlockInstNum)(false.B)))

  private def generateBoundary(
      boundary:            Vec[Bool],
      start:               Int,
      end:                 Int,
      firstInstrIsHalfRvi: Bool
  ): Unit = {
    require(HasCExtension, "C Extension can not be disabled in XiangShan")
    for (i <- start until end) {
      boundary(i) := {
        if (i == start) !firstInstrIsHalfRvi else !boundary(i - 1) || maybeRvc(i - 1)
      }
    }
  }

  generateBoundary(boundary, 0, FetchBlockInstNum / 2, io.req.firstInstrIsHalfRvi)
  generateBoundary(latterHalfBoundary1, FetchBlockInstNum / 2, FetchBlockInstNum, true.B)
  generateBoundary(latterHalfBoundary2, FetchBlockInstNum / 2, FetchBlockInstNum, false.B)

  for (i <- FetchBlockInstNum / 2 until FetchBlockInstNum) {
    boundary(i) := Mux(
      boundary(FetchBlockInstNum / 2 - 1) && !maybeRvc(FetchBlockInstNum / 2 - 1),
      latterHalfBoundary1(i),
      latterHalfBoundary2(i)
    )
  }

  io.resp.rawInstrVec := (0 until FetchBlockInstNum).map { i =>
    val instr   = Wire(new Instruction)
    val isStart = boundary(i)

    val crossBlockFallThrough =
      if (i == FetchBlockInstNum - 1) {
        false.B
      } else {
        io.req.fetchBlock(1).valid && !io.req.fetchBlock(0).takenCfiOffset.valid &&
        (i.U === io.req.fetchBlock(0).takenCfiOffset.bits) &&
        !blockSel(i) && blockSel(i + 1)
      }
    instr.valid := {
      if (i == 0)
        io.req.firstInstrIsHalfRvi || isStart
      else isStart
    } && range(i)

    instr.data := {
      if (i == FetchBlockInstNum - 1)
        Cat(0.U(16.W), data(i))
      else
        Cat(data(i + 1), data(i))
    }

    instr.isRvc := isStart && maybeRvc(i)
    // Not involved in the instruction delimiting logic; timing impact should be controllable.
    val fixedBlockSel = blockSel(i) || (crossBlockFallThrough && !maybeRvc(i))
    instr.blockSel := fixedBlockSel

    instr.isPredTaken       := false.B
    instr.invalidTaken      := false.B
    instr.isPrevEndHalfRvi  := false.B
    instr.isCrossBlockInstr := crossBlockFallThrough && !maybeRvc(i)

    val startOffset = Mux(
      fixedBlockSel,
      i.U(FetchBlockInstOffsetWidth.W) - io.req.fetchBlock(0).size,
      i.U(FetchBlockInstOffsetWidth.W)
    )
    instr.startOffset := startOffset
    instr.endOffset   := Mux(maybeRvc(i), startOffset, startOffset + 1.U)

    instr
  }

  io.resp.instrEndMask := boundary.zip(maybeRvc.asBools).zip(range.asBools).map { case ((boundary, isRvc), range) =>
    (!boundary || (boundary && isRvc)) && range
  }

  private val firstEndPos = io.req.fetchBlock(0).takenCfiOffset.bits
  io.resp.firstEndIsHalfRvi := range(firstEndPos) && boundary(firstEndPos) && !maybeRvc(firstEndPos)

  private val totalEndPos = io.req.totalEndPos
  io.resp.totalEndIsHalfRvi := range(totalEndPos) && boundary(totalEndPos) && !maybeRvc(totalEndPos)

  // For differential test only. Will be optimized out in release
  private val boundDiff = WireInit(VecInit(Seq.fill(FetchBlockInstNum)(false.B)))
  generateBoundary(boundDiff, 0, FetchBlockInstNum, io.req.firstInstrIsHalfRvi)
  boundary.zip(boundDiff).foreach {
    case (a, b) => XSError(io.req.valid && (a =/= b), p"boundary different: $a vs $b\n")
  }
}
