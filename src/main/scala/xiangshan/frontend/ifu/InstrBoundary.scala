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
    class InstrBoundaryReq(implicit p: Parameters) extends IfuBundle {
      val instrRange: Vec[Bool] = Vec(FetchBlockInstNum, Bool())
      // FIXME: magic number 512
      val cacheData:             UInt = UInt(512.W)
      val firstInstrIsHalfRvi:   Bool = Bool()
      val firstFetchBlockEndPos: UInt = UInt(log2Ceil(FetchBlockInstNum).W)
      val endPos:                UInt = UInt(log2Ceil(FetchBlockInstNum).W)
    }
    class InstrBoundaryResp(implicit p: Parameters) extends IfuBundle {
      val instrValid:                        Vec[Bool] = Vec(FetchBlockInstNum, Bool())
      val isRvc:                             Vec[Bool] = Vec(FetchBlockInstNum, Bool())
      val firstFetchBlockLastInstrIsHalfRvi: Bool      = Bool()
      val lastInstrIsHalfRvi:                Bool      = Bool()
    }

    val req:  InstrBoundaryReq  = Flipped(new InstrBoundaryReq)
    val resp: InstrBoundaryResp = new InstrBoundaryResp
  }
  val io: InstrBoundaryIO = IO(new InstrBoundaryIO)

  // FIXME: magic number 32, shoule be FetBlockInstNum after it changes to 32
  private val data = io.req.cacheData.asTypeOf(Vec(32, UInt(16.W)))

  private val rawInstrs = VecInit((0 until FetchBlockInstNum).map(i =>
    if (i == FetchBlockInstNum - 1) data(i) else Cat(data(i + 1), data(i))
  ))
  private val isRvc = VecInit(rawInstrs.map(isRVC))

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
        if (i == start) !firstInstrIsHalfRvi else !boundary(i - 1) || isRvc(i - 1)
      }
    }
  }

  generateBoundary(boundary, 0, FetchBlockInstNum / 2, io.req.firstInstrIsHalfRvi)
  generateBoundary(latterHalfBoundary1, FetchBlockInstNum / 2, FetchBlockInstNum, true.B)
  generateBoundary(latterHalfBoundary2, FetchBlockInstNum / 2, FetchBlockInstNum, false.B)

  for (i <- FetchBlockInstNum / 2 until FetchBlockInstNum) {
    boundary(i) := Mux(
      boundary(FetchBlockInstNum / 2 - 1) && !isRvc(FetchBlockInstNum / 2 - 1),
      latterHalfBoundary1(i),
      latterHalfBoundary2(i)
    )
  }

  io.resp.instrValid := boundary.zip(isRvc).zip(io.req.instrRange).map { case ((boundary, isRvc), range) =>
    (!boundary || isRvc) && range
  }
  io.resp.isRvc := boundary.zip(isRvc).zip(io.req.instrRange).map { case ((boundary, isRvc), range) =>
    boundary && isRvc && range
  }
  io.resp.firstFetchBlockLastInstrIsHalfRvi :=
    boundary(io.req.firstFetchBlockEndPos) && !isRvc(io.req.firstFetchBlockEndPos)
  io.resp.lastInstrIsHalfRvi := boundary(io.req.endPos) && !isRvc(io.req.endPos)

  // For differential test only. Will be optimized out in release
  private val boundDiff = WireInit(VecInit(Seq.fill(FetchBlockInstNum)(false.B)))
  generateBoundary(boundDiff, 0, FetchBlockInstNum, io.req.firstInstrIsHalfRvi)
  boundary.zip(boundDiff).foreach { case (a, b) => XSError(a =/= b, p"boundary different: $a vs $b\n") }
}
