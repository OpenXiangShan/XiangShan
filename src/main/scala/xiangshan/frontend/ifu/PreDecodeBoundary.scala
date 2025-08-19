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
import utility.XSDebug
import utility.XSError
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.PrunedAddr

class PreDecodeBoundary(implicit p: Parameters) extends IfuModule with PreDecodeHelper {
  class PreDecodeBoundIO(implicit p: Parameters) extends IfuBundle {
    class PreDecodeBoundReq(implicit p: Parameters) extends IfuBundle {
      val instrRange:        Vec[Bool] = Vec(PredictWidth, Bool())
      val cacheData:         UInt      = UInt(512.W)
      val prevLastIsHalfRvi: Bool      = Bool()
      val firstEndPos:       UInt      = UInt(log2Ceil(PredictWidth).W)
      val endPos:            UInt      = UInt(log2Ceil(PredictWidth).W)
    }
    class PreDecodeBoundResp(implicit p: Parameters) extends IfuBundle {
      val instrValid:         Vec[Bool] = Vec(PredictWidth, Bool())
      val isRvc:              Vec[Bool] = Vec(PredictWidth, Bool())
      val isFirstLastHalfRvi: Bool      = Bool()
      val isLastHalfRvi:      Bool      = Bool()
    }

    val req:  Valid[PreDecodeBoundReq]  = Flipped(ValidIO(new PreDecodeBoundReq))
    val resp: Valid[PreDecodeBoundResp] = ValidIO(new PreDecodeBoundResp)
  }
  val io: PreDecodeBoundIO = IO(new PreDecodeBoundIO)

  private class BoundInfo extends Bundle {
    val isStart: Bool = Bool()
    val isEnd:   Bool = Bool()
  }

  private val data = io.req.bits.cacheData.asTypeOf(Vec(32, UInt(16.W)))
  private val rawInsts =
    VecInit((0 until PredictWidth).map(i => Cat(data(i + 1), data(i))))

  // if the former fetch block's last 2 Bytes is a valid end, we need delimitation from data(0)
  //   we compute the first half directly -> bound(0, PredictWidth/2-1) is correct
  private val bound = WireInit(VecInit(Seq.fill(PredictWidth)(0.U.asTypeOf(new BoundInfo))))
  //   and compute two cases of the second half in parallel, we can choose later and reduce latency
  //     - case 1: data(PredictWidth/2-1) is a valid end, so data(PredictWidth/2) is a valid start
  //               -> readBound(PredictWidth/2, PredictWidth-1) is correct
  //     - case 2: data(PredictWidth/2) is a valid end, so data(PredictWidth/2+1) is a valid start
  //               -> readBoundPlus1(PredictWidth/2, PredictWidth-1) is correct
  //
  //     compute directly: 0 -> 1 -> ... -> 32 => bound(0, PredictWidth-1)
  //
  //     ours:        first half 0  -> 1  -> ... -> 16 ->  |  => bound(0, PredictWidth/2-1)
  //                                                       v
  //           second half case1 17 -> 18 -> ... -> 32 -> Mux => bound(PredictWidth/2, PredictWidth-1)
  //           second half case2 17 -> 18 -> ... -> 32 --/
  //
  //   NOTE: we use (PredictWidth/2, PredictWidth-1) only, but we still use Vec[PredictWidth] to simplify the code
  private val rearBound      = WireInit(VecInit(Seq.fill(PredictWidth)(0.U.asTypeOf(new BoundInfo))))
  private val rearBoundPlus1 = WireInit(VecInit(Seq.fill(PredictWidth)(0.U.asTypeOf(new BoundInfo))))

  private val currentIsRvc = VecInit(rawInsts.map(isRVC))
  def genBound(
      bound:        Vec[BoundInfo],
      start:        Int,
      end:          Int,
      isAlt:        Boolean = false,
      preIsHalfRvi: Bool = false.B
  ): Unit = {
    // when !HasCExtension, data is stepped by 4, and every data is a valid instruction start
    // otherwise, data is stepped by 2, and data on pc+i*2 is:
    //   - a valid instruction start iff data on pc+(i-1)*2 is a valid instruction end
    //     - when i == startPoint, we need to check whether last half is a valid end
    def checkThisIsStart(lastIsEnd: Bool): Bool =
      if (!HasCExtension) true.B
      else lastIsEnd
    //   - a valid end iff:
    //     - it is a valid start and is a RVC instruction
    //     - or it is not a valid start (which implies pc+(i-1)*2 is a valid start)
    def checkThisIsEnd(thisIsStart: Bool, thisIsRvc: Bool): Bool =
      if (!HasCExtension) true.B
      else thisIsStart && thisIsRvc || !thisIsStart

    for (i <- start until end) {
      if (i == 0) {
        bound(0).isStart := checkThisIsStart(true.B)
        bound(0).isEnd   := Mux(preIsHalfRvi, true.B, checkThisIsEnd(bound(0).isStart, currentIsRvc(0)))
      } else {
        bound(i).isStart := checkThisIsStart(if (i == start) true.B else bound(i - 1).isEnd)
        bound(i).isEnd   := checkThisIsEnd(bound(i).isStart, currentIsRvc(i))
      }
    }
  }

  genBound(bound, 0, PredictWidth / 2, false, io.req.bits.prevLastIsHalfRvi)
  genBound(rearBound, PredictWidth / 2, PredictWidth, false, false.B)
  genBound(rearBoundPlus1, PredictWidth / 2 + 1, PredictWidth, false, false.B)

  // for xxxPlus1, PredictWidth / 2 must be a valid end, since we assume PredictWidth / 2 + 1 is a valid start
  // and, it must not be a valid start, otherwise, PredictWidth / 2 - 1 is a valid end and rearBound should be selected
  rearBoundPlus1(PredictWidth / 2).isStart := false.B
  rearBoundPlus1(PredictWidth / 2).isEnd   := true.B

  // if PredictWidth / 2 - 1 is a valid end, PredictWidth / 2 is a valid start, then rearBound is correct
  // otherwise, rearBoundPlus1 is correct
  private val rearBoundCorrect = bound(PredictWidth / 2 - 1).isEnd

  for (i <- PredictWidth / 2 until PredictWidth) {
    bound(i) := Mux(rearBoundCorrect, rearBound(i), rearBoundPlus1(i))
  }

  // we also compute the whole block directly for differential testing, this will be optimized out in released code
  private val boundDiff = WireInit(VecInit(Seq.fill(PredictWidth)(0.U.asTypeOf(new BoundInfo))))

  genBound(boundDiff, 0, PredictWidth, false, io.req.bits.prevLastIsHalfRvi)

  private val startMismatch = Wire(Bool())
  private val endMismatch   = Wire(Bool())

  startMismatch := (bound zip boundDiff).map { case (a, b) => a.isStart =/= b.isStart }.reduce(_ || _)
  endMismatch   := (bound zip boundDiff).map { case (a, b) => a.isEnd =/= b.isEnd }.reduce(_ || _)

  XSError(io.req.valid && startMismatch, p"start mismatch\n")
  XSError(io.req.valid && endMismatch, p"end mismatch\n")

  for (i <- 0 until PredictWidth) {
    io.resp.bits.instrValid(i) := bound(i).isStart && io.req.bits.instrRange(i)
  }
  io.resp.valid := io.req.valid
  io.resp.bits.isRvc := VecInit(io.resp.bits.instrValid.zip(currentIsRvc).map { case (valid, rvc) =>
    valid & rvc
  })
  io.resp.bits.isRvc(0) := Mux(io.req.bits.prevLastIsHalfRvi, false.B, io.resp.bits.instrValid(0) && currentIsRvc(0))
  io.resp.bits.isFirstLastHalfRvi := io.resp.bits.instrValid(io.req.bits.firstEndPos) &&
    !currentIsRvc(io.req.bits.firstEndPos)
  io.resp.bits.isLastHalfRvi := io.resp.bits.instrValid(io.req.bits.endPos) &&
    !currentIsRvc(io.req.bits.endPos)
}
