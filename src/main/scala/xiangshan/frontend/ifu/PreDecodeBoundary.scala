// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
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

class PreDecodeBoundary(implicit p: Parameters) extends IfuModule with PreDecodeHelper {
  class PreDecodeBoundIO(implicit p: Parameters) extends IfuBundle {
    class PreDecodeBoundReq(implicit p: Parameters) extends IfuBundle {
      val instrRange:        Vec[Bool] = Vec(FetchBlockInstNum, Bool())
      val cacheData:         UInt      = UInt((FetchBlockSize * 8).W)
      val prevLastIsHalfRvi: Bool      = Bool()
      val firstEndPos:       UInt      = UInt(FetchBlockInstOffsetWidth.W)
      val endPos:            UInt      = UInt(FetchBlockInstOffsetWidth.W)
    }
    class PreDecodeBoundResp(implicit p: Parameters) extends IfuBundle {
      val instrValid:         Vec[Bool] = Vec(FetchBlockInstNum, Bool())
      val isRvc:              Vec[Bool] = Vec(FetchBlockInstNum, Bool())
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

  private val data = io.req.bits.cacheData.asTypeOf(Vec(FetchBlockInstNum, UInt((instBytes * 8).W)))
  private val rawInsts = VecInit((0 until FetchBlockInstNum).map(i =>
    if (i == FetchBlockInstNum - 1) data(i) else Cat(data(i + 1), data(i))
  ))

  // if the former fetch block's last 2 Bytes is a valid end, we need delimitation from data(0)
  //   we compute the first half directly -> bound(0, FetchBlockInstNum/2-1) is correct
  private val bound = WireInit(VecInit(Seq.fill(FetchBlockInstNum)(0.U.asTypeOf(new BoundInfo))))
  //   and compute two cases of the second half in parallel, we can choose later and reduce latency
  //     - case 1: data(FetchBlockInstNum/2-1) is a valid end, so data(FetchBlockInstNum/2) is a valid start
  //               -> readBound(FetchBlockInstNum/2, FetchBlockInstNum-1) is correct
  //     - case 2: data(FetchBlockInstNum/2) is a valid end, so data(FetchBlockInstNum/2+1) is a valid start
  //               -> readBoundPlus1(FetchBlockInstNum/2, FetchBlockInstNum-1) is correct
  //
  //     compute directly: 0 -> 1 -> ... -> 32 => bound(0, FetchBlockInstNum-1)
  //
  //     ours:        first half 0  -> 1  -> ... -> 16 ->  |  => bound(0, FetchBlockInstNum/2-1)
  //                                                       v
  //           second half case1 17 -> 18 -> ... -> 32 -> Mux => bound(FetchBlockInstNum/2, FetchBlockInstNum-1)
  //           second half case2 17 -> 18 -> ... -> 32 --/
  //
  //   NOTE: we use (FetchBlockInstNum/2, FetchBlockInstNum-1) only, but we still use Vec[FetchBlockInstNum] to simplify
  private val rearBound      = WireInit(VecInit(Seq.fill(FetchBlockInstNum)(0.U.asTypeOf(new BoundInfo))))
  private val rearBoundPlus1 = WireInit(VecInit(Seq.fill(FetchBlockInstNum)(0.U.asTypeOf(new BoundInfo))))

  private val currentIsRvc     = VecInit(rawInsts.map(isRVC))
  private val realCurrentIsRvc = Wire(Vec(FetchBlockInstNum, Bool()))
  realCurrentIsRvc    := currentIsRvc
  realCurrentIsRvc(0) := Mux(io.req.bits.prevLastIsHalfRvi, false.B, currentIsRvc(0))
  def genBound(
      bound:        Vec[BoundInfo],
      start:        Int,
      end:          Int,
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
        bound(0).isEnd   := Mux(preIsHalfRvi, true.B, checkThisIsEnd(bound(0).isStart, realCurrentIsRvc(0)))
      } else {
        bound(i).isStart := checkThisIsStart(if (i == start) true.B else bound(i - 1).isEnd)
        bound(i).isEnd   := checkThisIsEnd(bound(i).isStart, realCurrentIsRvc(i))
      }
    }
  }

  genBound(bound, 0, FetchBlockInstNum / 2, io.req.bits.prevLastIsHalfRvi)
  genBound(rearBound, FetchBlockInstNum / 2, FetchBlockInstNum, false.B)
  genBound(rearBoundPlus1, FetchBlockInstNum / 2 + 1, FetchBlockInstNum, false.B)

  // for xxxPlus1, FetchBlockInstNum / 2 must be a valid end, since we assume FetchBlockInstNum / 2 + 1 is a valid start
  // and, it must not be a valid start, otherwise, FetchBlockInstNum/2-1 is a valid end and rearBound should be selected
  rearBoundPlus1(FetchBlockInstNum / 2).isStart := false.B
  rearBoundPlus1(FetchBlockInstNum / 2).isEnd   := true.B

  // if FetchBlockInstNum / 2 - 1 is a valid end, FetchBlockInstNum / 2 is a valid start, then rearBound is correct
  // otherwise, rearBoundPlus1 is correct
  private val rearBoundCorrect = bound(FetchBlockInstNum / 2 - 1).isEnd

  for (i <- FetchBlockInstNum / 2 until FetchBlockInstNum) {
    bound(i) := Mux(rearBoundCorrect, rearBound(i), rearBoundPlus1(i))
  }

  // we also compute the whole block directly for differential testing, this will be optimized out in released code
  private val boundDiff = WireInit(VecInit(Seq.fill(FetchBlockInstNum)(0.U.asTypeOf(new BoundInfo))))

  genBound(boundDiff, 0, FetchBlockInstNum, io.req.bits.prevLastIsHalfRvi)

  private val startMismatch = Wire(Bool())
  private val endMismatch   = Wire(Bool())

  startMismatch := (bound zip boundDiff).map { case (a, b) => a.isStart =/= b.isStart }.reduce(_ || _)
  endMismatch   := (bound zip boundDiff).map { case (a, b) => a.isEnd =/= b.isEnd }.reduce(_ || _)

  XSError(io.req.valid && startMismatch, p"start mismatch\n")
  XSError(io.req.valid && endMismatch, p"end mismatch\n")

  for (i <- 0 until FetchBlockInstNum) {
    io.resp.bits.instrValid(i) := bound(i).isStart && io.req.bits.instrRange(i)
  }
  io.resp.valid := io.req.valid
  io.resp.bits.isRvc := VecInit(io.resp.bits.instrValid.zip(realCurrentIsRvc).map { case (valid, rvc) =>
    valid & rvc
  })
  io.resp.bits.isFirstLastHalfRvi := io.resp.bits.instrValid(io.req.bits.firstEndPos) &&
    !realCurrentIsRvc(io.req.bits.firstEndPos) && !((io.req.bits.firstEndPos === 0.U) && io.req.bits.prevLastIsHalfRvi)
  io.resp.bits.isLastHalfRvi := io.resp.bits.instrValid(io.req.bits.endPos) &&
    !realCurrentIsRvc(io.req.bits.endPos) && !((io.req.bits.endPos === 0.U) && io.req.bits.prevLastIsHalfRvi)
}
