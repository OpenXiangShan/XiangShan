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

class PreDecode(implicit p: Parameters) extends IfuModule with PreDecodeHelper {
  class PreDecodeIO(implicit p: Parameters) extends IfuBundle {
    class PreDecodeReq(implicit p: Parameters) extends IfuBundle {
      val data: Vec[UInt] =
        if (HasCExtension) Vec(PredictWidth + 1, UInt(16.W)) else Vec(PredictWidth, UInt(32.W))
      val pc: Vec[PrunedAddr] = Vec(PredictWidth, PrunedAddr(VAddrBits))
    }
    class PreDecodeResp(implicit p: Parameters) extends IfuBundle {
      val pd:         Vec[PreDecodeInfo] = Vec(PredictWidth, new PreDecodeInfo)
      val altValid:   Vec[Bool]          = Vec(PredictWidth, Bool())
      val instr:      Vec[UInt]          = Vec(PredictWidth, UInt(32.W))
      val jumpOffset: Vec[PrunedAddr]    = Vec(PredictWidth, PrunedAddr(VAddrBits))
    }

    val req:  Valid[PreDecodeReq] = Flipped(ValidIO(new PreDecodeReq))
    val resp: PreDecodeResp       = Output(new PreDecodeResp)
  }
  val io: PreDecodeIO = IO(new PreDecodeIO)

  // data is (16+1) * 2B raw instruction data if HasCExtension, otherwise 8 * 4B raw instruction data
  private val data = io.req.bits.data
  private val rawInsts =
    if (HasCExtension) VecInit((0 until PredictWidth).map(i => Cat(data(i + 1), data(i))))
    else VecInit((0 until PredictWidth).map(i => data(i)))

  private class BoundInfo extends Bundle {
    val isStart: Bool = Bool()
    val isEnd:   Bool = Bool()
  }

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

  // otherwise, we need to delimitation from data(1), we provide as alternative bound, Ifu will choose one in s3 stage
  //   similarly, we compute first half and two cases of second half in parallel to reduce latency
  private val altBound          = WireInit(VecInit(Seq.fill(PredictWidth)(0.U.asTypeOf(new BoundInfo))))
  private val altRearBound      = WireInit(VecInit(Seq.fill(PredictWidth)(0.U.asTypeOf(new BoundInfo))))
  private val altRearBoundPlus1 = WireInit(VecInit(Seq.fill(PredictWidth)(0.U.asTypeOf(new BoundInfo))))

  private val currentIsRVC = VecInit(rawInsts.map(isRVC))

  for (i <- 0 until PredictWidth) {
    val inst = WireInit(rawInsts(i))

    val (brType, isCall, isRet) = getBrInfo(inst)
    val jalOffset               = getJalOffset(inst, currentIsRVC(i))
    val brOffset                = getBrOffset(inst, currentIsRVC(i))

    io.resp.altValid(i) := altBound(i).isStart

    io.resp.pd(i).valid := bound(i).isStart
    io.resp.pd(i).isRVC := currentIsRVC(i)

    // for diff purpose only
    io.resp.pd(i).brType := brType
    io.resp.pd(i).isCall := isCall
    io.resp.pd(i).isRet  := isRet

    io.resp.instr(i)      := inst
    io.resp.jumpOffset(i) := Mux(io.resp.pd(i).isBr, brOffset, jalOffset)
  }

  def genBound(bound: Vec[BoundInfo], start: Int, end: Int, isAlt: Boolean = false): Unit = {
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

    // assume altBound is not start from a valid start, i.e. altBound(-1) is not a valid end
    // assume other cases are start from a valid start, i.e.
    //   - bound(-1) is a valid end
    //   - rearBound(PredictWidth / 2 - 1) is a valid end
    //   - rearBoundPlus1(PredictWidth / 2) is a valid end
    //   - altRearBound(PredictWidth / 2 - 1) is a valid end
    //   - altRearBoundPlus1(PredictWidth / 2) is a valid end
    val startFromValid = !(start == 0 && isAlt)
    for (i <- start until end) {
      bound(i).isStart := checkThisIsStart(if (i == start) startFromValid.B else bound(i - 1).isEnd)
      bound(i).isEnd   := checkThisIsEnd(bound(i).isStart, currentIsRVC(i))
    }
  }

  genBound(bound, 0, PredictWidth / 2)
  genBound(rearBound, PredictWidth / 2, PredictWidth)
  genBound(rearBoundPlus1, PredictWidth / 2 + 1, PredictWidth)

  genBound(altBound, 0, PredictWidth / 2, isAlt = true)
  genBound(altRearBound, PredictWidth / 2, PredictWidth, isAlt = true)
  genBound(altRearBoundPlus1, PredictWidth / 2 + 1, PredictWidth, isAlt = true)

  // for xxxPlus1, PredictWidth / 2 must be a valid end, since we assume PredictWidth / 2 + 1 is a valid start
  // and, it must not be a valid start, otherwise, PredictWidth / 2 - 1 is a valid end and rearBound should be selected
  rearBoundPlus1(PredictWidth / 2).isStart    := false.B
  rearBoundPlus1(PredictWidth / 2).isEnd      := true.B
  altRearBoundPlus1(PredictWidth / 2).isStart := false.B
  altRearBoundPlus1(PredictWidth / 2).isEnd   := true.B

  // if PredictWidth / 2 - 1 is a valid end, PredictWidth / 2 is a valid start, then rearBound is correct
  // otherwise, rearBoundPlus1 is correct
  private val rearBoundCorrect    = bound(PredictWidth / 2 - 1).isEnd
  private val altRearBoundCorrect = altBound(PredictWidth / 2 - 1).isEnd
  for (i <- PredictWidth / 2 until PredictWidth) {
    bound(i)    := Mux(rearBoundCorrect, rearBound(i), rearBoundPlus1(i))
    altBound(i) := Mux(altRearBoundCorrect, altRearBound(i), altRearBoundPlus1(i))
  }

  // we also compute the whole block directly for differential testing, this will be optimized out in released code
  private val boundDiff    = WireInit(VecInit(Seq.fill(PredictWidth)(0.U.asTypeOf(new BoundInfo))))
  private val altBoundDiff = WireInit(VecInit(Seq.fill(PredictWidth)(0.U.asTypeOf(new BoundInfo))))

  genBound(boundDiff, 0, PredictWidth)
  genBound(altBoundDiff, 0, PredictWidth, isAlt = true)

  private val startMismatch    = Wire(Bool())
  private val endMismatch      = Wire(Bool())
  private val altStartMismatch = Wire(Bool())
  private val altEndMismatch   = Wire(Bool())

  startMismatch    := (bound zip boundDiff).map { case (a, b) => a.isStart =/= b.isStart }.reduce(_ || _)
  endMismatch      := (bound zip boundDiff).map { case (a, b) => a.isEnd =/= b.isEnd }.reduce(_ || _)
  altStartMismatch := (altBound zip altBoundDiff).map { case (a, b) => a.isStart =/= b.isStart }.reduce(_ || _)
  altEndMismatch   := (altBound zip altBoundDiff).map { case (a, b) => a.isEnd =/= b.isEnd }.reduce(_ || _)

  XSError(io.req.valid && startMismatch, p"start mismatch\n")
  XSError(io.req.valid && endMismatch, p"end mismatch\n")
  XSError(io.req.valid && altStartMismatch, p"altStart mismatch\n")
  XSError(io.req.valid && altEndMismatch, p"altEnd mismatch\n")

  for (i <- 0 until PredictWidth) {
    XSDebug(
      true.B,
      p"instr ${Hexadecimal(io.resp.instr(i))}, " +
        p"isStart ${Binary(bound(i).isStart)}, " +
        p"isEnd ${Binary(bound(i).isEnd)}, " +
        p"isRVC ${Binary(io.resp.pd(i).isRVC)}, " +
        p"brType ${Binary(io.resp.pd(i).brType)}, " +
        p"isRet ${Binary(io.resp.pd(i).isRet)}, " +
        p"isCall ${Binary(io.resp.pd(i).isCall)}\n"
    )
  }
}
