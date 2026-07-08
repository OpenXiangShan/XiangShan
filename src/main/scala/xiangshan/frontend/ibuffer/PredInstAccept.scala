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

package xiangshan.frontend.ibuffer

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.vector.Bundles.Vstart
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.backend.vector.Decoder.{NumUopOH, UopBufferCtrlDecoder}
import xiangshan.backend.vector.Decoder.Types.UopBufferNum
import xiangshan.backend.vector.HasVectorSettings

class PredInstAccept(implicit p: Parameters) extends IBufferModule with HasVectorSettings {
  class PredInstAcceptIO extends Bundle {
    val outputEntries = Input(Vec(DecodeWidth, Valid(new IBufOutEntry)))
    val decodeAccept  = Input(Bool())
    val flush         = Input(Bool())
    val predUopBufferNum  = Output(UopBufferNum())
    val predUopNumOH      = Output(Vec(DecodeWidth, NumUopOH()))
    val predAccNum        = Output(UInt(DecodeWidth.U.getWidth.W))
  }

  val io = IO(new PredInstAcceptIO)

  val predUopNumOH = Wire(Vec(DecodeWidth, NumUopOH()))
  for (i <- 0 until DecodeWidth) {
    predUopNumOH(i) := Mux(io.outputEntries(i).valid, io.outputEntries(i).bits.uopNumOH, NumUopOH.N0)
  }
  io.predUopNumOH := predUopNumOH

  val uopBufferNumNext = Wire(UInt(uopBufferLength.U.getWidth.W))
  val uopBufferNum = RegInit(0.U(uopBufferLength.U.getWidth.W))
  when (io.flush || io.decodeAccept &&
    (io.outputEntries.head.valid || uopBufferNum =/= 0.U)) {
    uopBufferNum := uopBufferNumNext
  }
  io.predUopBufferNum := uopBufferNum

  val uopBufferCtrlDecoder = Module(new UopBufferCtrlDecoder(
    mopWidth = DecodeWidth,
    uopWidth = DecodeWidth,
    uopBufferLength = uopBufferLength,
    numM2M4M8Channel = (DecodeWidth, DecodeWidth, DecodeWidth),
  ))

  uopBufferCtrlDecoder.in.uopBufferNum := uopBufferNum
  uopBufferCtrlDecoder.in.channelUopNum := predUopNumOH
  io.predAccNum := uopBufferCtrlDecoder.out.accNum

  uopBufferNumNext := Mux(io.flush, 0.U, uopBufferCtrlDecoder.out.uopBufferNum)
}
