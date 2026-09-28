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
import xiangshan.backend.vector.Decoder.NumUopOH
import xiangshan.backend.vector.Decoder.Types.UopBufferNum
import xiangshan.backend.vector.Decoder.UopBufferCtrlDecoder
import xiangshan.backend.vector.HasVectorSettings

object PredInstAccept {
  class In(implicit p: Parameters) extends IBufferBundle {
    val flush         = Bool()
    val outputEntries = Vec(DecodeWidth, Valid(new IBufOutEntry))
    val decodeAccept  = Bool()
  }

  class Out(implicit p: Parameters) extends IBufferBundle {
    val predUopBufferNum = UopBufferNum()
    val predUopNumOH     = Vec(DecodeWidth, NumUopOH())
    val predAccNum       = UInt(DecodeWidth.U.getWidth.W)
  }
}

class PredInstAccept(implicit p: Parameters) extends IBufferModule with HasVectorSettings {

  val in  = IO(Input(new PredInstAccept.In))
  val out = IO(Output(new PredInstAccept.Out))

  val uopBufferCtrlDecoder = Module(new UopBufferCtrlDecoder(
    mopWidth = DecodeWidth,
    uopWidth = RenameWidth,
    uopBufferLength = uopBufferLength,
    numM2M4M8Channel = (DecodeWidth, DecodeWidth, DecodeWidth)
  ))

  val uopBufferNum     = RegInit(0.U(uopBufferLength.U.getWidth.W))
  val uopBufferNumNext = Wire(UInt(uopBufferLength.U.getWidth.W))

  val predUopNumOH = Wire(Vec(DecodeWidth, NumUopOH()))
  for (i <- 0 until DecodeWidth) {
    predUopNumOH(i) := Mux(in.outputEntries(i).valid, in.outputEntries(i).bits.uopNumOH, NumUopOH.N0)
  }

  when(
    in.flush ||
      in.decodeAccept && (in.outputEntries.head.valid || uopBufferNum =/= 0.U)
  ) {
    uopBufferNum := uopBufferNumNext
  }

  uopBufferCtrlDecoder.in.uopBufferNum  := uopBufferNum
  uopBufferCtrlDecoder.in.channelUopNum := predUopNumOH

  uopBufferNumNext := Mux(in.flush, 0.U, uopBufferCtrlDecoder.out.uopBufferNum)

  out.predUopNumOH     := predUopNumOH
  out.predUopBufferNum := uopBufferNum
  out.predAccNum       := uopBufferCtrlDecoder.out.accNum
}
