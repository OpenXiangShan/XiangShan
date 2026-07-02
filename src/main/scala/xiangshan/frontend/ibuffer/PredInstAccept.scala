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
import xiangshan.backend.decode.isa.Extensions._
import xiangshan.backend.fu.vector.Bundles.Vstart
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.backend.vector.Decoder.{DecodeChannelsCore, NumUopOH, UopBufferCtrlDecoder}
import xiangshan.backend.vector.Decoder.Types.UopBufferNum
import xiangshan.backend.vector.HasVectorSettings

class PredInstAccept(implicit p: Parameters) extends IBufferModule with HasVectorSettings {
  override def VLEN: Int = 128

  class PredInstAcceptIO extends Bundle {
    val outputEntries = Input(Vec(DecodeWidth, Valid(new IBufOutEntry)))
    val fromCSR       = Input(new CSRToDecode)
    val vstart        = Input(Vstart())
    val decodeAccept  = Input(Bool())
    val flush         = Input(Bool())
    val predUopBufferNum  = Output(UopBufferNum())
    val predUopNumOH      = Output(Vec(DecodeWidth, NumUopOH()))
    val predAccNum        = Output(UInt(DecodeWidth.U.getWidth.W))
  }

  val io = IO(new PredInstAcceptIO)

  val decodeCore = Module(new DecodeChannelsCore(
    mopWidth = DecodeWidth,
    uopWidth = DecodeWidth,
    numM2M4M8Channel = (DecodeWidth, DecodeWidth, DecodeWidth),
  ))

  val uopBufferNumNext = Wire(UInt(uopBufferLength.U.getWidth.W))
  val uopBufferNum = RegEnable(uopBufferNumNext, 0.U(uopBufferLength.U.getWidth.W), io.decodeAccept || io.flush)
  io.predUopBufferNum := uopBufferNum

  for (i <- 0 until DecodeWidth) {
    decodeCore.in.mops(i).valid := io.outputEntries(i).valid
    decodeCore.in.mops(i).bits.info.rawInst := io.outputEntries(i).bits.inst
    decodeCore.in.mops(i).bits.info.vtype   := io.outputEntries(i).bits.vtype
    decodeCore.in.mops(i).bits.info.fromCSR := io.fromCSR
    decodeCore.in.mops(i).bits.info.vstart  := io.vstart
    decodeCore.in.mops(i).bits.ctrl         := DontCare
  }

  val predUopNumOH = Wire(Vec(DecodeWidth, NumUopOH()))
  for (i <- 0 until DecodeWidth) {
    predUopNumOH(i) := Mux(
      !io.outputEntries(i).valid,
      NumUopOH.N0,
      Mux(
        decodeCore.out.illegalChannel(i).valid,
        NumUopOH.N1,
        Mux(
          decodeCore.out.psdChannel(i).valid,
          NumUopOH.N1,
          Mux1H(Seq(
            decodeCore.out.vecChannel(i)(0).valid -> decodeCore.out.vecUopNumOH(i),
            decodeCore.out.vsetChannel(i).valid -> NumUopOH.N1,
            decodeCore.out.simChannel(i)(0).valid -> decodeCore.out.simUopNumOH(i),
          )),
        ),
      ),
    )
  }
  io.predUopNumOH := predUopNumOH

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
