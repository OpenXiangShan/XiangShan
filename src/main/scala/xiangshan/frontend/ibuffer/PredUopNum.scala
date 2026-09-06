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
import xiangshan._
import xiangshan.backend.fu.vector.Bundles.Vstart
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.backend.vector.Decoder.DecodeChannelsCore
import xiangshan.backend.vector.Decoder.NumUopOH
import xiangshan.backend.vector.HasVectorSettings

object PredUopNum {
  class In(implicit p: Parameters) extends IBufferBundle {
    val valid   = Vec(DecodeWidth, Bool())
    val inst    = Vec(DecodeWidth, UInt(32.W))
    val vtype   = Vec(DecodeWidth, VType())
    val fromCSR = new CSRToDecode
    val vstart  = Vstart()
  }

  class Out(implicit p: Parameters) extends IBufferBundle {
    val uopNumOH = Vec(DecodeWidth, NumUopOH())
  }
}

class PredUopNum(implicit p: Parameters) extends IBufferModule with HasVectorSettings {

  val in  = IO(Input(new PredUopNum.In))
  val out = IO(Output(new PredUopNum.Out))

  val decodeCore = Module(new DecodeChannelsCore(
    mopWidth = DecodeWidth,
    uopWidth = RenameWidth,
    numM2M4M8Channel = (DecodeWidth, DecodeWidth, DecodeWidth)
  ))

  for (i <- 0 until DecodeWidth) {
    decodeCore.in.mops(i).valid             := in.valid(i)
    decodeCore.in.mops(i).bits.info.rawInst := in.inst(i)
    decodeCore.in.mops(i).bits.info.vtype   := in.vtype(i)
    decodeCore.in.mops(i).bits.info.fromCSR := in.fromCSR
    decodeCore.in.mops(i).bits.info.vstart  := in.vstart
  }

  for (i <- 0 until DecodeWidth) {
    out.uopNumOH(i) := MuxCase(
      NumUopOH.N1,
      Seq(
        !in.valid(i)                                                                   -> NumUopOH.N0,
        (decodeCore.out.simChannel(i)(1).valid && !decodeCore.out.psdChannel(i).valid) -> NumUopOH.N2,
        decodeCore.out.vecChannel(i)(0).valid                                          -> decodeCore.out.vecUopNumOH(i)
      )
    )
  }
}
