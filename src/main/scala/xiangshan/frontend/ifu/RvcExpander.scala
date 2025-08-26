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
import freechips.rocketchip.rocket.ExpandedInstruction
import freechips.rocketchip.rocket.RVCDecoder
import org.chipsalliance.cde.config.Parameters

class RvcExpander(implicit p: Parameters) extends IfuModule {
  class RVCExpanderIO(implicit p: Parameters) extends IfuBundle {
    val in:      UInt                = Input(UInt(32.W))
    val fsIsOff: Bool                = Input(Bool())
    val out:     ExpandedInstruction = Output(new ExpandedInstruction)
    val ill:     Bool                = Output(Bool())
  }
  val io: RVCExpanderIO = IO(new RVCExpanderIO)

  private val decoder = new RVCDecoder(io.in, io.fsIsOff, XLEN, fLen, useAddiForMv = true)

  if (HasCExtension) {
    io.out := decoder.decode
    io.ill := decoder.ill
  } else {
    io.out := decoder.passthrough
    io.ill := false.B
  }
}
