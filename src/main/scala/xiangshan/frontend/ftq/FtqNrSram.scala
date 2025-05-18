// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at: https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.ftq

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.sram.SplittedSRAMTemplate

class FtqNrSram[T <: Data](gen: T, numRead: Int)(implicit p: Parameters) extends FtqModule {

  val io = IO(new Bundle() {
    val raddr = Input(Vec(numRead, UInt(log2Up(FtqSize).W)))
    val ren   = Input(Vec(numRead, Bool()))
    val rdata = Output(Vec(numRead, gen))
    val waddr = Input(UInt(log2Up(FtqSize).W))
    val wen   = Input(Bool())
    val wdata = Input(gen)
  })

  for (i <- 0 until numRead) {
    val sram = Module(new SplittedSRAMTemplate(
      gen,
      set = FtqSize,
      way = 1,
      dataSplit = 2,
      singlePort = false,
      withClockGate = true,
      hasMbist = hasMbist
    ))
    sram.io.r.req.valid       := io.ren(i)
    sram.io.r.req.bits.setIdx := io.raddr(i)
    io.rdata(i)               := sram.io.r.resp.data(0)
    sram.io.w.req.valid       := io.wen
    sram.io.w.req.bits.setIdx := io.waddr
    sram.io.w.req.bits.data   := VecInit(io.wdata)
  }
}
