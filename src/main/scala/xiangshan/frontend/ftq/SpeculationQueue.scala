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

package xiangshan.frontend.ftq

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.SyncDataModuleTemplate
import xiangshan.frontend.bpu.BpuRedirectMeta

class SpeculationQueue(implicit p: Parameters) extends FtqModule {

  class SpeculationQueueIO extends FtqBundle {
    val ren:   Bool            = Input(Bool())
    val raddr: UInt            = Input(UInt(log2Up(FtqSize).W))
    val rdata: BpuRedirectMeta = Output(new BpuRedirectMeta)

    val wen:   Bool            = Input(Bool())
    val waddr: UInt            = Input(UInt(log2Up(FtqSize).W))
    val wdata: BpuRedirectMeta = Input(new BpuRedirectMeta)
  }

  val io: SpeculationQueueIO = IO(new SpeculationQueueIO)

  private val mem = Module(new SyncDataModuleTemplate(
    gen = new BpuRedirectMeta,
    numEntries = FtqSize,
    numRead = 1,
    numWrite = 1,
    hasRen = true
  ))

  mem.io.ren.get(0) := io.ren
  mem.io.raddr(0)   := io.raddr
  io.rdata          := mem.io.rdata(0)

  mem.io.wen(0)   := io.wen
  mem.io.waddr(0) := io.waddr
  mem.io.wdata(0) := io.wdata
}
