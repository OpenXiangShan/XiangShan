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
import xiangshan.frontend.FTBEntry_FtqMem

class FtbEntryQueue(implicit p: Parameters) extends FtqModule {

  class FtbEntryQueueIO extends FtqBundle {
    class ReadChannel extends FtqBundle {
      val ren:   Bool            = Input(Bool())
      val raddr: UInt            = Input(UInt(log2Ceil(FtqSize).W))
      val rdata: FTBEntry_FtqMem = Output(new FTBEntry_FtqMem)
    }

    val ifuWb           = new ReadChannel
    val backendRedirect = new ReadChannel

    val wen:   Bool            = Input(Bool())
    val waddr: UInt            = Input(UInt(log2Ceil(FtqSize).W))
    val wdata: FTBEntry_FtqMem = Input(new FTBEntry_FtqMem)
  }

  val io: FtbEntryQueueIO = IO(new FtbEntryQueueIO)

  private val readChannels = Seq(
    io.ifuWb,
    io.backendRedirect
  )

  private val mem = Module(new SyncDataModuleTemplate(
    gen = new FTBEntry_FtqMem,
    numEntries = FtqSize,
    numRead = 2,
    numWrite = 1,
    hasRen = true
  ))

  mem.io.ren.get := readChannels.map(_.ren)
  mem.io.raddr   := readChannels.map(_.raddr)
  readChannels.zipWithIndex.foreach { case (readChannel, i) => readChannel.rdata := mem.io.rdata(i) }

  mem.io.wen(0)   := io.wen
  mem.io.waddr(0) := io.waddr
  mem.io.wdata(0) := io.wdata
}
