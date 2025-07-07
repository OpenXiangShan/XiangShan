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
import xiangshan.frontend.PrunedAddr

class EntryQueue(implicit p: Parameters) extends FtqModule {

  class FtqEntryQueueIO extends FtqBundle {
    class ReadChannel extends FtqBundle {
      val ptr:   FtqPtr     = Input(new FtqPtr())
      val rdata: PrunedAddr = Output(new PrunedAddr(VAddrBits))
    }

    val pfPtr:     Vec[ReadChannel] = Vec(2, new ReadChannel)
    val ifuPtr:    Vec[ReadChannel] = Vec(3, new ReadChannel)
    val commitPtr: Vec[ReadChannel] = Vec(2, new ReadChannel)

    val wen:   Bool       = Input(Bool())
    val waddr: UInt       = Input(UInt(log2Ceil(FtqSize).W))
    val wdata: PrunedAddr = Input(new PrunedAddr(VAddrBits))
  }

  val io: FtqEntryQueueIO = IO(new FtqEntryQueueIO)

  private val readChannels = Seq.concat(
    io.pfPtr.map(ptr => ptr),
    io.ifuPtr.map(ptr => ptr),
    io.commitPtr.map(ptr => ptr)
  )
  private val readChannelNum = readChannels.size

  private val mem = Module(new SyncDataModuleTemplate(
    gen = PrunedAddr(VAddrBits),
    numEntries = FtqSize,
    numRead = readChannelNum,
    numWrite = 1
  ))

  mem.io.raddr := readChannels.map(_.ptr.value)
  readChannels.zipWithIndex.foreach { case (readChannel, i) =>
    readChannel.rdata := mem.io.rdata.dropRight(readChannelNum - i - 1).last
  }

  mem.io.wen(0)   := io.wen
  mem.io.waddr(0) := io.waddr
  mem.io.wdata(0) := io.wdata
}
