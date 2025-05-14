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
import chisel3.util.log2Ceil
import org.chipsalliance.cde.config.Parameters
import utility.SyncDataModuleTemplate

class FtqPcMemWrapper(numOtherReads: Int)(implicit p: Parameters) extends FtqModule {
  val io = IO(new Bundle {
    val ifuPtr_w           = Input(new FtqPtr)
    val ifuPtrPlus1_w      = Input(new FtqPtr)
    val ifuPtrPlus2_w      = Input(new FtqPtr)
    val pfPtr_w            = Input(new FtqPtr)
    val pfPtrPlus1_w       = Input(new FtqPtr)
    val commPtr_w          = Input(new FtqPtr)
    val commPtrPlus1_w     = Input(new FtqPtr)
    val ifuPtr_rdata       = Output(new FtqRfComponents)
    val ifuPtrPlus1_rdata  = Output(new FtqRfComponents)
    val ifuPtrPlus2_rdata  = Output(new FtqRfComponents)
    val pfPtr_rdata        = Output(new FtqRfComponents)
    val pfPtrPlus1_rdata   = Output(new FtqRfComponents)
    val commPtr_rdata      = Output(new FtqRfComponents)
    val commPtrPlus1_rdata = Output(new FtqRfComponents)

    val wen   = Input(Bool())
    val waddr = Input(UInt(log2Ceil(FtqSize).W))
    val wdata = Input(new FtqRfComponents)
  })

  val num_pc_read = numOtherReads + 5
  val mem         = Module(new SyncDataModuleTemplate(new FtqRfComponents, FtqSize, num_pc_read, 1, "FtqPC"))
  mem.io.wen(0)   := io.wen
  mem.io.waddr(0) := io.waddr
  mem.io.wdata(0) := io.wdata

  // read one cycle ahead for ftq local reads
  val raddr_vec = VecInit(Seq(
    io.ifuPtr_w.value,
    io.ifuPtrPlus1_w.value,
    io.ifuPtrPlus2_w.value,
    io.pfPtr_w.value,
    io.pfPtrPlus1_w.value,
    io.commPtrPlus1_w.value,
    io.commPtr_w.value
  ))

  mem.io.raddr := raddr_vec

  io.ifuPtr_rdata       := mem.io.rdata.dropRight(6).last
  io.ifuPtrPlus1_rdata  := mem.io.rdata.dropRight(5).last
  io.ifuPtrPlus2_rdata  := mem.io.rdata.dropRight(4).last
  io.pfPtr_rdata        := mem.io.rdata.dropRight(3).last
  io.pfPtrPlus1_rdata   := mem.io.rdata.dropRight(2).last
  io.commPtrPlus1_rdata := mem.io.rdata.dropRight(1).last
  io.commPtr_rdata      := mem.io.rdata.last
}
