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

package xiangshan.frontend.icache

import chisel3._
import org.chipsalliance.cde.config.Parameters

class ICacheDataArray(implicit p: Parameters) extends ICacheModule with ICacheDataHelper {
  class ICacheDataArrayIO(implicit p: Parameters) extends ICacheBundle {
    val write: DataWriteBundle = Flipped(new DataWriteBundle)
    val read:  DataReadBundle  = Flipped(new DataReadBundle)
  }

  val io: ICacheDataArrayIO = IO(new ICacheDataArrayIO)

  // sanity check
  require(DataSramWidth == (new ICacheDataEntry).getWidth)

  private val banks = Seq.tabulate(DataBanks)(i => Module(new ICacheDataBank(i)))

  /* *** read *** */
  private val r0_valid  = io.read.req.valid
  private val r0_setIdx = io.read.req.bits.vSetIdx
  private val r0_bankSel =
    getBankSel(io.read.req.bits.blkOffset, io.read.req.bits.blkEndOffset, io.read.req.bits.isDoubleLine)
  private val r0_lineSel = getLineSel(io.read.req.bits.blkOffset)
  private val r0_waymask = io.read.req.bits.waymask

  io.read.req.ready := banks.map(_.io.read.req.ready).reduce(_ || _)
  banks.zipWithIndex.foreach { case (b, i) =>
    b.io.read.req.valid        := r0_valid && r0_bankSel(r0_lineSel(i))(i)
    b.io.read.req.bits.setIdx  := r0_setIdx(r0_lineSel(i))
    b.io.read.req.bits.waymask := r0_waymask(r0_lineSel(i))
  }

  io.read.resp.datas := banks.map(_.io.read.resp.entry.data)
  io.read.resp.codes := banks.map(_.io.read.resp.entry.code)

  // TEST: force ECC to fail by setting parity codes to 0
  if (ForceDataEccFail) {
    io.read.resp.codes.foreach(_ := 0.U)
  }

  /* *** write *** */
  private val w0_valid   = io.write.req.valid
  private val w0_setIdx  = io.write.req.bits.vSetIdx
  private val w0_waymask = io.write.req.bits.waymask
  private val w0_entries = io.write.req.bits.entries

  io.write.req.ready := banks.map(_.io.write.req.ready).reduce(_ && _)
  banks.zipWithIndex.foreach { case (b, i) =>
    b.io.write.req.valid        := w0_valid
    b.io.write.req.bits.setIdx  := w0_setIdx
    b.io.write.req.bits.waymask := w0_waymask
    b.io.write.req.bits.entry   := w0_entries(i)
  }
}
