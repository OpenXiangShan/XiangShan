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

package xiangshan.frontend.icache.array

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.icache.DataReadBundle
import xiangshan.frontend.icache.DataWriteBundle
import xiangshan.frontend.icache.ICacheBundle
import xiangshan.frontend.icache.ICacheDataEntry
import xiangshan.frontend.icache.ICacheDataHelper
import xiangshan.frontend.icache.ICacheModule

class ICacheDataArray(implicit p: Parameters) extends ICacheModule with ICacheDataHelper {
  class ICacheDataArrayIO(implicit p: Parameters) extends ICacheBundle {
    val write: DataWriteBundle     = Flipped(new DataWriteBundle)
    val read:  Vec[DataReadBundle] = Vec(FetchPorts, Flipped(new DataReadBundle))
  }

  val io: ICacheDataArrayIO = IO(new ICacheDataArrayIO)

  // sanity check
  require(DataSramWidth == (new ICacheDataEntry).getWidth)

  private val banks = Seq.tabulate(DataBanks)(i => Module(new ICacheDataBank(i)))

  /* *** read *** */
  io.read.zipWithIndex.foreach { case (port, portIdx) =>
    val r0_valid   = port.req.valid
    val r0_setIdx  = port.req.bits.vSetIdx
    val r0_bankSel = getBankSel(port.req.bits.blkOffset, port.req.bits.blkEndOffset, port.req.bits.isDoubleLine)
    val r0_lineSel = getLineSel(port.req.bits.blkOffset)
    val r0_waymask = port.req.bits.waymask

    port.req.ready := banks.map(_.io.read(portIdx).req.ready).reduce(_ || _)

    banks.zipWithIndex.foreach { case (bank, bankIdx) =>
      bank.io.read(portIdx).req.valid        := r0_valid && r0_bankSel(r0_lineSel(bankIdx))(bankIdx)
      bank.io.read(portIdx).req.bits.setIdx  := r0_setIdx(r0_lineSel(bankIdx))
      bank.io.read(portIdx).req.bits.waymask := r0_waymask(r0_lineSel(bankIdx))
    }

    port.resp.datas := banks.map(_.io.read(portIdx).resp.entry.data)
    port.resp.codes := banks.map(_.io.read(portIdx).resp.entry.code)
  }

  // TEST: force ECC to fail by setting parity codes to 0
  if (ForceDataEccFail) {
    io.read.foreach(_.resp.codes.foreach(_ := 0.U))
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
