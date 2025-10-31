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

package xiangshan.frontend.bpu.mbtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class MainBtbAlignBank(
    alignIdx: Int
)(implicit p: Parameters) extends MainBtbModule with Helpers {
  class MainBtbAlignBankIO extends Bundle {
    class Read extends Bundle {
      class Req extends Bundle {
        val setIdx:           UInt = UInt(SetIdxLen.W)
        val internalBankMask: UInt = UInt(NumInternalBanks.W)
      }

      class Resp extends Bundle {
        val entries: Vec[MainBtbEntry] = Vec(NumWay, new MainBtbEntry)
      }

      val req:  Valid[Req] = Flipped(Valid(new Req))
      val resp: Resp       = Output(new Resp)
    }

    class Write extends Bundle {
      class Req extends Bundle {
        val setIdx:           UInt         = UInt(SetIdxLen.W)
        val internalBankMask: UInt         = UInt(NumInternalBanks.W)
        val wayMask:          UInt         = UInt(NumWay.W)
        val entry:            MainBtbEntry = new MainBtbEntry
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    class Flush extends Bundle {
      class Req extends Bundle {
        val setIdx:           UInt = UInt(SetIdxLen.W)
        val internalBankMask: UInt = UInt(NumInternalBanks.W)
        val wayMask:          UInt = UInt(NumWay.W)
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    val resetDone: Bool = Output(Bool())

    val read:  Read  = new Read
    val write: Write = new Write
    val flush: Flush = new Flush
  }

  val io: MainBtbAlignBankIO = IO(new MainBtbAlignBankIO)

  // alias
  private val r     = io.read
  private val w     = io.write
  private val flush = io.flush

  private val internalBanks = Seq.tabulate(NumInternalBanks) { bankIdx =>
    Module(new MainBtbInternalBank(alignIdx, bankIdx))
  }

  io.resetDone := internalBanks.map(_.io.resetDone).reduce(_ && _)

  // bank -> io
  internalBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.read.req.valid       := r.req.valid && r.req.bits.internalBankMask(i)
    b.io.read.req.bits.setIdx := r.req.bits.setIdx
  }
  r.resp.entries := Mux1H(
    RegEnable(r.req.bits.internalBankMask, r.req.valid),
    internalBanks.map(_.io.read.resp.entries)
  )

  // io -> bank
  internalBanks.zipWithIndex.foreach { case (b, i) =>
    b.io.write.req.valid        := w.req.valid && w.req.bits.internalBankMask(i)
    b.io.write.req.bits.setIdx  := w.req.bits.setIdx
    b.io.write.req.bits.wayMask := w.req.bits.wayMask
    b.io.write.req.bits.entry   := w.req.bits.entry

    b.io.flush.req.valid        := flush.req.valid && flush.req.bits.internalBankMask(i)
    b.io.flush.req.bits.setIdx  := flush.req.bits.setIdx
    b.io.flush.req.bits.wayMask := flush.req.bits.wayMask
  }
}
