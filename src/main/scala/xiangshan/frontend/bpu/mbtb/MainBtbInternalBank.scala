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
import utility.sram.SRAMTemplate
import xiangshan.frontend.bpu.WriteBuffer

class MainBtbInternalBank(
    alignIdx: Int,
    bankIdx:  Int
)(implicit p: Parameters) extends MainBtbModule with Helpers {
  class MainBtbInternalBankIO extends Bundle {
    class Read extends Bundle {
      class Req extends Bundle {
        val setIdx: UInt = UInt(SetIdxLen.W)
      }
      class Resp extends Bundle {
        val entries: Vec[MainBtbEntry] = Vec(NumWay, new MainBtbEntry)
      }

      val req:  Valid[Req] = Flipped(Valid(new Req))
      val resp: Resp       = Output(new Resp)
    }

    class Write extends Bundle {
      class Req extends Bundle {
        val setIdx:  UInt         = UInt(SetIdxLen.W)
        val wayMask: UInt         = UInt(NumWay.W)
        val entry:   MainBtbEntry = new MainBtbEntry
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    // flush interface for multi-hit
    class Flush extends Bundle {
      class Req extends Bundle {
        val setIdx:  UInt = UInt(SetIdxLen.W)
        val wayMask: UInt = UInt(NumWay.W)
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    val resetDone: Bool = Output(Bool())

    val read:  Read  = new Read
    val write: Write = new Write
    val flush: Flush = new Flush
  }

  val io: MainBtbInternalBankIO = IO(new MainBtbInternalBankIO)

  // alias
  private val r     = io.read
  private val w     = io.write
  private val flush = io.flush

  private val ways = Seq.tabulate(NumWay) { wayIdx =>
    Module(
      new SRAMTemplate(
        new MainBtbEntry,
        set = NumSets,
        way = 1, // Not using way in the template, preparing for future skewed assoc
        singlePort = true,
        shouldReset = true,
        holdRead = true,
        withClockGate = true,
        hasMbist = hasMbist,
        hasSramCtl = hasSramCtl
      )
    ).suggestName(s"mbtb_sram_align${alignIdx}_bank${bankIdx}_way${wayIdx}")
  }

  private val writeBuffer = Module(new WriteBuffer(new MainBtbSramWriteReq, WriteBufferSize, NumWay))

  private val resetDone = RegInit(false.B)
  when(ways.map(_.io.r.req.ready).reduce(_ && _)) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  // sram -> io
  ways.foreach { way =>
    way.io.r.req.valid       := r.req.valid
    way.io.r.req.bits.setIdx := r.req.bits.setIdx
  }
  // magic (0): each sram template has 1 way, so we only read data(0)
  r.resp.entries := VecInit(ways.map(_.io.r.resp.data(0)))

  // writeBuffer -> sram
  (ways zip writeBuffer.io.read).foreach { case (way, bufRead) =>
    way.io.w.req.valid        := bufRead.valid && !way.io.r.req.valid
    way.io.w.req.bits.data(0) := bufRead.bits.entry
    way.io.w.req.bits.setIdx  := bufRead.bits.setIdx
    bufRead.ready             := way.io.w.req.ready && !way.io.r.req.valid
  }

  // io -> writeBuffer
  writeBuffer.io.write.zipWithIndex.foreach { case (bufWrite, i) =>
    val writeValid = w.req.valid && w.req.bits.wayMask(i)
    val flushValid = flush.req.valid && flush.req.bits.wayMask(i)
    bufWrite.valid := writeValid || flushValid
    bufWrite.bits.setIdx := Mux(
      writeValid,
      w.req.bits.setIdx,
      flush.req.bits.setIdx
    )
    bufWrite.bits.entry := Mux(
      writeValid,
      w.req.bits.entry,
      0.U.asTypeOf(new MainBtbEntry)
    )
  }
}
