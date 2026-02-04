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
import utility.XSError
import utility.XSPerfAccumulate
import utility.sram.SRAMTemplate
import xiangshan.frontend.bpu.SaturateCounter
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
        val entries: Vec[MainBtbEntry]      = Vec(NumWay, new MainBtbEntry)
        val shareds: Vec[MainBtbSharedInfo] = Vec(NumWay, new MainBtbSharedInfo)
      }

      val req:  Valid[Req] = Flipped(Valid(new Req))
      val resp: Resp       = Output(new Resp)
    }

    class Write extends Bundle {
      class Req extends Bundle {
        val setIdx:  UInt                    = UInt(SetIdxLen.W)
        val wayMask: UInt                    = UInt(NumWay.W)
        val entries: Vec[MainBtbEntry]       = Vec(NumWay, new MainBtbEntry)
        val shareds: Vec[MainBtbSharedInfo]  = Vec(NumWay, new MainBtbSharedInfo)
        val status:  Vec[MainBtbWriteStatus] = Vec(NumWay, new MainBtbWriteStatus)
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

    // probe write buffer for prediction
    class Probe extends Bundle {
      val buffers: Vec[MainBtbWriteProbe] = Output(Vec(NumWay, new MainBtbWriteProbe))
    }

    val resetDone: Bool = Output(Bool())

    val read:  Read  = new Read
    val write: Write = new Write
    val flush: Flush = new Flush
    val probe: Probe = new Probe
  }

  val io: MainBtbInternalBankIO = IO(new MainBtbInternalBankIO)

  // alias
  private val read  = io.read
  private val write = io.write
  private val flush = io.flush
  private val probe = io.probe

  private val entrySrams = Seq.tabulate(NumWay) { wayIdx =>
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
        hasSramCtl = hasSramCtl,
        suffix = Option("bpu_mbtb_entry")
      )
    ).suggestName(s"mbtb_sram_entry_align${alignIdx}_bank${bankIdx}_way${wayIdx}")
  }

  // we often need to update counter, but not the whole entry, so store counters in separate SRAMs for better power
  private val sharedSrams = Seq.tabulate(NumWay) { wayIdx =>
    Module(
      new SRAMTemplate(
        new MainBtbSharedInfo,
        set = NumSets,
        way = 1, // Not using way in the template, preparing for future skewed assoc
        singlePort = true,
        shouldReset = true,
        holdRead = true,
        withClockGate = true,
        hasMbist = hasMbist,
        hasSramCtl = hasSramCtl,
        suffix = Option("bpu_mbtb_entry")
      )
    ).suggestName(s"mbtb_sram_shared_align${alignIdx}_bank${bankIdx}_way${wayIdx}")
  }

  private val writeBuffer = Module(new MainBtbWriteBuffer(
    numPorts = NumWay,
    numEntries = WriteBufferSize,
    nameSuffix = s"mbtb_entry_align${alignIdx}_bank${bankIdx}"
  ))

  private val resetDone = RegInit(false.B)
  when((entrySrams :++ sharedSrams).map(_.io.r.req.ready).reduce(_ && _)) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  /* *** sram -> io *** */
  // handle entry & shared together
  (entrySrams :++ sharedSrams).foreach { sram =>
    sram.io.r.req.valid       := read.req.valid
    sram.io.r.req.bits.setIdx := read.req.bits.setIdx
  }
  // each entry sram template has 1 way, so here we only read data.head
  read.resp.entries := VecInit(entrySrams.map(_.io.r.resp.data.head))
  read.resp.shareds := VecInit(sharedSrams.map(_.io.r.resp.data.head))

  /* *** writeBuffer -> sram *** */
  // entry
  (entrySrams zip writeBuffer.io.read).foreach { case (way, bufRead) =>
    way.io.w.req.valid        := bufRead.valid && bufRead.bits.status.isWriteAll && !way.io.r.req.valid
    way.io.w.req.bits.data(0) := bufRead.bits.entry
    way.io.w.req.bits.setIdx  := bufRead.bits.setIdx
    bufRead.ready             := way.io.w.req.ready && !way.io.r.req.valid
  }

  (sharedSrams zip writeBuffer.io.read).foreach { case (way, bufRead) =>
    way.io.w.req.valid        := bufRead.valid && !way.io.r.req.valid
    way.io.w.req.bits.data(0) := bufRead.bits.shared
    way.io.w.req.bits.setIdx  := bufRead.bits.setIdx
    bufRead.ready             := way.io.w.req.ready && !way.io.r.req.valid
  }

  /* *** io -> writeBuffer *** */
  // entry
  private val entryWriteMask = VecInit.tabulate(NumWay) { i =>
    write.req.bits.wayMask(i) &&
    write.req.bits.status(i).isWriteAll
  }
  private val writeEntry = Mux1H(entryWriteMask, io.write.req.bits.entries)
  XSError(
    write.req.valid && PopCount(entryWriteMask) > 1.U,
    f"alignBank${alignIdx}_bank${bankIdx}, write entry mask must be one-hot"
  )

  private val conflict =
    write.req.valid &&
      write.req.bits.setIdx === flush.req.bits.setIdx &&
      writeEntry.tagLower === 0.U

  writeBuffer.io.write.zipWithIndex.foreach { case (bufWrite, i) =>
    val writeValid = write.req.valid && write.req.bits.wayMask(i)
    val flushValid = flush.req.valid && flush.req.bits.wayMask(i) && !conflict
    val valid      = writeValid || flushValid
    bufWrite.valid := RegNext(valid, false.B)
    bufWrite.bits.setIdx := RegEnable(
      Mux(
        writeValid,
        write.req.bits.setIdx,
        flush.req.bits.setIdx
      ),
      valid
    )
    bufWrite.bits.entry := RegEnable(
      Mux(
        writeValid,
        write.req.bits.entries(i),
        0.U.asTypeOf(new MainBtbEntry)
      ),
      valid
    )
    bufWrite.bits.shared := RegEnable(
      Mux(
        writeValid,
        write.req.bits.shareds(i),
        0.U.asTypeOf(new MainBtbSharedInfo)
      ),
      valid
    )
    bufWrite.bits.status := RegEnable(
      Mux(
        writeValid,
        write.req.bits.status(i),
        0.U.asTypeOf(new MainBtbWriteStatus)
      ),
      valid
    )
  }

  probe.buffers := writeBuffer.io.probe

  /* *** perf *** */
  private val perf_entryDropWrite = (0 until NumWay).map { i =>
    write.req.valid && write.req.bits.wayMask(i) && !writeBuffer.io.write(i).ready
  }.reduce(_ || _)

  XSPerfAccumulate(
    "multihit_write_conflict",
    write.req.valid && flush.req.valid && write.req.bits.setIdx === flush.req.bits.setIdx &&
      (write.req.bits.wayMask & flush.req.bits.wayMask).orR
  )
  XSPerfAccumulate(
    "entry_writebuffer_drop_write",
    perf_entryDropWrite
  )
}
