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
import utility.XSPerfAccumulate
import utility.sram.SRAMTemplate
import xiangshan.frontend.bpu.SaturateCounter

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
        val entries:  Vec[MainBtbEntry]    = Vec(NumWay, new MainBtbEntry)
        val counters: Vec[SaturateCounter] = Vec(NumWay, TakenCounter())
      }

      val req:  Valid[Req] = Flipped(Valid(new Req))
      val resp: Resp       = Output(new Resp)
    }

    class WriteEntry extends Bundle {
      class Req extends Bundle {
        val setIdx:  UInt         = UInt(SetIdxLen.W)
        val wayMask: UInt         = UInt(NumWay.W)
        val entry:   MainBtbEntry = new MainBtbEntry
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    class WriteCounter extends Bundle {
      class Req extends Bundle {
        val setIdx:   UInt                 = UInt(SetIdxLen.W)
        val wayMask:  UInt                 = UInt(NumWay.W)
        val counters: Vec[SaturateCounter] = Vec(NumWay, TakenCounter())
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

    val sramResetDone: Bool = Output(Bool())

    val read:         Read         = new Read
    val writeEntry:   WriteEntry   = new WriteEntry
    val writeCounter: WriteCounter = new WriteCounter
    val flush:        Flush        = new Flush
  }

  val io: MainBtbInternalBankIO = IO(new MainBtbInternalBankIO)

  // alias
  private val read         = io.read
  private val writeEntry   = io.writeEntry
  private val writeCounter = io.writeCounter
  private val flush        = io.flush

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
  private val counterSram = Module(new SRAMTemplate(
    TakenCounter(),
    set = NumSets,
    way = NumWay,
    singlePort = true,
    shouldReset = true,
    holdRead = true,
    withClockGate = true,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl,
    suffix = Option("bpu_mbtb_counter")
  )).suggestName(s"mbtb_sram_counter_align${alignIdx}_bank${bankIdx}")

  private val writeBuffer = Module(new MainBtbWriteBuffer)
  writeBuffer.suggestName(s"mbtb_writebuffer_align${alignIdx}_bank${bankIdx}")

  io.sramResetDone := entrySrams.map(_.io.resetDone).reduce(_ && _) && counterSram.io.resetDone

  /* *** sram -> io *** */
  // handle entry & counter together
  (entrySrams :+ counterSram).foreach { sram =>
    sram.io.r.req.valid       := read.req.valid
    sram.io.r.req.bits.setIdx := read.req.bits.setIdx
  }
  writeBuffer.io.readBypass.req.valid := read.req.valid
  writeBuffer.io.readBypass.req.bits  := read.req.bits.setIdx
  private val bypass = writeBuffer.io.readBypass.resp
  // each entry sram template has 1 way, so here we only read data.head
  read.resp.entries := VecInit(entrySrams.zipWithIndex.map { case (sram, wayIdx) =>
    Mux(bypass.valid && bypass.entryWayMask(wayIdx), bypass.entries(wayIdx), sram.io.r.resp.data.head)
  })
  read.resp.counters := VecInit(counterSram.io.r.resp.data.zipWithIndex.map { case (sramCounter, wayIdx) =>
    Mux(bypass.valid && bypass.counterMask(wayIdx), bypass.counters(wayIdx), sramCounter)
  })

  /* *** writeBuffer -> sram *** */
  private val drainBits = writeBuffer.io.drain.bits
  entrySrams.zipWithIndex.foreach { case (way, wayIdx) =>
    way.io.w.req.valid        := writeBuffer.io.drain.valid && drainBits.entryWayMask(wayIdx) && !way.io.r.req.valid
    way.io.w.req.bits.data(0) := drainBits.entries(wayIdx)
    way.io.w.req.bits.setIdx  := drainBits.setIdx
  }
  counterSram.io.w.req.valid := writeBuffer.io.drain.valid && drainBits.counterMask.orR && !counterSram.io.r.req.valid
  counterSram.io.w.req.bits.data        := drainBits.counters
  counterSram.io.w.req.bits.setIdx      := drainBits.setIdx
  counterSram.io.w.req.bits.waymask.get := drainBits.counterMask
  private val entrySramReady = VecInit((0 until NumWay).map { wayIdx =>
    !drainBits.entryWayMask(wayIdx) || entrySrams(wayIdx).io.w.req.ready
  }).asUInt.andR
  private val counterSramReady = !drainBits.counterMask.orR || counterSram.io.w.req.ready
  writeBuffer.io.drain.ready := entrySramReady && counterSramReady && !read.req.valid

  /* *** io -> writeBuffer *** */
  // single write port: training (entry + counter) takes priority over a multi-hit flush. A flush
  // that loses arbitration is dropped; it is re-detected on the next access to that set.
  private val trainingValid = writeEntry.req.valid || writeCounter.req.valid

  writeBuffer.io.write.valid := trainingValid || flush.req.valid
  writeBuffer.io.write.bits.setIdx := Mux(
    trainingValid,
    Mux(writeEntry.req.valid, writeEntry.req.bits.setIdx, writeCounter.req.bits.setIdx),
    flush.req.bits.setIdx
  )
  writeBuffer.io.write.bits.entryWayMask := Mux(
    trainingValid,
    Mux(writeEntry.req.valid, writeEntry.req.bits.wayMask, 0.U),
    flush.req.bits.wayMask
  )
  // A counter-only write (or a flush) must carry entry = 0 so the write buffer's duplicate check
  // never treats it as an entry write; only a real writeEntry request may forward its entry.
  writeBuffer.io.write.bits.entry := Mux(
    writeEntry.req.valid,
    writeEntry.req.bits.entry,
    0.U.asTypeOf(new MainBtbEntry)
  )
  writeBuffer.io.write.bits.counterMask := Mux(
    trainingValid,
    Mux(writeCounter.req.valid, writeCounter.req.bits.wayMask, 0.U),
    0.U
  )
  writeBuffer.io.write.bits.counters := Mux(
    trainingValid,
    writeCounter.req.bits.counters,
    0.U.asTypeOf(Vec(NumWay, TakenCounter()))
  )

  XSPerfAccumulate("multihit_flush_dropped", flush.req.valid && trainingValid)
}
