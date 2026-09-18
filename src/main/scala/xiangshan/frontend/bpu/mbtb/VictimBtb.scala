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
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.SaturateCounter

class VictimBtb(implicit p: Parameters) extends MainBtbModule {
  class VictimBtbIO extends Bundle {
    class Read extends Bundle {
      class Resp extends Bundle {
        val entries: Vec[VictimBtbEntry] = Vec(NumVictimBtbWays, new VictimBtbEntry)
      }
      val resp: Resp = Output(new Resp)
    }

    class WriteEntry extends Bundle {
      class Req extends Bundle {
        val setIdx:          UInt         = UInt(SetIdxLen.W)
        val internalBankIdx: UInt         = UInt(InternalBankIdxLen.W)
        val wayMask:         UInt         = UInt(NumVictimBtbWays.W)
        val flushMask:       UInt         = UInt(NumVictimBtbWays.W)
        val entry:           MainBtbEntry = new MainBtbEntry
      }
      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    class TrainEntry extends Bundle {
      class Req extends Bundle {
        val entryWayMask:   UInt                 = UInt(NumVictimBtbWays.W)
        val counterWayMask: UInt                 = UInt(NumVictimBtbWays.W)
        val entry:          MainBtbEntry         = new MainBtbEntry
        val counters:       Vec[SaturateCounter] = Vec(NumVictimBtbWays, TakenCounter())
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    class Flush extends Bundle {
      class Req extends Bundle {
        val wayMask: UInt = UInt(NumVictimBtbWays.W)
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    val read:           Read       = new Read
    val writeEntryRead: Read       = new Read
    val trainEntryRead: Read       = new Read
    val writeEntry:     WriteEntry = new WriteEntry
    val trainEntry:     TrainEntry = new TrainEntry
    val flush:          Flush      = new Flush
  }
  val io: VictimBtbIO = IO(new VictimBtbIO)

  private val read           = io.read
  private val writeEntryRead = io.writeEntryRead
  private val trainEntryRead = io.trainEntryRead
  private val writeEntry     = io.writeEntry
  private val trainEntry     = io.trainEntry
  private val flush          = io.flush

  private val entries = Reg(Vec(NumVictimBtbWays, new VictimBtbEntry))

  // The VBTB is fully associative. All entries are exposed through combinational
  // Reg reads and MainBtbAlignBank performs full setIdx/tag/position matching.
  read.resp.entries           := entries
  writeEntryRead.resp.entries := entries
  trainEntryRead.resp.entries := entries

  // Training can update entry and counters independently:
  // - entryWayMask rewrites the victim BTB entry payload.
  // - counterWayMask updates the taken counter for conditional branches.
  // This allows a VBTB hit to be repaired in place without allocating MainBtb.
  for (w <- 0 until NumVictimBtbWays) {
    when(trainEntry.req.valid) {
      when(trainEntry.req.bits.counterWayMask(w)) {
        entries(w).counter := trainEntry.req.bits.counters(w)
      }
      when(trainEntry.req.bits.entryWayMask(w)) {
        entries(w).entry := trainEntry.req.bits.entry
      }
    }

    // Flushes override ordinary training updates.
    when(flush.req.valid && flush.req.bits.wayMask(w)) {
      entries(w).entry.attribute := BranchAttribute.None
    }

    // Snapshot insertion has priority over training and flushes. This preserves
    // a newly evicted MainBtb entry if the same physical VBTB way is also flushed.
    when(writeEntry.req.valid) {
      when(writeEntry.req.bits.flushMask(w)) {
        entries(w).entry.attribute := BranchAttribute.None
      }
      when(writeEntry.req.bits.wayMask(w)) {
        entries(w).entry           := writeEntry.req.bits.entry
        entries(w).setIdx          := writeEntry.req.bits.setIdx
        entries(w).internalBankIdx := writeEntry.req.bits.internalBankIdx
        entries(w).counter         := TakenCounter.WeakPositive
      }
    }
  }

  when(reset.asBool) {
    entries.foreach(e => e.entry.attribute := BranchAttribute.None)
  }
}
