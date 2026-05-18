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
import xiangshan.frontend.bpu.SaturateCounter

class VictimBtb(implicit p: Parameters) extends MainBtbModule {
  class VictimBtbIO extends Bundle {
    class Read extends Bundle {
      class Req extends Bundle {
        val setIdx: UInt = UInt(VictimBtbSetIdxLen.W)
      }
      class Resp extends Bundle {
        val entries: Vec[VictimBtbEntry] = Vec(NumWay, new VictimBtbEntry)
      }
      val req:  Req  = Flipped(new Req)
      val resp: Resp = Output(new Resp)
    }

    class WriteEntry extends Bundle {
      class Req extends Bundle {
        val setIdx:    UInt         = UInt(SetIdxLen.W)
        val wayMask:   UInt         = UInt(NumWay.W)
        val flushMask: UInt         = UInt(NumWay.W)
        val entry:     MainBtbEntry = new MainBtbEntry
      }
      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    class TrainEntry extends Bundle {
      class Req extends Bundle {
        val setIdx:         UInt                 = UInt(SetIdxLen.W)
        val entryWayMask:   UInt                 = UInt(NumWay.W)
        val counterWayMask: UInt                 = UInt(NumWay.W)
        val entry:          MainBtbEntry         = new MainBtbEntry
        val counters:       Vec[SaturateCounter] = Vec(NumWay, TakenCounter())
      }

      val req: Valid[Req] = Flipped(Valid(new Req))
    }

    class Flush extends Bundle {
      class Req extends Bundle {
        val setIdx:  UInt = UInt(VictimBtbSetIdxLen.W)
        val wayMask: UInt = UInt(NumWay.W)
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

  private val entries = Reg(Vec(NumVictimBtbSets, Vec(NumWay, new VictimBtbEntry)))

  // VBTB read ports are combinational Reg reads. MainBtbAlignBank drives the
  // truncated setIdx in S1/T1/writeback paths and does full setIdx/tag/position
  // matching outside this module.
  read.resp.entries           := entries(read.req.setIdx)
  writeEntryRead.resp.entries := entries(writeEntryRead.req.setIdx)
  trainEntryRead.resp.entries := entries(trainEntryRead.req.setIdx)

  // Training can update entry and counter independently:
  // - entryWayMask rewrites the victim BTB entry payload.
  // - counterWayMask updates the taken counter for conditional branches.
  // This allows a VBTB hit to be repaired in place without allocating MainBtb.
  private val updateEntries = entries(trainEntry.req.bits.setIdx.take(VictimBtbSetIdxLen))
  for (w <- 0 until NumWay) {
    when(trainEntry.req.valid) {
      when(trainEntry.req.bits.counterWayMask(w)) {
        updateEntries(w).counter := trainEntry.req.bits.counters(w)
      }
      when(trainEntry.req.bits.entryWayMask(w)) {
        updateEntries(w).entry := trainEntry.req.bits.entry
      }
    }
  }

  // Snapshot write inserts an entry evicted from MainBtb. If the same cycle also
  // flushes a duplicated incoming entry, write has higher priority so the
  // newly evicted entry is not lost.
  private val writeEntries = entries(writeEntry.req.bits.setIdx.take(VictimBtbSetIdxLen))
  for (w <- 0 until NumWay) {
    when(writeEntry.req.valid) {
      when(writeEntry.req.bits.flushMask(w)) {
        writeEntries(w).entry.valid := false.B
      }
      // write entry has higher priority than flush
      when(writeEntry.req.bits.wayMask(w)) {
        writeEntries(w).entry   := writeEntry.req.bits.entry
        writeEntries(w).setIdx  := writeEntry.req.bits.setIdx
        writeEntries(w).counter := TakenCounter.WeakPositive
      }
    }
  }

  // Flush is used to remove duplicated VBTB entries when MainBtb already holds
  // a matching prediction position.
  for (w <- 0 until NumWay) {
    when(flush.req.valid && flush.req.bits.wayMask(w)) {
      entries(flush.req.bits.setIdx)(w).entry.valid := false.B
    }
  }

  when(reset.asBool) {
    entries.foreach(_.foreach(_.entry.valid := false.B))
  }
}
