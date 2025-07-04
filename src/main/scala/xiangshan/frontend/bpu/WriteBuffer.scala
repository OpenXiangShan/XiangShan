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

package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.Random.oneHot
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._

// Ensure read priority and temporarily store write data in case of read/write conflicts
// Ensure that the data is up-to-date and that the data stored in the write buffer can be updated
class WriteBuffer[T <: WriteReqBundle](
    gen:             T,
    val NumEntries:  Int,
    val usefulWidth: Int = 1,
    val hasCnt:      Boolean = false,
    val pipe:        Boolean = false,
    val hasFlush:    Boolean = false
)(implicit p: Parameters) extends XSModule {
  require(NumEntries >= 0)
  require(usefulWidth >= 1)
  val io = IO(new Bundle {
    val write: DecoupledIO[T] = Flipped(DecoupledIO(gen))
    val read:  DecoupledIO[T] = DecoupledIO(gen)
    val flush: Option[Bool]   = Option.when(hasFlush)(Input(Bool()))
    val taken: Option[Bool]   = Option.when(hasCnt)(Input(Bool()))
  })

  // clean write buffer when flush is true
  private val flush = io.flush.getOrElse(false.B)

  private val usefulCnts = RegInit(0.U.asTypeOf(Vec(NumEntries, new SaturateCounter(usefulWidth))))
  private val entries    = RegInit(VecInit(Seq.fill(NumEntries)(0.U.asTypeOf(gen.cloneType))))

  private val hitMask    = WireDefault(VecInit(Seq.fill(NumEntries)(false.B)))
  private val hitTouch   = WireDefault(0.U.asTypeOf(Valid(UInt(log2Ceil(NumEntries).W))))
  private val writeTouch = WireDefault(0.U.asTypeOf(Valid(UInt(log2Ceil(NumEntries).W))))

  private val writeValid = WireDefault(io.write.valid)
  private val readReady  = WireDefault(io.read.ready)

  // Used to select a flag entry for writing to SRAM
  private val readVec = VecInit(usefulCnts.map(_.isPositive))
  private val readIdx = PriorityEncoder(readVec)

  private val replacer = ReplacementPolicy.fromString("plru", NumEntries) // TODO: make it configurable

  private val empty = !readVec.reduce(_ || _)
  private val full  = readVec.reduce(_ && _)

  private val victim = WireDefault(0.U(log2Up(NumEntries).W))

  private val notUsefulVec = VecInit(usefulCnts.map(_.isSaturateNegative))
  private val notUseful    = notUsefulVec.reduce(_ || _)
  private val notUsefulIdx = PriorityEncoder(notUsefulVec)
  private val replacerWayNotUseful =
    usefulCnts(replacer.way).isSaturateNegative // If the replacer way is invalid, we need to priority select

  victim := Mux(replacerWayNotUseful, replacer.way, Mux(notUseful, notUsefulIdx, replacer.way))

  io.write.ready := !full
  io.read.valid  := !empty
  io.read.bits   := DontCare

  when(writeValid) {
    for (i <- 0 until NumEntries) {
      hitMask(i) := io.write.bits.setIdx === entries(i).setIdx &&
        io.write.bits.tag === entries(i).tag
    }
    assert(PopCount(hitMask) <= 1.U, "WriteBuffer hitMask should be one-hot")
    // If a hit occurs, update the replacer regardless of whether the entry is valid
    hitTouch.valid := hitMask.reduce(_ || _)
    hitTouch.bits  := OHToUInt(hitMask)
    when(hitTouch.valid) {
      // If the write request hits a valid entry, update the entry
      when(usefulCnts(hitTouch.bits).isPositive) {
        entries(hitTouch.bits) := io.write.bits
      }
    }.elsewhen(!hitTouch.valid && readReady && empty) {
      // If the current write request misses, the WriteBuffer is empty, and the read request is ready,
      // forward the write data stream to the read request.
      entries(victim)    := io.write.bits
      usefulCnts(victim) := 0.U.asTypeOf(new SaturateCounter(usefulWidth))
      io.read.bits       := io.write.bits
      writeTouch.valid   := true.B
      writeTouch.bits    := victim
    }.otherwise {
      // If the victim is not valid, we enqueue the new data
      entries(victim) := io.write.bits
      usefulCnts(victim).increase()
      writeTouch.valid := true.B
      writeTouch.bits  := victim
    }
  }

  when(readReady) {
    when(!empty) {
      // If there is a valid entry, we read the data
      io.read.bits := entries(readIdx)
      // io.read.valid := true.B
      usefulCnts(readIdx) := 0.U.asTypeOf(new SaturateCounter(usefulWidth))
    }.elsewhen(writeValid && empty) {
      io.read.valid := true.B
    }.otherwise {
      // If there is no valid entry, we do not read
      io.read.valid := false.B
    }
  }

  if (pipe) {
    when(io.read.ready)(io.write.ready := true.B)
  }

  // If the write request has conuter need update
  private val temporarily = WireDefault(io.write.bits)
  if (hasCnt) {
    when(hitTouch.valid) {
      // If a write request hits an entry, update its counter using 'taken'
      // and write other entry information
      val updateCnt = entries(hitTouch.bits).cnt.get.getUpdate(io.taken.getOrElse(false.B))
      temporarily.cnt.get    := updateCnt.asTypeOf(entries(hitTouch.bits).cnt.get)
      entries(hitTouch.bits) := temporarily
      // If the write request hits a not write flag entry, update the usefulCnts
      when(usefulCnts(hitTouch.bits).isNegative) {
        usefulCnts(hitTouch.bits).increase()
      }
    }
  }

  when(flush) {
    // Reset the write buffer usefulCnts when flush is true
    for (i <- 0 until NumEntries) {
      usefulCnts(i) := 0.U.asTypeOf(new SaturateCounter(usefulWidth))
    }
  }

  replacer.access(Seq(hitTouch, writeTouch))

  XSPerfAccumulate("hit_write_update", writeValid && hitTouch.valid && usefulCnts(hitTouch.bits).isPositive)
  XSPerfAccumulate("hit_recent_entry", writeValid && hitTouch.valid && usefulCnts(hitTouch.bits).isNegative)
  XSPerfAccumulate("write_request_miss", writeValid && !hitTouch.valid)
  XSPerfAccumulate("write_request_drop", writeValid && full)
}
