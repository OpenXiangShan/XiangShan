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
import org.chipsalliance.cde.config.Parameters
import utility.ReplacementPolicy
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import xiangshan.XSModule

/**
 * A write buffer that supports multiple write ports and read ports, including the following features:
 * 1. Handling SRAM Read-Write Conflicts
 * 2. The temporary write data can be updated
 * 3. Written SRAM entries undergo write comparison - only differing data triggers re-write
 * 4. Entries with saturation counters support counter updates
 * 5. Single-write-multiple-read (SWMR) port configuration
 * 6. Bypass write data to read port when empty
 * @param gen The type of the write request bundle
 * @param numEntries The number of entries in the write buffer
 * @param numPorts The number of write ports
 * @param usefulWidth The width of the useful counter, used to determine if the entry is useful
 * @param hasCnt Whether the write request bundle has a counter field, used to update the entry's useful counter
 * @param pipe Whether the write buffer is pipelined, used to determine if the read
 * @param hasFlush Whether the write buffer has a flush signal, used to reset the write bufferq
*/
class WriteBuffer[T <: WriteReqBundle](
    gen:             T,
    val numEntries:  Int,
    val numPorts:    Int,
    val usefulWidth: Int = 1,
    val hasCnt:      Boolean = false,
    val pipe:        Boolean = false,
    val hasFlush:    Boolean = false
)(implicit p: Parameters) extends XSModule {
  require(numEntries >= 0)
  require(usefulWidth >= 1)
  require(numPorts >= 1)
  class WriteBufferIO extends Bundle {
    val write: Vec[DecoupledIO[T]] = Vec(numPorts, Flipped(DecoupledIO(gen)))
    val read:  Vec[DecoupledIO[T]] = Vec(numPorts, DecoupledIO(gen))
    val flush: Option[Bool]        = Option.when(hasFlush)(Input(Bool()))
    val taken: Option[Bool]        = Option.when(hasCnt)(Input(Bool()))
  }
  val io: WriteBufferIO = IO(new WriteBufferIO)

  // clean write buffer when flush is true
  private val flush = io.flush.getOrElse(false.B)

  private val usefulCnts =
    RegInit(VecInit(Seq.fill(numPorts)(VecInit(Seq.fill(numEntries)(0.U.asTypeOf(new SaturateCounter(usefulWidth)))))))
  private val entries = RegInit(VecInit(Seq.fill(numPorts)(VecInit(Seq.fill(numEntries)(0.U.asTypeOf(gen.cloneType))))))
  private val valids  = RegInit(VecInit(Seq.fill(numPorts)(VecInit(Seq.fill(numEntries)(false.B)))))
  private val hitMask = WireInit(VecInit(Seq.fill(numPorts)(VecInit(Seq.fill(numEntries)(false.B)))))
  private val writeValidPortVec = WireInit(VecInit(Seq.fill(numPorts)(false.B)))
  private val writePort         = Wire(Valid(gen.cloneType))
  private val hit               = WireInit(false.B)

  for (i <- 0 until numPorts) {
    writeValidPortVec(i) := io.write(i).valid
  }
  assert(PopCount(writeValidPortVec) <= 1.U, "Only one port can be written at a time")

  when(!writeValidPortVec.reduce(_ || _)) {
    writePort.valid := false.B
    writePort.bits  := DontCare
  }.otherwise {
    writePort.valid := io.write(OHToUInt(writeValidPortVec)).valid
    writePort.bits  := io.write(OHToUInt(writeValidPortVec)).bits
  }

  for (p <- 0 until numPorts) {
    for (e <- 0 until numEntries) {
      hitMask(p)(e) := writePort.valid && valids(p)(e) && writePort.bits.setIdx === entries(p)(e).setIdx &&
        writePort.bits.tag.getOrElse(0.U) === entries(p)(e).tag.getOrElse(0.U) &&
        writePort.bits.position.getOrElse(0.U) === entries(p)(e).position.getOrElse(0.U)
    }
  }
  assert(PopCount(hitMask.flatten) <= 1.U, "WriteBuffer hitMask should be one-hot")

  hit := hitMask.flatten.reduce(_ || _)

  for (port <- 0 until numPorts) {
    val hitTouch   = WireInit(0.U.asTypeOf(Valid(UInt(log2Ceil(numEntries).W))))
    val writeTouch = WireInit(0.U.asTypeOf(Valid(UInt(log2Ceil(numEntries).W))))
    val writeValid = WireInit(io.write(port).valid)
    val readReady  = WireInit(io.read(port).ready)
    val replacer   = ReplacementPolicy.fromString("plru", numEntries)

    // Select a flag entry for writing to SRAM
    val readVec = VecInit(usefulCnts(port).map(_.isPositive))
    val readIdx = PriorityEncoder(readVec)

    val empty                = !readVec.reduce(_ || _)
    val full                 = readVec.reduce(_ && _)
    val victim               = WireInit(0.U(log2Up(numEntries).W))
    val notUsefulVec         = VecInit(usefulCnts(port).map(_.isSaturateNegative))
    val notUseful            = notUsefulVec.reduce(_ || _)
    val notUsefulIdx         = PriorityEncoder(notUsefulVec)
    val replacerWayNotUseful = usefulCnts(port)(replacer.way).isSaturateNegative
    val hitUsefulEntry       = hitTouch.valid && usefulCnts(port)(hitTouch.bits).isPositive
    val hitUselessEntry      = hitTouch.valid && usefulCnts(port)(hitTouch.bits).isNegative
    val writeFlow            = !hit && readReady && empty

    victim               := Mux(replacerWayNotUseful, replacer.way, Mux(notUseful, notUsefulIdx, replacer.way))
    io.write(port).ready := !full
    io.read(port).valid  := !empty
    io.read(port).bits   := DontCare

    // If a hit occurs, update the replacer regardless of whether the entry is valid
    hitTouch.valid := hitMask(port).reduce(_ || _)
    hitTouch.bits  := OHToUInt(hitMask(port))
    assert(PopCount(hitMask(port)) <= 1.U, "WriteBuffer hitMask should be one-hot")

    when(readReady && !empty) {
      io.read(port).bits        := entries(port)(readIdx)
      usefulCnts(port)(readIdx) := 0.U.asTypeOf(new SaturateCounter(usefulWidth))
    }

    /**
     * Write request processing cases:
     * 1. Write hit
     *  1.1 Hit occurs on a useful entry update the entry
     *  1.2 Hit occurs on a useless entry with data mismatch, triggers re-write
     * 2. Write miss but bypassable
     *  Write flow and record the entry as useless
     * 3. Default handling
     *  Write data into WriteBuffer
    */

    when(writeValid) {
      when(hit) {
        when(hitUsefulEntry) {
          entries(port)(hitTouch.bits) := io.write(port).bits
          valids(port)(hitTouch.bits)  := true.B
        }.elsewhen(hitUselessEntry) {
          val entryChange = entries(port)(hitTouch.bits).asUInt =/= io.write(port).bits.asUInt
          when(entryChange && !hasCnt.B) {
            usefulCnts(port)(hitTouch.bits).increase()
            entries(port)(hitTouch.bits) := io.write(port).bits
            valids(port)(hitTouch.bits)  := true.B
          }
        }
      }.elsewhen(writeFlow) {
        entries(port)(victim)    := io.write(port).bits
        valids(port)(victim)     := true.B
        usefulCnts(port)(victim) := 0.U.asTypeOf(new SaturateCounter(usefulWidth))
        io.read(port).valid      := true.B
        io.read(port).bits       := io.write(port).bits
        writeTouch.valid         := true.B
        writeTouch.bits          := victim
      }.otherwise {
        entries(port)(victim) := io.write(port).bits
        valids(port)(victim)  := true.B
        usefulCnts(port)(victim).increase()
        writeTouch.valid := true.B
        writeTouch.bits  := victim
      }
    }

    if (pipe) {
      when(io.read(port).ready)(io.write(port).ready := true.B)
    }
    // If the write request has conuter need update
    val temporarily = WireInit(io.write(port).bits)
    if (hasCnt) {
      when(hitTouch.valid) {
        // If a write request hits an entry, update its counter using 'taken'
        // and write other entry information
        val updateCnt = entries(port)(hitTouch.bits).cnt.get.getUpdate(io.taken.getOrElse(false.B))
        temporarily.cnt.get          := updateCnt.asTypeOf(entries(port)(hitTouch.bits).cnt.get)
        entries(port)(hitTouch.bits) := temporarily
        valids(port)(hitTouch.bits)  := true.B
        // If the write request hits a not write flag entry, update the usefulCnts
        when(usefulCnts(port)(hitTouch.bits).isNegative) {
          usefulCnts(port)(hitTouch.bits).increase()
        }
      }
    }
    when(flush) {
      // Reset the write buffer usefulCnts when flush is true
      for (i <- 0 until numEntries) {
        usefulCnts(port)(i) := 0.U.asTypeOf(new SaturateCounter(usefulWidth))
      }
    }

    replacer.access(Seq(hitTouch, writeTouch))

    XSPerfAccumulate(f"write_buffer_port${port}_hit_useful_entry", writeValid && hitUsefulEntry)
    XSPerfAccumulate(f"write_buffer_port${port}_hit_useless_entry", writeValid && hitUselessEntry)
    XSPerfAccumulate(f"write_buffer_port${port}_not_hit", writeValid && !hitTouch.valid)
    XSPerfAccumulate(f"write_buffer_port${port}_not_hit_flow", writeValid && writeFlow)
    XSPerfAccumulate(f"write_buffer_port${port}_is_full", writeValid && full)
    XSPerfHistogram(f"write_buffer_port${port}_useful", PopCount(readVec), readReady, 0, numEntries)
  }
  XSPerfAccumulate("write_buffer_hit", writePort.valid && hit)
}
