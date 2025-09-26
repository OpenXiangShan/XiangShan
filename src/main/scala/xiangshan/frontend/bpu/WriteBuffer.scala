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
import utility.XSError
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
 * @param hasCnt Whether the write request bundle has a counter field, used to update the entry's useful counter
 * @param hasFlush Whether the write buffer has a flush signal, used to reset the write bufferq
*/
class WriteBuffer[T <: WriteReqBundle](
    gen:            T,
    val numEntries: Int = 1,
    val numPorts:   Int = 1,
    val hasCnt:     Boolean = false,
    val hasFlush:   Boolean = false
)(implicit p: Parameters) extends XSModule {
  require(numEntries >= 0)
  require(numPorts >= 1)
  require(numPorts <= numEntries)
  class WriteBufferIO extends Bundle {
    val write: Vec[DecoupledIO[T]] = Vec(numPorts, Flipped(DecoupledIO(gen)))
    val read:  Vec[DecoupledIO[T]] = Vec(numPorts, DecoupledIO(gen))
    val flush: Option[Bool]        = Option.when(hasFlush)(Input(Bool()))
  }
  val io: WriteBufferIO = IO(new WriteBufferIO)

  // clean write buffer when flush is true
  private val flush = io.flush.getOrElse(false.B)

  private val needWrite = RegInit(VecInit(Seq.fill(numPorts)(VecInit(Seq.fill(numEntries)(false.B)))))
  private val entries = RegInit(VecInit(Seq.fill(numPorts)(VecInit(Seq.fill(numEntries)(0.U.asTypeOf(gen.cloneType))))))
  private val valids  = RegInit(VecInit(Seq.fill(numPorts)(VecInit(Seq.fill(numEntries)(false.B)))))

  private val writePortValid = VecInit(Seq.fill(numPorts)(false.B))
  // writePortNum * histMask ->  writePortNum * (readPortNum * numEntries)
  private val portsHitMask = VecInit.fill(numPorts)(VecInit.fill(numPorts)(VecInit.fill(numEntries)(false.B)))
  private val hitTouchVec =
    WireInit(VecInit.fill(numPorts)(VecInit.fill(numEntries)(0.U.asTypeOf(Valid(UInt(log2Ceil(numEntries).W))))))
  private val writeTouchVec = WireInit(VecInit.fill(numPorts)(0.U.asTypeOf(Valid(UInt(log2Ceil(numEntries).W)))))
  // private val replacer      = ReplacementPolicy.fromString(Some("setplru"), numEntries, numPorts)
  private val replacerWay  = Wire(Vec(numPorts, UInt(log2Ceil(numEntries).W)))
  private val readValidVec = WireInit(VecInit.fill(numPorts)(VecInit.fill(numEntries)(false.B)))
  private val readReadyVec = WireInit(VecInit(Seq.fill(numPorts)(false.B)))
  private val emptyVec     = WireInit(VecInit(Seq.fill(numPorts)(false.B)))
  private val fullVec      = WireInit(VecInit(Seq.fill(numPorts)(false.B)))
  private val writeFlowVec = WireInit(VecInit(Seq.fill(numPorts)(false.B)))

  private val writePortVec = io.write
  dontTouch(writePortVec)
  dontTouch(readValidVec)
  dontTouch(replacerWay)
  dontTouch(emptyVec)

  // not allowed to write entries with same setIdx and tag
  for {
    i <- 0 until numPorts
    j <- i + 1 until numPorts
  } {
    XSError(
      writePortVec(i).valid && writePortVec(j).valid && writePortVec(i).bits.setIdx === writePortVec(j).bits.setIdx &&
        writePortVec(i).bits.tag.getOrElse(0.U) === writePortVec(j).bits.tag.getOrElse(0.U),
      "WriteBuffer can not support write same data"
    )
  }

  // maintain a set of hitMask for each write port
  for {
    i <- 0 until numPorts
    p <- 0 until numPorts
    e <- 0 until numEntries
  } {
    portsHitMask(i)(p)(e) := writePortVec(i).valid && valids(p)(e) &&
      writePortVec(i).bits.setIdx === entries(p)(e).setIdx &&
      writePortVec(i).bits.tag.getOrElse(0.U) === entries(p)(e).tag.getOrElse(0.U)
    writePortValid(i) := writePortVec(i).valid
  }

  /**
     * Write request processing cases:
     * 1. Write miss but bypassable
     *  1.1 Write data into WriteBuffer
     * 2. Write hit
     *  2.1 Hit occurs on a useful entry update the entry
     *  2.2 Hit occurs on a useless entry with data mismatch, triggers re-write
     *  2.3 Hit and hasCnt, update the entry's counter
    */
  writePortValid.zip(portsHitMask).zipWithIndex.foreach { case ((writeValid, hitMask), portIdx) =>
    // each write port can only hit at most one entry
    XSError(
      writeValid && PopCount(hitMask.flatten) > 1.U,
      f"WriteBuffer port${portIdx}_hitMask should be no more than 1"
    )
    val hit           = hitMask.flatten.reduce(_ || _)
    val hitPortVec    = VecInit(hitMask.map(_.reduce(_ || _)))
    val hitIdxVec     = VecInit(hitMask.map(OHToUInt(_)))
    val hitPortIdx    = OHToUInt(hitPortVec)
    val hitNotWritten = hit && needWrite(hitPortIdx)(hitIdxVec(hitPortIdx))
    val hitWritten    = hit && !needWrite(hitPortIdx)(hitIdxVec(hitPortIdx))
    dontTouch(hitPortVec)
    dontTouch(hitIdxVec)
    when(writeValid) {
      // if the entry is not written, it is useful
      val notUsefulVec = needWrite(portIdx).map(!_)
      val notUseful    = notUsefulVec.reduce(_ || _)
      val notUsefulIdx = PriorityEncoder(notUsefulVec)
      val victim       = Mux(notUseful, notUsefulIdx, replacerWay(portIdx))
      // if this wirte port !hit need to write a new entry
      when(!hit) {
        entries(portIdx)(victim)    := io.write(portIdx).bits
        valids(portIdx)(victim)     := true.B
        needWrite(portIdx)(victim)  := true.B
        writeTouchVec(victim).valid := true.B
        writeTouchVec(victim).bits  := victim
      }
      // if hit need to update the entry
      for (entryRow <- 0 until numPorts) {
        val hitPort = hitPortVec(entryRow)
        val hitIdx  = hitIdxVec(entryRow)
        hitTouchVec(entryRow)(hitIdx).valid := hitPort
        hitTouchVec(entryRow)(hitIdx).bits  := hitIdx

        val hitNotWrittenEntry = hitPort && needWrite(entryRow)(hitIdx)
        val hitWrittenEntry    = hitPort && !needWrite(entryRow)(hitIdx)
        when(hitPort) {
          when(hitNotWrittenEntry) {
            entries(entryRow)(hitIdx) := io.write(portIdx).bits
            valids(entryRow)(hitIdx)  := true.B
          }.elsewhen(hitWrittenEntry) {
            val entryChange = entries(entryRow)(hitIdx).asUInt =/= io.write(portIdx).bits.asUInt
            when(entryChange) {
              needWrite(entryRow)(hitIdx) := true.B
              entries(entryRow)(hitIdx)   := io.write(portIdx).bits
              valids(entryRow)(hitIdx)    := true.B
            }
          }
        }
        // If the write request has conuter need update
        val temporarily = WireInit(io.write(portIdx).bits)
        if (hasCnt) {
          when(hitPort) {
            // If a write request hits an entry, update its counter using 'taken'
            // and write other entry information
            val writePortTaken = io.write(portIdx).bits.taken.getOrElse(false.B)
            val updateCnt      = entries(entryRow)(hitIdx).cnt.get.getUpdate(writePortTaken)
            temporarily.cnt.get       := updateCnt.asTypeOf(temporarily.cnt.get)
            entries(entryRow)(hitIdx) := temporarily
            valids(entryRow)(hitIdx)  := true.B
            // If the write port hit a written entry, update the needWrite
            needWrite(entryRow)(hitIdx) := true.B
          }
        }
      }
    }
    XSPerfAccumulate(f"port${portIdx}_hit_not_written", writeValid && hitNotWritten)
    XSPerfAccumulate(f"port${portIdx}_hit_written", writeValid && hitWritten)
    XSPerfAccumulate(f"port${portIdx}_hit", writeValid && hit)
    XSPerfAccumulate(f"port${portIdx}_not_hit", writeValid && !hit)

  }

  for (nRows <- 0 until numPorts) {
    val replacer = ReplacementPolicy.fromString("plru", numEntries)
    readReadyVec(nRows) := io.read(nRows).ready
    readValidVec(nRows) := needWrite(nRows)
    emptyVec(nRows)     := !readValidVec(nRows).reduce(_ || _)
    fullVec(nRows)      := readValidVec(nRows).reduce(_ && _)
    val readIdx = PriorityEncoder(readValidVec(nRows))

    io.write(nRows).ready := !fullVec(nRows)
    io.read(nRows).valid  := !emptyVec(nRows)
    io.read(nRows).bits   := DontCare

    when(readReadyVec(nRows) && !emptyVec(nRows)) {
      io.read(nRows).bits       := entries(nRows)(readIdx)
      needWrite(nRows)(readIdx) := false.B
    }
    val touchWays = Seq(writeTouchVec(nRows)) ++ hitTouchVec(nRows).filter(_.valid == true.B).take(numPorts)
    replacerWay(nRows) := replacer.way
    replacer.access(touchWays)
    when(flush) {
      // Reset the write buffer needWrite when flush is true
      for (i <- 0 until numEntries) {
        needWrite(nRows)(i) := false.B
      }
    }
    XSPerfAccumulate(f"port${nRows}_is_full", writePortValid(nRows) && fullVec(nRows))
    XSPerfHistogram(f"port${nRows}_useful", PopCount(readValidVec(nRows)), readReadyVec(nRows), 0, numEntries)
  }
}
