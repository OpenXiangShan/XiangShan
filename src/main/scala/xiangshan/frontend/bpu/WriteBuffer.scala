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
 * @param hasFlush Whether the write buffer has a flush signal, used to reset the write buffer
 * @param nameSuffix Suffix of name, used for clearer logging
*/
class WriteBuffer[T <: WriteReqBundle](
    gen:        T,
    numEntries: Int = 1,
    numPorts:   Int = 1,
    hasCnt:     Boolean = false,
    hasFlush:   Boolean = false,
    nameSuffix: String = ""
)(implicit p: Parameters) extends XSModule {
  require(numEntries >= 0)
  require(numPorts >= 1)
  require(numPorts <= numEntries)
  class WriteBufferIO extends Bundle {
    val write:     Vec[DecoupledIO[T]] = Vec(numPorts, Flipped(DecoupledIO(gen)))
    val read:      Vec[DecoupledIO[T]] = Vec(numPorts, DecoupledIO(gen))
    val takenMask: Option[Vec[Bool]]   = Option.when(hasCnt)(Vec(numPorts, Input(Bool())))
    val flush:     Option[Bool]        = Option.when(hasFlush)(Input(Bool()))
  }
  val io: WriteBufferIO = IO(new WriteBufferIO)

  private val namePrefix = s"WriteBuffer_${nameSuffix}"

  // clean write buffer when flush is true
  private val flush = io.flush.getOrElse(false.B)

  private val needWrite = RegInit(VecInit(Seq.fill(numPorts)(VecInit(Seq.fill(numEntries)(false.B)))))
  private val entries = RegInit(VecInit(Seq.fill(numPorts)(VecInit(Seq.fill(numEntries)(0.U.asTypeOf(gen.cloneType))))))
  private val valids  = RegInit(VecInit(Seq.fill(numPorts)(VecInit(Seq.fill(numEntries)(false.B)))))

  private val writePortValid = VecInit(Seq.fill(numPorts)(false.B))
  private val writePortBits  = VecInit(Seq.fill(numPorts)(0.U.asTypeOf(gen.cloneType)))
  // Record the hitTouch situation for each row of all write requests
  private val hitTouchVec =
    WireInit(VecInit.fill(numPorts)(VecInit.fill(numEntries)(0.U.asTypeOf(Valid(UInt(log2Ceil(numEntries).W))))))
  private val writeTouchVec = WireInit(VecInit.fill(numPorts)(0.U.asTypeOf(Valid(UInt(log2Ceil(numEntries).W)))))

  private val replacerWay  = Wire(Vec(numPorts, UInt(log2Ceil(numEntries).W)))
  private val readValidVec = WireInit(VecInit.fill(numPorts)(VecInit.fill(numEntries)(false.B)))
  private val readReadyVec = WireInit(VecInit(Seq.fill(numPorts)(false.B)))
  private val emptyVec     = WireInit(VecInit(Seq.fill(numPorts)(false.B)))
  private val fullVec      = WireInit(VecInit(Seq.fill(numPorts)(false.B)))
  private val writeFlowVec = WireInit(VecInit(Seq.fill(numPorts)(false.B)))

  writePortValid := io.write.map(_.valid)
  writePortBits  := io.write.map(_.bits)
  dontTouch(readValidVec)
  dontTouch(replacerWay)
  dontTouch(emptyVec)

  // not allowed to write entries with same setIdx and tag
  for {
    i <- 0 until numPorts
    j <- i + 1 until numPorts
  } {
    val writeSameEntry =
      writePortValid(i) && writePortValid(j) && writePortBits(i).setIdx === writePortBits(j).setIdx &&
        writePortBits(i).tag.getOrElse(0.U) === writePortBits(j).tag.getOrElse(0.U)
    XSError(
      writeSameEntry,
      f"WriteBuffer cannot write same data simultaneously, WriteBuffer_$nameSuffix port$i and $j violated"
    )
  }

  /**
     * Write request processing cases:
     * 1. Write req miss
     *  1.1 Write data into WriteBuffer
     * 2. Write req hit
     *  2.1 Hit occurs on a useful entry update the entry
     *  2.2 Hit occurs on a useless entry with data mismatch, triggers re-write
     *  2.3 Hit and hasCnt, update the entry's counter
    */
  writePortValid.zipWithIndex.foreach { case (writeValid, portIdx) =>
    // maintain hitMask for each write port
    val hitMask = VecInit.fill(numPorts)(VecInit.fill(numEntries)(false.B))
    for (p <- 0 until numPorts; e <- 0 until numEntries) {
      hitMask(p)(e) := writeValid && valids(p)(e) &&
        writePortBits(portIdx).setIdx === entries(p)(e).setIdx &&
        writePortBits(portIdx).tag.getOrElse(0.U) === entries(p)(e).tag.getOrElse(0.U)
    }
    // each write port can only hit at most one entry
    XSError(
      writeValid && PopCount(hitMask.flatten) > 1.U,
      f"WriteBuffer_$nameSuffix port${portIdx}_hitMask should be no more than 1"
    )

    val hit          = hitMask.flatten.reduce(_ || _)         // whether this write port hits any entry
    val hitRowsVec   = VecInit(hitMask.map(_.reduce(_ || _))) // Mark hit status of each row
    val hitRowIdxVec = VecInit(hitMask.map(OHToUInt(_)))      // if hit record which entry hit each line

    val rowIdx        = OHToUInt(hitRowsVec) // hitRow's idx
    val hitIdx        = hitRowIdxVec(rowIdx) // hit entry's idx
    val hitNotWritten = hit && needWrite(rowIdx)(hitIdx)
    val hitWritten    = hit && !needWrite(rowIdx)(hitIdx)
    dontTouch(hitRowsVec)
    dontTouch(hitRowIdxVec)

    when(writeValid) {
      // if the entry is not written, it is useful
      val notUsefulVec = needWrite(portIdx).map(!_)
      val notUseful    = notUsefulVec.reduce(_ || _)
      val notUsefulIdx = PriorityEncoder(notUsefulVec)
      val victim       = Mux(notUseful, notUsefulIdx, replacerWay(portIdx))
      // if this write port !hit need to write a new entry
      when(!hit) {
        entries(portIdx)(victim)     := io.write(portIdx).bits
        valids(portIdx)(victim)      := true.B
        needWrite(portIdx)(victim)   := true.B
        writeTouchVec(portIdx).valid := true.B
        writeTouchVec(portIdx).bits  := victim
      }

      // if hit need to update the entry
      when(hit) {
        hitTouchVec(rowIdx)(hitIdx).valid := hit
        hitTouchVec(rowIdx)(hitIdx).bits  := hitIdx
        when(hitNotWritten) {
          entries(rowIdx)(hitIdx) := io.write(portIdx).bits
          valids(rowIdx)(hitIdx)  := true.B
        }.elsewhen(hitWritten) {
          val entryChange = entries(rowIdx)(hitIdx).asUInt =/= io.write(portIdx).bits.asUInt
          when(entryChange) {
            needWrite(rowIdx)(hitIdx) := true.B
            entries(rowIdx)(hitIdx)   := io.write(portIdx).bits
            valids(rowIdx)(hitIdx)    := true.B
          }
        }
      }

      // If the write request has counter need update counter
      val temporarily = WireInit(io.write(portIdx).bits)
      if (hasCnt) {
        val takenMask = io.takenMask.getOrElse(VecInit.fill(numPorts)(false.B))
        when(hit) {
          // If a write request hits an entry, update its counter using 'taken'
          // and write other entry information
          val writePortTaken = takenMask(portIdx)
          val updateCnt      = entries(rowIdx)(hitIdx).cnt.get.getUpdate(writePortTaken)
          temporarily.cnt.get     := updateCnt.asTypeOf(temporarily.cnt.get)
          entries(rowIdx)(hitIdx) := temporarily
          valids(rowIdx)(hitIdx)  := true.B
          // If the write port hit a written entry, update the needWrite
          needWrite(rowIdx)(hitIdx) := true.B
        }
      }
    }
    XSPerfAccumulate(f"${namePrefix}_port${portIdx}_hit_not_written", writeValid && hitNotWritten)
    XSPerfAccumulate(f"${namePrefix}_port${portIdx}_hit_written", writeValid && hitWritten)
    XSPerfAccumulate(f"${namePrefix}_port${portIdx}_hit", writeValid && hit)
    XSPerfAccumulate(f"${namePrefix}_port${portIdx}_not_hit", writeValid && !hit)

  }

  //  read buffer entry
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
    XSPerfAccumulate(f"${namePrefix}_port${nRows}_is_full", writePortValid(nRows) && fullVec(nRows))
    XSPerfHistogram(
      f"${namePrefix}_port${nRows}_useful",
      PopCount(readValidVec(nRows)),
      readReadyVec(nRows),
      0,
      numEntries
    )
  }
}
