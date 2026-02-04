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
import utility.ReplacementPolicy
import utility.XSError
import utility.XSPerfAccumulate
import utility.XSPerfHistogram

class MainBtbWriteBuffer(
    numEntries: Int = 1,
    numPorts:   Int = 1,
    nameSuffix: String = ""
)(implicit p: Parameters) extends MainBtbModule {
  require(numEntries >= 0)
  require(numPorts >= 1)
  require(numPorts <= numEntries)
  class MainBtbWriteBufferIO extends Bundle {
    val write: Vec[DecoupledIO[MainBtbWriteReq]] = Vec(numPorts, Flipped(Decoupled(new MainBtbWriteReq)))
    val read:  Vec[DecoupledIO[MainBtbWriteReq]] = Vec(numPorts, Decoupled(new MainBtbWriteReq))
    // used for prediction
    val probe: Vec[MainBtbWriteProbe] = Vec(numPorts, new MainBtbWriteProbe)
  }
  val io: MainBtbWriteBufferIO = IO(new MainBtbWriteBufferIO)

  private val namePrefix = s"MainBtbWriteBuffer_${nameSuffix}"

  private val needWrite = RegInit(VecInit(Seq.fill(numPorts)(VecInit(Seq.fill(numEntries)(false.B)))))
  private val entries =
    RegInit(VecInit(Seq.fill(numPorts)(VecInit(Seq.fill(numEntries)(0.U.asTypeOf(new MainBtbWriteReq))))))
  private val valids = RegInit(VecInit(Seq.fill(numPorts)(VecInit(Seq.fill(numEntries)(false.B)))))

  private val writePortValid = VecInit(Seq.fill(numPorts)(false.B))
  private val writePortBits  = VecInit(Seq.fill(numPorts)(0.U.asTypeOf(new MainBtbWriteReq)))
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
  // Replace is to prioritize replacing the entry of the same port as setIdx
  private val victimSameSetIdx = WireInit(VecInit.fill(numPorts)(0.U.asTypeOf(Valid(UInt(log2Ceil(numEntries).W)))))

  io.probe.zipWithIndex.foreach { case (port, portIdx) =>
    val readIdx   = PriorityEncoder(readValidVec(portIdx))
    val readValid = readValidVec(portIdx).reduce(_ || _)
    port.valid  := readValid
    port.fire   := readValid && readReadyVec(portIdx)
    port.setIdx := entries(portIdx)(readIdx).setIdx
    port.entry  := entries(portIdx)(readIdx).entry
    port.shared := entries(portIdx)(readIdx).shared
    port.status := entries(portIdx)(readIdx).status
  }

  def isEntryMatch(a: MainBtbWriteReq, b: MainBtbWriteReq): Bool = {
    val rawHit    = a.setIdx === b.setIdx
    val hasUpdate = a.status.isReqUpdate || b.status.isReqUpdate
    rawHit && Mux(
      hasUpdate,
      a.getUpdateTag === b.getUpdateTag,
      a.getOverrideTag === b.getOverrideTag
    )
  }

  writePortValid := io.write.map(_.valid)
  writePortBits  := io.write.map(_.bits)
  dontTouch(readValidVec)
  dontTouch(replacerWay)
  dontTouch(emptyVec)

  writePortValid.zipWithIndex.foreach { case (writeValid, portIdx) =>
    val setIdxHitVec = entries(portIdx).zip(valids(portIdx)).map { case (entry, valid) =>
      writeValid && valid && writePortBits(portIdx).setIdx === entry.setIdx
    }
    XSError(
      writeValid && PopCount(setIdxHitVec) > 1.U,
      f"${namePrefix}_port${portIdx}_setIdxHitVec should be no more than 1"
    )
    victimSameSetIdx(portIdx).valid := setIdxHitVec.reduce(_ || _)
    victimSameSetIdx(portIdx).bits  := OHToUInt(VecInit(setIdxHitVec))
  }

  // not allowed to write entries with same setIdx and tag
  for {
    i <- 0 until numPorts
    j <- i + 1 until numPorts
  } {
    val writeSameEntry =
      writePortValid(i) && writePortValid(j) && isEntryMatch(writePortBits(i), writePortBits(j))
    XSError(
      writeSameEntry,
      f"${namePrefix} cannot write same data simultaneously, ${namePrefix} port$i and $j violated"
    )
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

    XSPerfAccumulate(f"${namePrefix}_port${nRows}_is_full", writePortValid(nRows) && fullVec(nRows))
    XSPerfHistogram(
      f"${namePrefix}_port${nRows}_useful",
      PopCount(readValidVec(nRows)),
      readReadyVec(nRows),
      0,
      numEntries
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
      hitMask(p)(e) := writeValid && valids(p)(e) && isEntryMatch(writePortBits(portIdx), entries(p)(e))
    }
    // each write port can only hit at most one entry
    XSError(
      writeValid && PopCount(hitMask.flatten) > 1.U && writePortBits(portIdx).status.isReqOverride,
      f"${namePrefix}_port${portIdx}_hitMask should be no more than 1"
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
      val victim = Mux(
        victimSameSetIdx(portIdx).valid,
        victimSameSetIdx(portIdx).bits,
        Mux(notUseful, notUsefulIdx, replacerWay(portIdx))
      )
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

        val write        = io.write(portIdx).bits
        val entry        = entries(rowIdx)(hitIdx)
        val isValid      = valids(rowIdx)(hitIdx)
        val isNeedWrite  = needWrite(rowIdx)(hitIdx)
        val sharedChange = entry.shared.asUInt =/= write.shared.asUInt
        val wholeChange  = entry.asUInt =/= write.asUInt

        when(write.status.isReqUpdate && entry.status.isReqUpdate) {
          // Case 1: write and entry request update
          // - just update the shared info
          when(hitNotWritten) {
            entry.shared := write.shared
            isValid      := true.B
            when(sharedChange) {
              isNeedWrite := true.B
            }
          }.elsewhen(hitWritten && sharedChange) {
            entry.shared := write.shared
            isValid      := true.B
            isNeedWrite  := true.B
          }
        }.elsewhen(write.status.isReqUpdate && entry.status.isReqOverride) {
          // Case 2: write request update and entry request override
          // - update entry's shared info from write
          when(!entry.entry.attribute.isConditional) {
            // drop update write because non-conditional entry cannot be updated
            // do nothing
          }.elsewhen(hitNotWritten) {
            entry.shared := write.shared
            isValid      := true.B
            when(sharedChange) {
              isNeedWrite := true.B
            }
          }.elsewhen(hitWritten && sharedChange) {
            entry.shared           := write.shared
            entry.status.writeType := MainBtbWriteStatus.WriteType.Shared // only need to write shared info
            isValid                := true.B
            isNeedWrite            := true.B
          }
        }.elsewhen(write.status.isReqOverride && entry.status.isReqUpdate) {
          // Case 3: write request override and entry request update
          // - override entry with write
          entry       := write
          isValid     := true.B
          isNeedWrite := true.B
        }.otherwise {
          // Case 4: write and entry request override
          // - override entry with write
          when(hitNotWritten) {
            entry   := write
            isValid := true.B
            when(wholeChange) {
              isNeedWrite := true.B
            }
          }.elsewhen(hitWritten && wholeChange) {
            entry       := write
            isValid     := true.B
            isNeedWrite := true.B
          }
        }
      }
    }
    XSPerfAccumulate(f"${namePrefix}_port${portIdx}_hit_not_written", writeValid && hitNotWritten)
    XSPerfAccumulate(f"${namePrefix}_port${portIdx}_hit_written", writeValid && hitWritten)
    XSPerfAccumulate(f"${namePrefix}_port${portIdx}_hit", writeValid && hit)
    XSPerfAccumulate(f"${namePrefix}_port${portIdx}_not_hit", writeValid && !hit)

  }

}
