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

package xiangshan.frontend.bpu.ittage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.ReplacementPolicy
import utility.XSError
import utility.XSPerfAccumulate

class RegionWays(implicit p: Parameters) extends IttageModule {
  class RegionWaysIO extends Bundle {
    val reqPointer: Vec[UInt] = Input(Vec(NumTables, UInt(log2Ceil(RegionNums).W)))
    val respHit:    Vec[Bool] = Output(Vec(NumTables, Bool()))
    val respRegion: Vec[UInt] = Output(Vec(NumTables, UInt(RegionBits.W)))

    val updateRegion:  Vec[UInt] = Input(Vec(RegionPorts, UInt(RegionBits.W)))
    val updateHit:     Vec[Bool] = Output(Vec(RegionPorts, Bool()))
    val updatePointer: Vec[UInt] = Output(Vec(RegionPorts, UInt(log2Ceil(RegionNums).W)))

    val writeValid:   Bool = Input(Bool())
    val writeRegion:  UInt = Input(UInt(RegionBits.W))
    val writePointer: UInt = Output(UInt(log2Ceil(RegionNums).W))
  }

  val io: RegionWaysIO = IO(new RegionWaysIO)

  private class RegionEntry(implicit p: Parameters) extends IttageBundle {
    val valid:  Bool = Bool()
    val region: UInt = UInt(RegionBits.W)
  }

  private val regions           = RegInit(VecInit(Seq.fill(RegionNums)(0.U.asTypeOf(new RegionEntry))))
  private val replacer          = ReplacementPolicy.fromString(RegionReplacer, RegionNums)
  private val replacerTouchWays = Wire(Vec(1, Valid(UInt(log2Ceil(RegionNums).W))))

  private val valids = VecInit((0 until RegionNums).map(w => regions(w).valid)).asUInt
  private val valid  = WireInit(valids.andR)
  // write data
  private val writeTotalHits =
    VecInit((0 until RegionNums).map(w => regions(w).region === io.writeRegion && regions(w).valid))
  private val writeHit = writeTotalHits.reduce(_ || _)
  private val writePointer =
    Mux(writeHit, OHToUInt(writeTotalHits), Mux(!valid, PriorityEncoder(~valids), replacer.way))
  XSError(PopCount(writeTotalHits) > 1.U, "region has multiple hits!\n")
  XSPerfAccumulate("Region_entry_replace", !writeHit && valid && io.writeValid)

  io.writePointer := writePointer
  // read and metaTarget update read ports
  for (i <- 0 until NumTables) {
    // read region use pointer
    io.respHit(i)    := regions(io.reqPointer(i)).valid
    io.respRegion(i) := regions(io.reqPointer(i)).region
  }

  for (i <- 0 until RegionPorts) {
    // When using metaTarget for updates, redefine the pointer
    val updateTotalHits =
      VecInit((0 until RegionNums).map(w => regions(w).region === io.updateRegion(i) && regions(w).valid))
    val updateBypass  = (io.updateRegion(i) === io.writeRegion) && io.writeValid
    val updateHit     = updateTotalHits.reduce(_ || _) || updateBypass
    val updatePointer = Mux(updateBypass, writePointer, OHToUInt(updateTotalHits))
    io.updateHit(i)     := updateHit
    io.updatePointer(i) := updatePointer
    XSError(PopCount(updateTotalHits) > 1.U, "region has multiple hits!\n")
  }
  // write
  when(io.writeValid) {
    when(!regions(writePointer).valid) {
      regions(writePointer).valid := true.B
    }
    regions(writePointer).region := io.writeRegion
  }
  replacerTouchWays(0).valid := io.writeValid
  replacerTouchWays(0).bits  := writePointer
  replacer.access(replacerTouchWays)
}
