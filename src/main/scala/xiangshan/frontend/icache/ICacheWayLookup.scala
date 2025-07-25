// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.CircularQueuePtr
import xiangshan.frontend.ExceptionType

class ICacheWayLookup(implicit p: Parameters) extends ICacheModule with ICacheMissUpdateHelper {

  class ICacheWayLookupIO(implicit p: Parameters) extends ICacheBundle {
    val flush:  Bool                         = Input(Bool())
    val read:   DecoupledIO[WayLookupBundle] = DecoupledIO(new WayLookupBundle)
    val write:  DecoupledIO[WayLookupBundle] = Flipped(DecoupledIO(new WayLookupBundle))
    val update: Valid[MissRespBundle]        = Flipped(ValidIO(new MissRespBundle))
  }

  val io: ICacheWayLookupIO = IO(new ICacheWayLookupIO)

  class ICacheWayLookupPtr extends CircularQueuePtr[ICacheWayLookupPtr](nWayLookupSize)
  private object ICacheWayLookupPtr {
    def apply(f: Bool, v: UInt): ICacheWayLookupPtr = {
      val ptr = Wire(new ICacheWayLookupPtr)
      ptr.flag  := f
      ptr.value := v
      ptr
    }
  }

  private val entries  = RegInit(VecInit(Seq.fill(nWayLookupSize)(0.U.asTypeOf(new WayLookupEntry))))
  private val readPtr  = RegInit(ICacheWayLookupPtr(false.B, 0.U))
  private val writePtr = RegInit(ICacheWayLookupPtr(false.B, 0.U))

  private val empty = readPtr === writePtr
  private val full  = (readPtr.value === writePtr.value) && (readPtr.flag ^ writePtr.flag)

  when(io.flush) {
    writePtr.value := 0.U
    writePtr.flag  := false.B
  }.elsewhen(io.write.fire) {
    writePtr := writePtr + 1.U
  }

  when(io.flush) {
    readPtr.value := 0.U
    readPtr.flag  := false.B
  }.elsewhen(io.read.fire) {
    readPtr := readPtr + 1.U
  }

  private val gpfEntry = RegInit(0.U.asTypeOf(Valid(new WayLookupGpfEntry)))
  private val gpfPtr   = RegInit(ICacheWayLookupPtr(false.B, 0.U))
  private val gpfHit   = gpfPtr === readPtr && gpfEntry.valid

  when(io.flush) {
    // we don't need to reset gpfPtr, since the valid is actually gpf_entries.excp_tlb_gpf
    gpfEntry.valid := false.B
    gpfEntry.bits  := 0.U.asTypeOf(new WayLookupGpfEntry)
  }

  /* *** update *** */
  private val entryUpdate = VecInit(entries.map { entry =>
    (0 until PortNumber).map { i =>
      val (updated, newMask, newCode) = updateMetaInfo(
        io.update,
        entry.waymask(i),
        entry.vSetIdx(i),
        entry.pTag(i),
        entry.metaCodes(i)
      )
      when(updated) {
        entry.waymask(i)   := newMask
        entry.metaCodes(i) := newCode
      }
      updated
    }.reduce(_ || _)
  })
  // if the entry is being updated, we should not read it (i.e. read.valid should be false)
  // NOTE: this is not necessary in current design:
  //       when update is valid, DataArray is refilling at the same cycle, so DataArray.read.req.ready will be false.B
  //       then, MainPipe will get s0_canGo (= toData.last.ready && fromWayLookup.valid && s1_ready) = false.B
  //       then, WayLookup.read.ready (= s0_fire = s0_valid && s0_canGo && !s0_flush) will be false.B
  //       so, even if read.valid is true, read.fire will still be false.B, and no read happens.
//  private val updateStall = entryUpdate(readPtr.value)
  //       currently, we use a simple assertion to avoid using extra logic
  assert(
    !(!empty && io.read.fire && entryUpdate(readPtr.value)),
    "WayLookup read should not happen when entry is being updated"
  )
  private val updateStall = false.B

  /* *** read *** */
  // if the entry is empty, but there is a valid write, we can bypass it to read port (maybe timing critical)
  private val canBypass = empty && io.write.valid
  private val canRead   = !empty && !updateStall
  io.read.valid := canRead || canBypass
  when(canBypass) {
    io.read.bits := io.write.bits
  }.otherwise { // can't bypass
    io.read.bits.entry := entries(readPtr.value)
    when(gpfHit) { // ptr match && entry valid
      io.read.bits.gpf := gpfEntry.bits
      // also clear gpfEntry.valid when it's read, note this will be overridden by write (L175)
      when(io.read.fire) {
        gpfEntry.valid := false.B
      }
    }.otherwise { // gpf not hit
      io.read.bits.gpf := 0.U.asTypeOf(new WayLookupGpfEntry)
    }
  }

  /**
    ******************************************************************************
    * write
    ******************************************************************************
    */
  // if there is a valid gpf to be read, we should stall write
  private val gpfStall = gpfEntry.valid && !(io.read.fire && gpfHit)
  io.write.ready := !full && !gpfStall
  when(io.write.fire) {
    entries(writePtr.value) := io.write.bits.entry
    when(io.write.bits.itlbException.map(_.isGpf).reduce(_ || _)) {
      // if gpfEntry is bypassed, we don't need to save it
      // note this will override the read (L156)
      gpfEntry.valid := !(canBypass && io.read.fire)
      gpfEntry.bits  := io.write.bits.gpf
      gpfPtr         := writePtr
    }
  }
}
