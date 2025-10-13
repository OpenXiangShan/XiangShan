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
import utility.HasCircularQueuePtrHelper
import utility.XSPerfHistogram
import xiangshan.frontend.ftq.BpuFlushInfo
import xiangshan.frontend.ftq.FtqPtr

class ICacheWayLookup(implicit p: Parameters) extends ICacheModule
    with ICacheMissUpdateHelper
    with HasCircularQueuePtrHelper {

  class ICacheWayLookupIO(implicit p: Parameters) extends ICacheBundle {
    val flush:        Bool                              = Input(Bool())
    val flushFromBpu: BpuFlushInfo                      = Input(new BpuFlushInfo)
    val read:         DecoupledIO[WayLookupBundle]      = DecoupledIO(new WayLookupBundle)
    val write:        DecoupledIO[WayLookupWriteBundle] = Flipped(DecoupledIO(new WayLookupWriteBundle))
    val update:       Valid[MissRespBundle]             = Flipped(ValidIO(new MissRespBundle))

    val perf: WayLookupPerfInfo = Output(new WayLookupPerfInfo)
  }

  val io: ICacheWayLookupIO = IO(new ICacheWayLookupIO)

  class ICacheWayLookupPtr extends CircularQueuePtr[ICacheWayLookupPtr](WayLookupSize)
  private object ICacheWayLookupPtr {
    def apply(f: Bool, v: UInt): ICacheWayLookupPtr = {
      val ptr = Wire(new ICacheWayLookupPtr)
      ptr.flag  := f
      ptr.value := v
      ptr
    }
  }

  private val entries  = RegInit(VecInit(Seq.fill(WayLookupSize)(0.U.asTypeOf(new WayLookupEntry))))
  private val readPtr  = RegInit(ICacheWayLookupPtr(false.B, 0.U))
  private val writePtr = RegInit(ICacheWayLookupPtr(false.B, 0.U))

  private val tailFtqIdx = RegInit(0.U.asTypeOf(new FtqPtr))

  private val empty = readPtr === writePtr
  private val full  = (readPtr.value === writePtr.value) && (readPtr.flag ^ writePtr.flag)

  // NOTE: May be unportable, we have bp3 == pf2 now, and WayLookup is written in pf1,
  // so the tailing 0 (already bypassed to if1) or 1 (if1 stall, stored here) entries might be flushed by bp3,
  // therefore, when shouldFlushByStage3, we need to move back writePtr by 0 (empty) or 1.
  // If in future we have bp4 (or even more) flush, this might not be enough.
  private val bpuS3FlushValid = io.flushFromBpu.shouldFlushByStage3(tailFtqIdx, !empty)
  private val bpuS3FlushPtr   = writePtr - 1.U

  when(io.flush) {
    writePtr.value := 0.U
    writePtr.flag  := false.B
  }.elsewhen(bpuS3FlushValid) {
    writePtr := bpuS3FlushPtr
  }.elsewhen(io.write.fire) {
    writePtr := writePtr + 1.U
  }

  when(io.flush) {
    readPtr.value := 0.U
    readPtr.flag  := false.B
  }.elsewhen(io.read.fire) {
    readPtr := readPtr + 1.U
  }

  when(io.flush) {
    tailFtqIdx.value := 0.U
    tailFtqIdx.flag  := false.B
  }.elsewhen(io.write.fire) {
    tailFtqIdx := io.write.bits.ftqIdx
  }

  private val gpfEntry = RegInit(0.U.asTypeOf(Valid(new WayLookupGpfEntry)))
  private val gpfPtr   = RegInit(ICacheWayLookupPtr(false.B, 0.U))
  private val gpfHit   = gpfPtr === readPtr && gpfEntry.valid

  when(io.flush || bpuS3FlushValid && gpfPtr === bpuS3FlushPtr) {
    // When flushed by bp3
    // we don't need to reset gpfPtr, since the valid is actually gpf_entries.excp_tlb_gpf
    gpfEntry.valid := false.B
    gpfEntry.bits  := 0.U.asTypeOf(new WayLookupGpfEntry)
  }

  /* *** update *** */
  private val entryUpdate = VecInit(entries.map { entry =>
    (0 until PortNumber).map { i =>
      val (updated, newMask, newMaybeRvcMap, newCode) = updateMetaInfo(
        io.update,
        entry.waymask(i),
        entry.vSetIdx(i),
        entry.pTag,
        entry.maybeRvcMap(i),
        entry.metaCodes(i)
      )
      when(updated) {
        entry.waymask(i)     := newMask
        entry.maybeRvcMap(i) := newMaybeRvcMap
        entry.metaCodes(i)   := newCode
      }
      updated
    }.reduce(_ || _)
  })
  // if the entry is being updated, we should not read it (i.e. read.valid should be false)
  private val updateStall = entryUpdate(readPtr.value)

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
    when(io.write.bits.itlbException.isGpf) {
      // if gpfEntry is bypassed, we don't need to save it
      // note this will override the read (L156)
      gpfEntry.valid := !(canBypass && io.read.fire)
      gpfEntry.bits  := io.write.bits.gpf
      gpfPtr         := writePtr
    }
  }

  /* *** perf *** */
  // tell ICache top if queue is empty
  io.perf.empty := empty

  // perf counter
  // occupancy
  XSPerfHistogram(
    "occupiedEntryCnt",
    distanceBetween(writePtr, readPtr),
    true.B,
    0,
    WayLookupSize
  )
}
