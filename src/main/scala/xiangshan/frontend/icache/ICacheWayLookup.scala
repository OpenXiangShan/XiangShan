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
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import utils.EnumUInt
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
  private val bpuS3FlushValid = io.flushFromBpu.shouldFlushByStage3(tailFtqIdx, true.B)
  private val bpuS3FlushPtr   = writePtr - 1.U

  when(io.flush) {
    writePtr.value := 0.U
    writePtr.flag  := false.B
  }.elsewhen(bpuS3FlushValid && !empty) {
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

  // we can store only the first exception encountered, as exceptions must trigger a redirection (and thus a flush)
  private val exceptionEntry = RegInit(0.U.asTypeOf(Valid(new WayLookupExceptionEntry)))
  private val exceptionPtr   = RegInit(ICacheWayLookupPtr(false.B, 0.U))
  private val exceptionHit   = exceptionPtr === readPtr && exceptionEntry.valid

  when(io.flush || bpuS3FlushValid && exceptionPtr === bpuS3FlushPtr) {
    // When flushed by bp3
    // we don't need to reset exceptionEntry/Ptr to save power
    exceptionEntry.valid := false.B
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
  private val canBypass = empty && io.write.valid && !exceptionEntry.valid
  private val canRead   = !empty && !updateStall
  io.read.valid := canRead || canBypass
  when(canBypass) {
    io.read.bits := io.write.bits
  }.otherwise {
    io.read.bits.entry          := entries(readPtr.value)
    io.read.bits.exceptionEntry := Mux(exceptionHit, exceptionEntry.bits, 0.U.asTypeOf(new WayLookupExceptionEntry))
  }

  /**
    ******************************************************************************
    * write
    ******************************************************************************
    */
  // stall write if there is an exceptions to save power (i.e. wait for flush)
  // this will stall the prefetch pipe
  io.write.ready := !full && !exceptionEntry.valid
  when(io.write.fire) {
    entries(writePtr.value) := io.write.bits.entry
    when(io.write.bits.exception.hasException) {
      exceptionEntry.valid := true.B
      exceptionEntry.bits  := io.write.bits.exceptionEntry
      exceptionPtr         := writePtr
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
  // exception stall cycles
  XSPerfAccumulate("waitingForExceptionRead", exceptionEntry.valid && !empty)
  XSPerfAccumulate("waitingForExceptionFlush", exceptionEntry.valid && empty)
}
