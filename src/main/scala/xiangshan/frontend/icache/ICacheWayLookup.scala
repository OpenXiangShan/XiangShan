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

class ICacheWayLookup(implicit p: Parameters) extends ICacheModule
    with ICacheMissUpdateHelper
    with HasCircularQueuePtrHelper {

  class ICacheWayLookupIO(implicit p: Parameters) extends ICacheBundle {
    val flush:  Bool                         = Input(Bool())
    val read:   DecoupledIO[WayLookupBundle] = DecoupledIO(new WayLookupBundle)
    val write:  DecoupledIO[WayLookupBundle] = Flipped(DecoupledIO(new WayLookupBundle))
    val update: Valid[MissRespBundle]        = Flipped(ValidIO(new MissRespBundle))

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

  // we can store only the first exception encountered, as exceptions must trigger a redirection (and thus a flush)
  // we use a fsm-like way to track the exception status
  private object ExceptionStatus extends EnumUInt(3) {
    // no exception encountered since last flush (or power-on/reset)
    // allow read/write/bypass
    def None: UInt = 0.U(width.W)
    // prefetchPipe has written an exception entry, waiting for mainPipe to read it
    // allow read, not write/bypass (to save power)
    def Written: UInt = 1.U(width.W)
    // mainPipe has read the exception entry, waiting for flush
    // disallow read/write/bypass
    def Read: UInt = 2.U(width.W)
  }

  private val exceptionStatus = RegInit(ExceptionStatus.None)
  private val exceptionEntry  = RegInit(0.U.asTypeOf(new WayLookupExceptionEntry))
  private val exceptionPtr    = RegInit(ICacheWayLookupPtr(false.B, 0.U))
  private val exceptionHit    = exceptionPtr === readPtr && exceptionStatus === ExceptionStatus.Written

  when(io.flush) {
    // we don't need to reset exceptionEntry/Ptr to save power
    exceptionStatus := ExceptionStatus.None
  }

  /* *** update *** */
  private val entryUpdate = VecInit(entries.map { entry =>
    (0 until PortNumber).map { i =>
      val (updated, newMask, newCode) = updateMetaInfo(
        io.update,
        entry.waymask(i),
        entry.vSetIdx(i),
        entry.pTag,
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
  private val canBypass = empty && io.write.valid && exceptionStatus === ExceptionStatus.None
  private val canRead   = !empty && !updateStall && exceptionStatus =/= ExceptionStatus.Read
  io.read.valid := canRead || canBypass
  when(canBypass) {
    io.read.bits := io.write.bits
  }.otherwise {
    io.read.bits.entry          := entries(readPtr.value)
    io.read.bits.exceptionEntry := Mux(exceptionHit, exceptionEntry, 0.U.asTypeOf(new WayLookupExceptionEntry))
    when(io.read.fire && exceptionHit) {
      exceptionStatus := ExceptionStatus.Read
    }
  }

  /**
    ******************************************************************************
    * write
    ******************************************************************************
    */
  // stall write if there is an exceptions to save power (i.e. wait for flush)
  // this will stall the prefetch pipe
  io.write.ready := !full && exceptionStatus === ExceptionStatus.None
  when(io.write.fire) {
    entries(writePtr.value) := io.write.bits.entry
    when(io.write.bits.itlbException.hasException) {
      // if is bypassed, goto ExceptionStatus.Read straight, otherwise goto ExceptionStatus.Written
      exceptionStatus := Mux(canBypass && io.read.fire, ExceptionStatus.Read, ExceptionStatus.Written)
      exceptionEntry  := io.write.bits.exceptionEntry
      exceptionPtr    := writePtr
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
  XSPerfAccumulate("waitingForExceptionRead", exceptionStatus === ExceptionStatus.Written)
  XSPerfAccumulate("waitingForExceptionFlush", exceptionStatus === ExceptionStatus.Read)
}
