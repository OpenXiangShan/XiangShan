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
import utility.XSError
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import utility.XSPerfSeqAccumulate
import xiangshan.frontend.ftq.BpuFlushInfo
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.frontend.ftq.FtqToWayLookupBundle

class ICacheWayLookup(implicit p: Parameters) extends ICacheModule
    with ICacheMissUpdateHelper
    with HasCircularQueuePtrHelper {

  class ICacheWayLookupIO(implicit p: Parameters) extends ICacheBundle {
    val flush:        Bool         = Input(Bool())
    val flushFromBpu: BpuFlushInfo = Input(new BpuFlushInfo)

    val fromFtq: DecoupledIO[FtqToWayLookupBundle] = Flipped(Decoupled(new FtqToWayLookupBundle))
    val toFtq:   WayLookupToFtqBundle              = Output(new WayLookupToFtqBundle)

    val toMainPipe: DecoupledIO[WayLookupToMainPipeBundle] = DecoupledIO(new WayLookupToMainPipeBundle)

    val write: Vec[DecoupledIO[WayLookupWriteBundle]] = Vec(FetchPorts, Flipped(DecoupledIO(new WayLookupWriteBundle)))

    val update: Valid[MissRespBundle] = Flipped(ValidIO(new MissRespBundle))

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

  private val numValidEntries = distanceBetween(writePtr, readPtr)
  private val numFreeEntries  = WayLookupSize.U - numValidEntries
  dontTouch(numValidEntries)
  dontTouch(numFreeEntries)

  // NOTE: May be unportable, we have bp3 == pf2 now, and WayLookup is written in pf1,
  // so the tailing 0 (already bypassed to if1) or 1 (if1 stall, stored here) entries might be flushed by bp3,
  // therefore, when shouldFlushByStage3, we need to move back writePtr by 0 (empty) or 1.
  // If in future we have bp4 (or even more) flush, this might not be enough.
  // NOTE: With 2-prefetch, writePtr - 2.U still does not need to be flushed,
  // as we ask the second fetch block to be flushed within Ftq. Refer to `canTwoPrefetch` condition in `class Ftq`
  private val bpuS3FlushValid = io.flushFromBpu.shouldFlushByStage3(tailFtqIdx, true.B)
  private val bpuS3FlushPtr   = writePtr - 1.U

  when(io.flush) {
    writePtr.value := 0.U
    writePtr.flag  := false.B
  }.elsewhen(bpuS3FlushValid && !empty) {
    writePtr := bpuS3FlushPtr
  }.elsewhen(io.write.head.fire) {
    // FetchPorts must be 1 or 2, and last.fire is depend on head.fire
    val enqCnt = Mux(io.write.last.fire, FetchPorts.U, 1.U)
    writePtr := writePtr + enqCnt
  }

  when(io.flush) {
    readPtr.value := 0.U
    readPtr.flag  := false.B
  }.elsewhen(io.toMainPipe.fire) {
    readPtr := readPtr + Mux(io.toFtq.realTwoFetchValid, 2.U, 1.U)
  }

  when(io.flush) {
    tailFtqIdx.value := 0.U
    tailFtqIdx.flag  := false.B
  }.elsewhen(io.write.head.fire) {
    tailFtqIdx := Mux(io.write.last.fire, io.write.last.bits.ftqIdx, io.write.head.bits.ftqIdx)
  }

  // we can store only the first exception encountered, as exceptions must trigger a redirection (and thus a flush)
  private val exceptionEntry = RegInit(0.U.asTypeOf(Valid(new WayLookupExceptionEntry)))
  private val exceptionPtr   = RegInit(ICacheWayLookupPtr(false.B, 0.U))
  private val exceptionHit = VecInit((0 until MaxFetchReqNum).map { i =>
    exceptionPtr === (readPtr + i.U) && exceptionEntry.valid
  })

  when(io.flush || bpuS3FlushValid && exceptionPtr === bpuS3FlushPtr) {
    // When flushed by bp3
    // we don't need to reset exceptionEntry/Ptr to save power
    exceptionEntry.valid := false.B
  }

  /* *** update *** */
  private val entryUpdate = VecInit(entries.map { entry =>
    (0 until PortNumber).map { i =>
      val (updated, newInfo) = updateMetaInfo(
        io.update,
        entry.vSetIdx(i),
        entry.pTag,
        entry.getMetaInfo(i)
      )
      when(updated) {
        entry.updateMetaInfo(i, newInfo)
      }
      updated
    }.reduce(_ || _)
  })
  // if the entry is being updated, we should not read it (i.e. read.valid should be false)
  private val updateStall = VecInit((0 until MaxFetchReqNum).map(i => entryUpdate((readPtr + i.U).value)))

  /* *** read *** */
  private val secondWriteValid = if (FetchPorts == 1) false.B else io.write(1).valid
  private val fetchReq         = io.fromFtq.bits.req

  // if WayLookup is empty, but there is a valid write, we can bypass one to read port (maybe timing critical)
  private val canBypass =
    empty && io.write(0).valid && !secondWriteValid && !fetchReq(1).valid && !exceptionEntry.valid

  private val canDeq       = !empty && !updateStall(0)
  private val canDeqSecond = numValidEntries > 1.U && !updateStall(0) && !updateStall(1)

  private val canServe = canBypass || canDeq

  private val fetchReqEntry = VecInit((0 until MaxFetchReqNum).map(i => entries((readPtr + i.U).value)))
  private val fetchReqExceptionEntry = VecInit((0 until MaxFetchReqNum).map { i =>
    Mux(exceptionHit(i), exceptionEntry.bits, 0.U.asTypeOf(new WayLookupExceptionEntry))
  })

  private val isDataSramReadConflict = (0 until DataBanks).map { bankIdx =>
    val reqReadInfo = (0 until MaxFetchReqNum).map { i =>
      val lineSelOH = Cat(fetchReq(i).bankSel(1)(bankIdx), fetchReq(i).bankSel(0)(bankIdx))
      val readValid = lineSelOH.orR && fetchReq(i).valid
      val wayMask   = Mux1H(lineSelOH, fetchReqEntry(i).waymask) & Fill(nWays, readValid)
      val setIdx    = Mux1H(lineSelOH, fetchReq(i).vSetIdx)
      (readValid, wayMask, setIdx)
    }

    reqReadInfo.map(_._1).reduce(_ && _) &&    // read same bank
    reqReadInfo(0)._2 === reqReadInfo(1)._2 && // read same way
    reqReadInfo(0)._3 =/= reqReadInfo(1)._3    // read different set
  }.reduce(_ || _)

  private val hasMmio          = fetchReqEntry.map(_.isMmio).reduce(_ || _)
  private val hasItlbException = exceptionHit.reduce(_ || _)

  private val realTwoFetchValid = fetchReq(1).valid && canDeqSecond &&
    !isDataSramReadConflict && !hasMmio && !hasItlbException

  io.toFtq.realTwoFetchValid         := realTwoFetchValid
  io.toFtq.perf_canNotServeTwoMeta   := !canDeqSecond
  io.toFtq.perf_dataSramReadConflict := isDataSramReadConflict
  io.toFtq.perf_hasMmio              := hasMmio
  io.toFtq.perf_hasItlbException     := hasItlbException

  io.fromFtq.ready := io.toMainPipe.ready && canServe && !io.flush

  io.toMainPipe.valid             := io.fromFtq.valid && canServe && !io.flush
  io.toMainPipe.bits.req          := fetchReq
  io.toMainPipe.bits.req(1).valid := realTwoFetchValid

  io.toMainPipe.bits.wayLookupInfo(0).entry := Mux(
    canBypass,
    io.write(0).bits.entry,
    fetchReqEntry(0)
  )
  io.toMainPipe.bits.wayLookupInfo(0).exceptionEntry := Mux(
    canBypass,
    io.write(0).bits.exceptionEntry,
    fetchReqExceptionEntry(0)
  )
  io.toMainPipe.bits.wayLookupInfo(1).entry          := fetchReqEntry(1)
  io.toMainPipe.bits.wayLookupInfo(1).exceptionEntry := fetchReqExceptionEntry(1)

  if (!env.FPGAPlatform) {
    when(io.toMainPipe.fire) {
      assert(io.toMainPipe.bits.wayLookupInfo(0).entry.debug_ftqIdx.get === fetchReq(0).ftqIdx)
      assert(io.toMainPipe.bits.wayLookupInfo(0).entry.debug_startVAddr.get === fetchReq(0).startVAddr)
      when(realTwoFetchValid) {
        assert(io.toMainPipe.bits.wayLookupInfo(1).entry.debug_ftqIdx.get === fetchReq(1).ftqIdx)
        assert(io.toMainPipe.bits.wayLookupInfo(1).entry.debug_startVAddr.get === fetchReq(1).startVAddr)
      }
    }
  }

  /**
    ******************************************************************************
    * write
    ******************************************************************************
    */
  // stall write if there is an exceptions to save power (i.e. wait for flush)
  // this will stall the prefetch pipe
  // also we disallow only 1 ready, to simplify PrefetchPipe
  io.write.foreach(_.ready := numFreeEntries >= FetchPorts.U && !exceptionEntry.valid)
  when(io.write.head.fire) {
    entries(writePtr.value) := io.write.head.bits.entry
    if (FetchPorts > 1) {
      when(io.write.last.fire) {
        entries(writePtr.value + 1.U) := io.write.last.bits.entry
      }
    }
    // ftq/prefetchPipe ensure fetch blocks has the same exception, here we can consider only .head
    when(io.write.head.bits.itlbException.hasException) {
      exceptionEntry.valid := true.B
      exceptionEntry.bits  := io.write.head.bits.exceptionEntry
      exceptionPtr         := writePtr
    }
  }
  // the second port (if FetchPorts == 2) must fire together with the first port
  XSError(io.write.last.fire && !io.write.head.fire, "2-prefetch port fire without first port fire")

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
  XSPerfAccumulate("write", io.write.head.fire)
  XSPerfAccumulate("empty_when_write", empty && io.write.head.fire)
  XSPerfAccumulate("bypass", empty && io.write.head.fire && canBypass && io.toMainPipe.fire)
  // exception stall cycles
  XSPerfAccumulate("waitingForExceptionRead", exceptionEntry.valid && !empty)
  XSPerfAccumulate("waitingForExceptionFlush", exceptionEntry.valid && empty)
}
