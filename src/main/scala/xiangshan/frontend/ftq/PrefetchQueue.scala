// Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.frontend.ftq

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.HasCircularQueuePtrHelper
import utility.XSError
import utility.XSPerfAccumulate
import xiangshan.SoftIPrefetchBundle
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.PcInit
import xiangshan.frontend.TwoPrefetchCase
import xiangshan.frontend.icache.PrefetchSource

class PrefetchQueue(implicit p: Parameters) extends FtqModule
    with HasFtqPrefetchQueueParameters
    with HasCircularQueuePtrHelper {
  class Entry extends Bundle {
    val vAddr:  GuardedPc      = GuardedPc()
    val source: PrefetchSource = new PrefetchSource
  }

  class PrefetchQueueIO extends Bundle {
    class Enqueue extends Entry {
      def :=(that: SoftIPrefetchBundle): Unit = {
        this.vAddr  := PcInit(that.vaddr).signGuard
        this.source := PrefetchSource.Sw
      }
    }

    class Dequeue extends FtqToPrefetchBundle {
      // we need to represent "MaxPrefetchReqNum" itself, so we need log2(...+1) width
      val fdipNum: UInt = UInt(log2Ceil(MaxPrefetchReqNum + 1).W)
    }

    // enqueue from non-FDIP sources
    val enq: Vec[DecoupledIO[Enqueue]] = Vec(EnqueueNum, Flipped(DecoupledIO(new Enqueue)))
    // convenient aliases
    def enqFromSw: Seq[DecoupledIO[Enqueue]] = enq.slice(0, EnqueueSwNum)

    // wired from ftq entryQueue to provide FDIP prefetch
    val fromEntryQueue: Vec[Valid[FtqEntry]] = Vec(MaxPrefetchReqNum, Flipped(Valid(new FtqEntry)))

    // dequeue, directly to ICache
    val deq: DecoupledIO[Dequeue] = DecoupledIO(new Dequeue)
  }

  val io: PrefetchQueueIO = IO(new PrefetchQueueIO)

  private val mem = RegInit(0.U.asTypeOf(Vec(Size, Valid(new Entry))))

  private val enqPtr = RegInit(PrefetchQueuePtr(false.B, 0.U))
  private val deqPtr = RegInit(PrefetchQueuePtr(false.B, 0.U))
  private val full   = distanceBetween(enqPtr, deqPtr) >= (Size - EnqueueNum).U

  /* *** enqueue *** */
  io.enq.foreach(_.ready := !full)

  /* *** dequeue *** */
  // select prefetch source:
  // 1. if FDIP (from Bpu -> entryQueue) can provide 2 non-conflict prefetch target, use them
  // 2. otherwise, if FDIP and prefetchQueue can provide 1 each, and not conflict, use them (TODO)

  private val prefetchReq = VecInit(
    Wire(new FtqPrefetchReq).fromFtqEntry(io.fromEntryQueue(0).bits),
    Wire(new FtqPrefetchReq).fromFtqEntry(io.fromEntryQueue(1).bits)
  )

  private val canTwoPrefetch =
    // when ftq can provide 2 FDIP entry (valid && passedPnr && no backend flag)
    io.fromEntryQueue(1).valid &&
      // and the 2 entries are on the same page, to prevent extra ITLB port
      prefetchReq(0).vPageNumber === prefetchReq(1).vPageNumber

  // (io.toICache.toPrefetch.fire && twoPrefetchValid) is passed to apply(..., canAssert) to prevent assert(x-state)
  private val twoPrefetchCase = TwoPrefetchCase(prefetchReq, io.deq.fire && canTwoPrefetch)

  io.deq.valid := io.fromEntryQueue(0).valid
  io.deq.bits.req.zipWithIndex.foreach { case (req, i) =>
    req.startVAddr       := prefetchReq(i).startVAddr
    req.nextLineVAddr    := prefetchReq(i).nextLineVAddr
    req.vSetIdx          := prefetchReq(i).vSetIdx
    req.isCrossLine      := prefetchReq(i).isCrossLine
    req.source           := PrefetchSource.Fdip
    req.ftqIdx           := DontCare // assigned in Ftq top
    req.backendException := DontCare // assigned in Ftq top
  }
  io.deq.bits.twoPrefetchCase := Mux(canTwoPrefetch, twoPrefetchCase, TwoPrefetchCase.Conflict)
  io.deq.bits.fdipNum         := Mux(canTwoPrefetch, 2.U, 1.U)

  /* *** sanity check & perf *** */
  XSError(deqPtr > enqPtr, "Dequeue pointer exceeds enqueue pointer in FtqPrefetchQueue")

  XSPerfAccumulate("full", full)
  XSPerfAccumulate("drop", PopCount(io.enq.map(port => port.valid && !port.ready)))
  XSPerfAccumulate("enq_total", PopCount(io.enq.map(_.fire)))
  XSPerfAccumulate("enq_sw", PopCount(io.enqFromSw.map(_.fire)))
}
