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
import utility.XSPerfSeqAccumulate
import xiangshan.SoftIPrefetchBundle
import xiangshan.frontend.PcInit
import xiangshan.frontend.TwoPrefetchCase
import xiangshan.frontend.icache.PrefetchSource

class PrefetchQueue(implicit p: Parameters) extends FtqModule
    with HasFtqPrefetchQueueParameters
    with HasCircularQueuePtrHelper {
  class PrefetchQueueIO extends Bundle {
    class Enqueue extends PrefetchQueueEntry {
      def :=(that: SoftIPrefetchBundle): Unit = {
        this.vAddr  := PcInit(that.vaddr).signGuard
        this.source := PrefetchSource.Sw
      }
    }

    class Dequeue extends Bundle {
      val req:             Vec[FtqPrefetchReq] = Vec(MaxPrefetchReqNum, new FtqPrefetchReq)
      val twoPrefetchCase: TwoPrefetchCase     = new TwoPrefetchCase
      // we need to represent "MaxPrefetchReqNum" itself, so we need log2(...+1) width
      val numFdip: UInt = UInt(log2Ceil(MaxPrefetchReqNum + 1).W)
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

  private val mem = RegInit(0.U.asTypeOf(Vec(Size, Valid(new PrefetchQueueEntry))))

  private val enqPtr = RegInit(PrefetchQueuePtr(false.B, 0.U))
  private val deqPtr = RegInit(PrefetchQueuePtr(false.B, 0.U))
  private val full   = distanceBetween(enqPtr, deqPtr) >= (Size - EnqueueNum).U

  /* *** enqueue *** */
  io.enq.zipWithIndex.foreach { case (enqPort, i) =>
    enqPort.ready := !full
    val enqIdx = (enqPtr + PopCount(io.enq.take(i).map(_.valid))).value
    when(enqPort.valid && !full) {
      mem(enqIdx).valid := true.B
      mem(enqIdx).bits  := enqPort.bits
    }
  }
  when(!full && io.enq.map(_.valid).reduce(_ || _)) { // gate using valid.orR to save some power, may be bad for timing
    enqPtr := enqPtr + PopCount(io.enq.map(_.valid))
  }

  /* *** dequeue *** */
  private class PrefetchGroup extends Bundle {
    val req:             Vec[Valid[FtqPrefetchReq]] = Vec(MaxPrefetchReqNum, Valid(new FtqPrefetchReq))
    val twoPrefetchCase: TwoPrefetchCase            = new TwoPrefetchCase

    // pointer movements, invalid if !has1
    // we need to represent "MaxPrefetchReqNum" itself, so we need log2(...+1) width
    val numFdip:   UInt = UInt(log2Ceil(MaxPrefetchReqNum + 1).W)
    val numQueued: UInt = UInt(log2Ceil(MaxPrefetchReqNum + 1).W)

    def has1: Bool = req.head.valid
    def has2: Bool = twoPrefetchCase.valid // !conflict
  }
  private object PrefetchGroup {
    def apply(req: Vec[Valid[FtqPrefetchReq]], t: String): PrefetchGroup = {
      val group = Wire(new PrefetchGroup)
      group.req := req
      // io.deq.fire is passed to apply(..., canAssert) to prevent assert(x-state)
      group.twoPrefetchCase := TwoPrefetchCase(req, io.deq.fire)
      // pointer movements
      t match {
        case "fdip" =>
          group.numFdip   := Mux(group.twoPrefetchCase.valid, 2.U, 1.U)
          group.numQueued := 0.U
        case "mixed" =>
          group.numFdip   := 1.U
          group.numQueued := Mux(group.twoPrefetchCase.valid, 1.U, 0.U)
        case "queued" =>
          group.numFdip   := 0.U
          group.numQueued := Mux(group.twoPrefetchCase.valid, 2.U, 1.U)
      }
      group
    }
  }

  private def genFdipPrefetch(entry: Valid[FtqEntry]): Valid[FtqPrefetchReq] = {
    val req = Wire(Valid(new FtqPrefetchReq))
    req.bits.fromFtqEntry(entry.bits)
    req.valid := entry.valid
    req
  }
  private val fdipPrefetch = PrefetchGroup(VecInit(io.fromEntryQueue.map(genFdipPrefetch)), "fdip")

  private def genQueuedPrefetch(entry: Valid[PrefetchQueueEntry]): Valid[FtqPrefetchReq] = {
    val req = Wire(Valid(new FtqPrefetchReq))
    req.bits.fromPrefetchQueueEntry(entry.bits)
    req.valid := entry.valid
    req
  }
  private val queuedPrefetch = PrefetchGroup(
    VecInit(
      genQueuedPrefetch(mem(deqPtr.value)),
      genQueuedPrefetch(mem((deqPtr + 1.U).value))
    ),
    "queued"
  )

  private val mixedPrefetch = PrefetchGroup(VecInit(fdipPrefetch.req.head, queuedPrefetch.req.head), "mixed")

  // select prefetch source:
  // 2-fdip > 1-fdip + 1-queued(software etc.) > 1-fdip > 2 or 1-queued
//  private val selectedPrefetch = MuxCase(
//    queuedPrefetch,
//    Seq(
//      fdipPrefetch.has2  -> fdipPrefetch,
//      mixedPrefetch.has2 -> mixedPrefetch,
//      fdipPrefetch.has1  -> fdipPrefetch
//    )
//  )
  // NOTE: the above version can be bad at timing, a simplified version will be:
  // 2-fdip > 1-fdip + 1-queued or 1-fdip, no 2-queued is allowed
  private val selectedPrefetch = Mux(fdipPrefetch.has2, fdipPrefetch, mixedPrefetch)

  // send back to Ftq when has at least 1 prefetch req
  io.deq.valid                := selectedPrefetch.has1
  io.deq.bits.req             := VecInit(selectedPrefetch.req.map(_.bits))
  io.deq.bits.twoPrefetchCase := selectedPrefetch.twoPrefetchCase
  io.deq.bits.numFdip         := selectedPrefetch.numFdip

  when(io.deq.fire) {
    deqPtr := deqPtr + selectedPrefetch.numQueued
    (0 until MaxPrefetchReqNum).foreach { i=>
      when (i.U < selectedPrefetch.numQueued) {
        mem((deqPtr + i.U).value).valid := false.B
      }
    }
  }

  /* *** sanity check & perf *** */
  XSError(deqPtr > enqPtr, "Dequeue pointer exceeds enqueue pointer in FtqPrefetchQueue")

  XSPerfAccumulate("full", full)
  XSPerfAccumulate("drop", PopCount(io.enq.map(port => port.valid && !port.ready)))
  XSPerfAccumulate("enq_total", PopCount(io.enq.map(_.fire)))
  XSPerfAccumulate("enq_sw", PopCount(io.enqFromSw.map(_.fire)))

  XSPerfSeqAccumulate(
    "2pf",
    io.deq.fire && selectedPrefetch.has2,
    Seq(
      ("total", true.B)
    ) ++ selectedPrefetch.twoPrefetchCase.getValidSeq
  )
  XSPerfSeqAccumulate(
    "fdip_2pf",
    io.deq.fire && fdipPrefetch.has2,
    Seq(
      ("total", true.B)
    ) ++ fdipPrefetch.twoPrefetchCase.getValidSeq
  )
  XSPerfSeqAccumulate(
    "mixed_2pf",
    io.deq.fire && !fdipPrefetch.has2 && mixedPrefetch.has2,
    Seq(
      ("total", true.B)
    ) ++ mixedPrefetch.twoPrefetchCase.getValidSeq
  )
  XSPerfSeqAccumulate(
    "fdip_1pf",
    io.deq.fire && !fdipPrefetch.has2,
    Seq(
      ("no_target", !fdipPrefetch.req(1).valid),
      ("page_conflict", fdipPrefetch.req(0).bits.vPageNumber =/= fdipPrefetch.req(1).bits.vPageNumber),
      ("sram_conflict", fdipPrefetch.twoPrefetchCase.isConflict)
    ),
    withPriority = true
  )
  XSPerfSeqAccumulate(
    "mixed_1pf",
    io.deq.fire && !fdipPrefetch.has2 && !mixedPrefetch.has2,
    Seq(
      ("no_target", !mixedPrefetch.req(1).valid),
      ("page_conflict", mixedPrefetch.req(0).bits.vPageNumber =/= mixedPrefetch.req(1).bits.vPageNumber),
      ("sram_conflict", mixedPrefetch.twoPrefetchCase.isConflict)
    ),
    withPriority = true
  )
}
