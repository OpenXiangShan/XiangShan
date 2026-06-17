package xiangshan.backend.vector

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.Bundles.UopIdx
import xiangshan.backend.rob.RobPtr

object VecOrderQueue {
  val NumEntries = 32
  val VAGQSize = 8

  class VoQEnqIO(implicit p: Parameters) extends XSBundle {
    val valid = Bool()
    val robIdx = new RobPtr
    val uopIdx = UopIdx()
    val useVAGQ = Bool()
    val useGather = Bool()
  }

  class Wake(implicit p: Parameters) extends XSBundle {
    val valid = Bool()
    val robIdx = new RobPtr
    val entryIdx = UInt(log2Ceil(VAGQSize).W)
  }

  class In(implicit p: Parameters) extends XSBundle {
    val enq = Vec(RenameWidth, new VoQEnqIO)
    val commit = new RobCommitIO
    val redirect = ValidIO(new Redirect)
  }

  class Out(implicit p: Parameters) extends XSBundle {
    val canAccept = Bool()
    val wake = new Wake
    val vagqEntryBitmap = UInt(VAGQSize.W)
  }
}

class VecOrderQueue(implicit p: Parameters) extends XSModule {
  import VecOrderQueue._

  private class Entry extends XSBundle {
    val valid = Bool()
    val robIdx = new RobPtr
    val useVAGQ = Bool()
    val useGather = Bool()
    val uopNum = UopIdx()
    val vagqEntryMask = UInt(VAGQSize.W)
  }

  val in = IO(Input(new VecOrderQueue.In))
  val out = IO(Output(new VecOrderQueue.Out))

  private val entries = RegInit(0.U.asTypeOf(Vec(NumEntries, new Entry)))
  private val enqPtrOH = RegInit(1.U(NumEntries.W))
  private val deqPtrOH = RegInit(1.U(NumEntries.W))
  private val wakePtrOH = RegInit(1.U(NumEntries.W))
  private val entryCount = RegInit(0.U(log2Ceil(NumEntries + RenameWidth + 1).W))
  private val vagqEntryBitmap = RegInit(0.U(VAGQSize.W))

  private def consecutiveMaskConst(base: Int, count: Int): UInt = {
    (0 until count).foldLeft(BigInt(0)) { (mask, offset) =>
      mask | (BigInt(1) << ((base + offset) % VAGQSize))
    }.U(VAGQSize.W)
  }

  private def entryReleaseMask(entry: Entry): UInt = {
    Mux(
      entry.useVAGQ,
      entry.vagqEntryMask,
      0.U(VAGQSize.W)
    )
  }

  private val enq = VecInit(in.enq.map(_.valid))
  private val enqCount = PopCount(enq)
  private val allowEnqueue = RegNext(entryCount + enqCount <= (NumEntries - RenameWidth).U)
  private val doEnq = !in.redirect.valid && (enq.asUInt.orR && allowEnqueue)

  private val deqPtrOHShift = CircularShift(deqPtrOH)
  private val deqPtrOHVec = VecInit.tabulate(CommitWidth + 1)(deqPtrOHShift.left)
  private val enqPtrOHShift = CircularShift(enqPtrOH)
  private val enqPtrOHVec = VecInit.tabulate(RenameWidth + 1)(enqPtrOHShift.left)
  private val wakePtrOHShift = CircularShift(wakePtrOH)
  private val allocatePtrOHVec = VecInit((0 until RenameWidth).map { i =>
    val enqOffset = Wire(UInt(log2Ceil(RenameWidth + 1).W))
    enqOffset := 0.U
    if (i > 0) {
      enqOffset := PopCount(enq.take(i))
    }
    enqPtrOHVec(enqOffset)
  })

  private val enqWdata = WireDefault(0.U.asTypeOf(Vec(RenameWidth, new Entry)))
  for (i <- 0 until RenameWidth) {
    when(enq(i)) {
      enqWdata(i).valid := true.B
      enqWdata(i).robIdx := in.enq(i).robIdx
      enqWdata(i).useVAGQ := in.enq(i).useVAGQ
      enqWdata(i).useGather := in.enq(i).useGather
      enqWdata(i).uopNum := in.enq(i).uopIdx
      enqWdata(i).vagqEntryMask := 0.U
    }
  }

  // commit
  private val doCommit = in.commit.isCommit
  private val commitCount = Mux(doCommit, in.commit.info.head.voqCommitSize, 0.U)
  private val commitHit = VecInit((0 until NumEntries).map { entryIdx =>
    VecInit((0 until CommitWidth).map { i =>
      i.U < commitCount && deqPtrOHVec(i)(entryIdx)
    }).asUInt.orR
  })

  private val commitReleaseMask = VecInit((0 until NumEntries).map { entryIdx =>
    Mux(commitHit(entryIdx), entryReleaseMask(entries(entryIdx)), 0.U(VAGQSize.W))
  }).reduce(_ | _)

  // redirect
  private val flushHit = VecInit((0 until NumEntries).map { entryIdx =>
    entries(entryIdx).valid && entries(entryIdx).robIdx.needFlush(in.redirect)
  })
  private val flushHitBits = flushHit.asUInt
  private val prevFlushHitBits = CircularShift(flushHitBits).left(1)
  private val firstFlushOH = flushHitBits & (deqPtrOH | ~prevFlushHitBits)
  private val flushReleaseMask = VecInit((0 until NumEntries).map { entryIdx =>
    Mux(flushHit(entryIdx), entryReleaseMask(entries(entryIdx)), 0.U(VAGQSize.W))
  }).reduce(_ | _)

  // release
  private val releaseMask = commitReleaseMask | flushReleaseMask
  private val bitmapAfterRelease = vagqEntryBitmap & ~releaseMask

  // wake
  private val wakeEntry = Mux1H(wakePtrOH, entries)
  private val wakeReqCount = wakeEntry.uopNum.pad(log2Ceil(VAGQSize + 1)) + 1.U
  private val allCanAlloc = VecInit((1 to VAGQSize).map { count =>
    VecInit((0 until VAGQSize).map { base =>
      (vagqEntryBitmap & consecutiveMaskConst(base, count)) === 0.U
    })
  })
  private val wakeCanAlloc = Mux1H((1 to VAGQSize).map { count =>
    (wakeReqCount === count.U) -> allCanAlloc(count - 1)
  })
  private val wakeNeedsVAGQ = wakeEntry.useVAGQ
  private val wakeCanAllocBits = Mux(wakeNeedsVAGQ, wakeCanAlloc.asUInt, 0.U(VAGQSize.W))//TODO gather
  private val wakeEntryIdxOH = PriorityEncoderOH(wakeCanAllocBits)
  private val allWakeMask = VecInit((1 to VAGQSize).map { count =>
    Mux1H((0 until VAGQSize).map { base =>
      wakeEntryIdxOH(base) -> consecutiveMaskConst(base, count)
    })
  })
  private val wakeMask = Mux1H((1 to VAGQSize).map { count =>
    (wakeReqCount === count.U) -> allWakeMask(count - 1)
  })
  private val wakeWillClear = (wakePtrOH & flushHit.asUInt).orR
  private val wakeValid = !in.redirect.valid && wakeEntry.valid && wakeEntry.useVAGQ && !wakeWillClear && wakeCanAllocBits.orR
  private val wakeAllocMask = Mux(wakeValid, wakeMask, 0.U(VAGQSize.W))

  when(doEnq) {
    for (entryIdx <- 0 until NumEntries) {
      val hit = VecInit((0 until RenameWidth).map(i => enq(i) && allocatePtrOHVec(i)(entryIdx)))
      when(hit.asUInt.orR) {
        entries(entryIdx) := Mux1H(hit, enqWdata)
      }
    }
  }

  for (entryIdx <- 0 until NumEntries) {
    when(wakeValid && wakePtrOH(entryIdx)) {
      entries(entryIdx).vagqEntryMask := wakeAllocMask
    }
  }

  when(doCommit) {
    for (entryIdx <- 0 until NumEntries) {
      when(commitHit(entryIdx)) {
        entries(entryIdx).valid := false.B
      }
    }
  }

  when(in.redirect.valid) {
    for (entryIdx <- 0 until NumEntries) {
      when(flushHit(entryIdx)) {
        entries(entryIdx).valid := false.B
      }
    }

    val redirectEnqPtrOH = Mux(flushHitBits.orR, firstFlushOH, enqPtrOH)
    val redirectFlushAll = (firstFlushOH & deqPtrOH).orR

    enqPtrOH := redirectEnqPtrOH
    when(wakeWillClear || redirectFlushAll) {
      wakePtrOH := redirectEnqPtrOH
    }
    entryCount := PopCount(VecInit((0 until NumEntries).map { entryIdx =>
      entries(entryIdx).valid && !flushHit(entryIdx)
    }))
    vagqEntryBitmap := bitmapAfterRelease | wakeAllocMask
  }.otherwise {
    deqPtrOH := deqPtrOHVec(commitCount)
    when(doEnq) {
      enqPtrOH := enqPtrOHVec(enqCount)
    }
    when(wakeValid) {
      wakePtrOH := wakePtrOHShift.left(1)
    }.elsewhen(entryCount === commitCount && doEnq) {
      wakePtrOH := enqPtrOH
    }
    entryCount := entryCount - commitCount + Mux(doEnq, enqCount, 0.U)
    vagqEntryBitmap := bitmapAfterRelease | wakeAllocMask
  }

  out.canAccept := allowEnqueue
  out.vagqEntryBitmap := vagqEntryBitmap

  out.wake.valid := wakeValid
  out.wake.robIdx := wakeEntry.robIdx
  out.wake.entryIdx := OHToUInt(wakeEntryIdxOH)

  dontTouch(entries)
  dontTouch(enqPtrOH)
  dontTouch(deqPtrOH)
  dontTouch(wakePtrOH)
  dontTouch(entryCount)
  dontTouch(vagqEntryBitmap)

  XSError(in.redirect.valid && in.commit.isCommit, "VecOrderQueue: commit and redirect in same cycle")
}
