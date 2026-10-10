package xiangshan.frontend.bpu.mbtb

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.CircularQueuePtr
import utility.XSPerfAccumulate
import xiangshan.frontend.bpu.SaturateCounter

class MainBtbCounterBuffer(implicit p: Parameters) extends MainBtbModule with Helpers {
  class MainBtbCounterBufferIO extends Bundle {
    class WriteReq extends Bundle {
      val alloc:     Bool                 = Bool()
      val tag:       UInt                 = UInt(TagWidth.W)
      val position:  UInt                 = UInt(CfiPositionWidth.W)
      val setIdx:    UInt                 = UInt(SetIdxLen.W)
      val wayMask:   UInt                 = UInt(NumWay.W)
      val allocMask: UInt                 = UInt(NumWay.W)
      val counters:  Vec[SaturateCounter] = Vec(NumWay, TakenCounter())
    }
    val writeReq:  Valid[WriteReq]                         = Input(Valid(new WriteReq))
    val writeSram: DecoupledIO[MainBtbCounterSramWriteReq] = Decoupled(new MainBtbCounterSramWriteReq)
  }
  val io: MainBtbCounterBufferIO = IO(new MainBtbCounterBufferIO)

  private val FilterSize = WriteBufferSize / 2

  class FilterCounter(implicit p: Parameters) extends MainBtbBundle {
    val valid:    Bool = Bool()
    val setIdx:   UInt = UInt(SetIdxLen.W)
    val tag:      UInt = UInt(TagWidth.W)
    val position: UInt = UInt(CfiPositionWidth.W)
  }

  class CounterEntry(implicit p: Parameters) extends MainBtbBundle {
    val setIdx:    UInt                 = UInt(SetIdxLen.W)
    val wayMask:   UInt                 = UInt(NumWay.W)
    val allocMask: UInt                 = UInt(NumWay.W)
    val counters:  Vec[SaturateCounter] = Vec(NumWay, TakenCounter())
  }

  class CounterBufferPtr(implicit p: Parameters) extends CircularQueuePtr[CounterBufferPtr](WriteBufferSize) {}

  private val req = io.writeReq.bits

  private val filters       = RegInit(VecInit(Seq.fill(FilterSize)(0.U.asTypeOf(new FilterCounter))))
  private val bufferEntries = RegInit(VecInit(Seq.fill(WriteBufferSize)(0.U.asTypeOf(new CounterEntry))))
  private val dirty         = RegInit(VecInit(Seq.fill(WriteBufferSize)(false.B)))
  private val enqPtr        = RegInit(0.U.asTypeOf(new CounterBufferPtr))
  private val deqPtr        = RegInit(0.U.asTypeOf(new CounterBufferPtr))
  private val isEmpty       = deqPtr === enqPtr
  private val isFull        = (enqPtr.value === deqPtr.value) && (enqPtr.flag =/= deqPtr.flag)

  // filter out duplicated allocation of the same (setIdx, tag, position), used for multi-hit
  private val filterHit = VecInit(filters.map { f =>
    f.valid && f.setIdx === req.setIdx && f.tag === req.tag && f.position === req.position
  }).asUInt.orR
  private val canAlloc = !filterHit

  // duplicated allocations are dropped, plain counter updates are always accepted
  private val canWrite = (req.alloc && canAlloc) || !req.alloc
  private val accept   = io.writeReq.valid && canWrite

  // coalesce writes targeting the same set
  private val hitMask = VecInit((bufferEntries zip dirty).map { case (entry, d) =>
    d && entry.setIdx === req.setIdx
  })
  private val hit    = hitMask.asUInt.orR
  private val hitIdx = PriorityEncoder(hitMask.asUInt)

  private val writeIdx = Mux(hit, hitIdx, enqPtr.value)
  private val doWrite  = accept && (hit || !isFull)
  private val allocNew = accept && !hit && !isFull

  // merged next value of the target slot
  private val oldWayMask   = Mux(hit, bufferEntries(writeIdx).wayMask, 0.U)
  private val oldAllocMask = Mux(hit, bufferEntries(writeIdx).allocMask, 0.U)
  private val merged       = Wire(new CounterEntry)
  merged := bufferEntries(writeIdx)
  when(doWrite) {
    merged.setIdx    := req.setIdx
    merged.wayMask   := oldWayMask | req.wayMask
    merged.allocMask := oldAllocMask | Mux(req.alloc, req.allocMask, 0.U)
    for (i <- 0 until NumWay) {
      // this way was just allocated, take its fresh (weak) counter
      val allocThisWay = req.alloc && req.allocMask(i)
      // on a hit, only update ways that are not locked by a previous allocation
      val lockedThisWay = oldAllocMask(i)
      val updateThisWay = req.wayMask(i) && !lockedThisWay
      // on a miss the whole slot is repurposed, so take the entire counter vector
      when(!hit || allocThisWay || updateThisWay) {
        merged.counters(i) := req.counters(i)
      }
    }
    bufferEntries(writeIdx) := merged
  }

  when(allocNew) {
    dirty(enqPtr.value) := true.B
    enqPtr              := enqPtr + 1.U
  }

  when(!isEmpty && io.writeSram.ready) {
    dirty(deqPtr.value) := false.B
    deqPtr              := deqPtr + 1.U
  }

  // record accepted allocations so duplicated (multi-hit) allocations can be filtered
  when(doWrite && req.alloc) {
    filters(0).valid    := true.B
    filters(0).setIdx   := req.setIdx
    filters(0).tag      := req.tag
    filters(0).position := req.position
    for (i <- 1 until FilterSize) {
      filters(i) := filters(i - 1)
    }
  }

  // forward the merged value if the entry being sent to the sram is also being merged this cycle
  private val deqEntry = Wire(new CounterEntry)
  deqEntry := bufferEntries(deqPtr.value)
  when(doWrite && writeIdx === deqPtr.value) {
    deqEntry := merged
  }

  io.writeSram.valid         := !isEmpty
  io.writeSram.bits.setIdx   := deqEntry.setIdx
  io.writeSram.bits.wayMask  := deqEntry.wayMask
  io.writeSram.bits.counters := deqEntry.counters

  XSPerfAccumulate(
    "counter_writebuffer_drop_dup",
    io.writeReq.valid && req.alloc && !canAlloc
  )
  XSPerfAccumulate(
    "counter_writebuffer_full_drop",
    accept && !hit && isFull
  )
}
