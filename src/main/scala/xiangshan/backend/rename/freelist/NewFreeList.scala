package xiangshan.backend.rename.freelist

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.rename.RegType
import xiangshan.backend.rename.Reg_I
import xiangshan.backend.rename.Reg_F
import xiangshan.backend.rename.Reg_V

class FreeListBundle(numPhyRegs: Int, RenameWidth: Int, commitWidth: Int)(implicit
    p: Parameters
) extends XSBundle {
  val redirect = Input(Bool())
  val walk     = Input(Bool())

  val walkPhyReg   = Input(Vec(commitWidth,UInt(log2Up(numPhyRegs).W)))
  val walkReq = Input(Vec(commitWidth, Bool()))
  val allocateReq    = Input(Vec(RenameWidth, Bool()))
  val allocatePhyReg = Output(Vec(RenameWidth, UInt(log2Up(numPhyRegs).W)))
  val canAllocate    = Output(Bool())
  val doAllocate     = Input(Bool())

  val freeReq    = Input(Vec(commitWidth, Bool()))
  val freePhyReg = Input(Vec(commitWidth, UInt(log2Up(numPhyRegs).W)))
  val commit = Input(new NewFreeListCommitBundle(commitWidth,numPhyRegs))
  
  val snpt = Input(new SnapshotPort)
  
  val debug_UnusedRegCount = if(backendParams.debugEn) Some(Output(UInt(PhyRegIdxWidth.W))) else None
}

object FreeListBundle {
  def apply(numPhyRegs: Int, RenameWidth: Int, commitWidth: Int)(implicit p: Parameters) =
    new FreeListBundle(numPhyRegs, RenameWidth, commitWidth)
}

object NewFreeList {
  val DefaultS1QueueSize = NewFLManager.DefaultS1QueueSize
}

class NewFreeList(
  numPhyRegs: Int,
  commitWidth: Int,
  RenameWidth: Int,
  regType: RegType,
  numLogicRegs: Int = 32,
  s1QueueSize: Int = NewFreeList.DefaultS1QueueSize
)(implicit p: Parameters)
    extends XSModule with HasXSParameter with HasPerfEvents {
  val io                = IO(FreeListBundle(numPhyRegs, RenameWidth, commitWidth))
  // Performance events are intentionally disabled for this implementation.
  val perfEvents: Seq[(String, UInt)] = Seq.empty
  // Encode free bits relative to the architectural reset bitmap.
  // Storage resets to zero; XOR restores exactly the original visible bits.
  def InitFreeList(reg_t: RegType) = {
    val initialFree = reg_t match {
      case Reg_I => Seq.tabulate(numPhyRegs)(i => i != 0)
      case Reg_F => Seq.tabulate(numPhyRegs)(i => i % (numPhyRegs / numLogicRegs) != 0)
      case Reg_V => Seq.tabulate(numPhyRegs)(i => i % (numPhyRegs / numLogicRegs) != 0)
      case _ => Seq.tabulate(numPhyRegs)(i => i >= numLogicRegs)
    }
    val resetMask = VecInit(initialFree.map(_.B)).asUInt
    // Keep the flops behind one word enable instead of a hold condition per bit.
    val state = Module(new NewFreeListBitmapRegister(numPhyRegs))
    val storage = Wire(UInt(numPhyRegs.W))
    state.io.data := storage
    state.io.enable := true.B
    val logical = (state.io.value ^ resetMask).asTypeOf(Vec(numPhyRegs, Bool()))
    (storage, logical, resetMask, state)
  }
  val (specStorage, specfreeListReg, specResetMask, specState) = InitFreeList(regType)
  val (archStorage, archfreeListReg, archResetMask, archState) = InitFreeList(regType)
  val freePhyRegOH = VecInit((0 until commitWidth).map { i =>
    Mux(io.freeReq(i), UIntToOH(io.freePhyReg(i), numPhyRegs), 0.U(numPhyRegs.W))
  })

  /** NewFreeList owns the free bitmaps; NewFLManager owns preg allocation. */
  val flManager = Module(new NewFLManager(numPhyRegs, RenameWidth, s1QueueSize, commitWidth))
  flManager.in.freeBitmap := specfreeListReg.asUInt
  flManager.in.allocateReq := io.allocateReq
  flManager.in.freeReq := io.freeReq
  flManager.in.freePhyReg := io.freePhyReg
  flManager.in.freePhyRegOH := freePhyRegOH
  flManager.in.doAllocate := io.doAllocate && !io.walk
  // RAB/VTypeBuffer enter walk one cycle after redirect; Rob registers their
  // commit bundles once more before Rename sees isWalk. Bridge that gap so s1
  // cannot select bitmap candidates or allocate before restoration. Valid
  // commit frees may still refill s1 throughout recovery.
  val redirectReg = RegNext(io.redirect, false.B)
  flManager.in.flush := io.redirect || redirectReg || io.walk

  io.allocatePhyReg := flManager.out.allocatePhyReg
  io.canAllocate := flManager.out.canAllocate

  val isWalkAlloc = io.walk && io.doAllocate
  val isNormalAlloc = !io.walk && io.canAllocate && io.doAllocate
  val realDoAllocate = !io.redirect && (isWalkAlloc || isNormalAlloc)
  val lastCycleRedirect = RegNext(redirectReg, false.B)
  val doRestore = realDoAllocate && lastCycleRedirect
  val doWalkClear = realDoAllocate && (io.walk || lastCycleRedirect)
  val freePhyRegOHOR = freePhyRegOH.reduce(_ | _)
  val walkPhyRegOHOR = (0 until commitWidth).map { i =>
    Mux(
      io.walkReq(i) && doWalkClear,
      UIntToOH(io.walkPhyReg(i), numPhyRegs),
      0.U(numPhyRegs.W)
    )
  }.reduce(_ | _)
  val commitPhyRegOHOR = (0 until commitWidth).map { i =>
    Mux(
      io.commit.doCommit && io.commit.archAlloc(i),
      UIntToOH(io.commit.archAllocPhyReg(i), numPhyRegs),
      0.U(numPhyRegs.W)
    )
  }.reduce(_ | _)

  // Snapshot metadata are only consumed when lastCycleRedirect is true.
  // Its reset value masks both pipeline stages until they contain valid data.
  val lastCycleSnpt     = RegNext(RegNext(io.snpt))
  val snapshotStore = Module(new NewFreeListSnapshotStore(numPhyRegs))
  snapshotStore.io.enqData := specfreeListReg.asUInt
  snapshotStore.io.enq := io.snpt.snptEnq
  snapshotStore.io.deq := io.snpt.snptDeq
  snapshotStore.io.redirect := io.redirect
  snapshotStore.io.flushVec := io.snpt.flushVec
  snapshotStore.io.freePhyRegOH := freePhyRegOHOR
  snapshotStore.io.hasFree := io.freeReq.asUInt.orR
  snapshotStore.io.select := lastCycleSnpt.snptSelect

  val redirectedFreeList = Mux(
    lastCycleSnpt.useSnpt,
    snapshotStore.io.selected,
    archfreeListReg.asUInt
  )
  
  // The manager qualifies dequeue with canAllocate, doAllocate and flush.
  // Reuse that mask for speculative state instead of qualifying it again.
  val specBase = Mux(doRestore, redirectedFreeList,
    specfreeListReg.asUInt & ~flManager.out.dequeuedBitmap)
  specStorage := ((specBase & ~walkPhyRegOHOR) | freePhyRegOHOR) ^ specResetMask
  archStorage := ((archfreeListReg.asUInt & ~commitPhyRegOHOR) | freePhyRegOHOR) ^ archResetMask

  // Without any of these events, both logical bitmaps keep their values.
  val bitmapUpdate = doRestore || doWalkClear || isNormalAlloc || io.freeReq.asUInt.orR || io.commit.doCommit
  specState.io.enable := bitmapUpdate
  archState.io.enable := bitmapUpdate

  // Keep the optional debug port deterministic without synthesizing
  // the free-list PopCount or performance-monitoring network.
  io.debug_UnusedRegCount.foreach(_ := 0.U)
}

class NewFreeListCommitBundle(commitWidth: Int,numPhyRegs: Int) extends Bundle {
  val doCommit = Bool()
  val archAlloc = Vec(commitWidth, Bool())
  val archAllocPhyReg = Vec(commitWidth, UInt(log2Up(numPhyRegs).W))
}

// Capture and releases update fixed busy-bitmap checkpoint rows.
// Inline the store to share its update logic with the owner's bitmap paths.
class NewFreeListSnapshotStore(numPhyRegs: Int)(implicit p: Parameters)
  extends XSModule with HasCircularQueuePtrHelper {
  chisel3.experimental.annotate(this)(Seq(firrtl.passes.InlineAnnotation(toNamed)))
  private val count = RenameSnapshotNum
  val io = IO(new Bundle {
    val enq = Input(Bool())
    val deq = Input(Bool())
    val redirect = Input(Bool())
    val flushVec = Input(Vec(count, Bool()))
    val enqData = Input(UInt(numPhyRegs.W))
    val freePhyRegOH = Input(UInt(numPhyRegs.W))
    val hasFree = Input(Bool())
    val select = Input(UInt(log2Ceil(count).W))
    val selected = Output(UInt(numPhyRegs.W))
  })
  // Reuse the original pointer/valid/flush implementation with no payload.
  val metadata = Module(new NewFreeListSnapshotMetadata)
  metadata.io.enq := io.enq
  metadata.io.deq := io.deq
  metadata.io.redirect := io.redirect
  metadata.io.flushVec := io.flushVec
  metadata.io.enqData := 0.U

  // Capture overwrites one checkpoint and releases clear busy bits in all rows.
  val rows = Reg(Vec(count, UInt(numPhyRegs.W)))
  val capture = io.enq && !io.redirect && !isFull(metadata.io.enqPtr, metadata.io.deqPtr)
  when(capture || io.hasFree) {
    for (row <- 0 until count) {
      rows(row) := Mux(capture && metadata.io.enqPtr.value === row.U, ~io.enqData, rows(row)) & ~io.freePhyRegOH
    }
  }
  io.selected := ~Mux1H(UIntToOH(io.select, count), rows)
}

// Keep the original enqueue/dequeue/flush rules and only replace payload storage.
class NewFreeListSnapshotMetadata(implicit p: Parameters)
  extends xiangshan.backend.rename.SnapshotGenerator[UInt](0.U(1.W))

class NewFreeListBitmapRegister(width: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val data = Input(UInt(width.W))
    val enable = Input(Bool())
    val value = Output(UInt(width.W))
  })
  val bits = RegInit(0.U(width.W))
  when(io.enable) { bits := io.data }
  io.value := bits
}
