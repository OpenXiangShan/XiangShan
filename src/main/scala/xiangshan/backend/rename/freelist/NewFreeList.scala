package xiangshan.backend.rename.freelist

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import xiangshan._
import xiangshan.backend.rename.FreeListSnapshotGenerator
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
    extends XSModule with HasXSParameter with HasPerfEvents{
  val io                = IO(FreeListBundle(numPhyRegs, RenameWidth, commitWidth))
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
    val storage = RegInit(0.U(numPhyRegs.W))
    val logical = (storage ^ resetMask).asTypeOf(Vec(numPhyRegs, Bool()))
    (storage, logical, resetMask)
  }
  val (specStorage, specfreeListReg, specResetMask) = InitFreeList(regType)
  val (archStorage, archfreeListReg, archResetMask) = InitFreeList(regType)
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

  //Debug
  XSPerfAccumulate("utilization", PopCount(io.allocateReq))
  XSPerfAccumulate("allocation_blocked_cycle", !io.canAllocate)
  
  val freeListSize = numPhyRegs - numLogicRegs
  val freeRegCnt = Option.when(!env.FPGAPlatform || backendParams.debugEn)(PopCount(specfreeListReg.asUInt))
  io.debug_UnusedRegCount.foreach(_ := freeRegCnt.get)
  val perfEvents = if (!env.FPGAPlatform) {
    val freeRegCntReg = RegNext(freeRegCnt.get)
    QueuePerf(size = freeListSize, utilization = freeRegCntReg, full = freeRegCntReg === 0.U)
    Seq(
      ("std_freelist_1_4_valid", freeRegCntReg <  (freeListSize / 4).U                                            ),
      ("std_freelist_2_4_valid", freeRegCntReg >= (freeListSize / 4).U && freeRegCntReg < (freeListSize / 2).U    ),
      ("std_freelist_3_4_valid", freeRegCntReg >= (freeListSize / 2).U && freeRegCntReg < (freeListSize * 3 / 4).U),
      ("std_freelist_4_4_valid", freeRegCntReg >= (freeListSize * 3 / 4).U                                        )
    )
  } else {
    Seq.empty
  }

  generatePerfEvent()
}

class NewFreeListCommitBundle(commitWidth: Int,numPhyRegs: Int) extends Bundle {
  val doCommit = Bool()
  val archAlloc = Vec(commitWidth, Bool())
  val archAllocPhyReg = Vec(commitWidth, UInt(log2Up(numPhyRegs).W))
}

// On capture/free events, swap each pair of physical rows and update phase.
// Logical checkpoint numbers stay fixed. The shared event enables whole rows,
// avoiding a separate hold/clock-gate condition for every physical-register bit.
class NewFreeListSnapshotStore(numPhyRegs: Int)(implicit p: Parameters)
  extends XSModule with HasCircularQueuePtrHelper {
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

  // Store busy bits so every release clears the same bit in every checkpoint.
  val rows = Reg(Vec(count, UInt(numPhyRegs.W)))
  val phase = RegInit(false.B)
  val nextPhase = !phase
  def physicalRow(index: UInt, flip: Bool): UInt = {
    val canSwap = if (count % 2 == 0) true.B else index =/= (count - 1).U
    Mux(flip && canSwap, index ^ 1.U, index)
  }
  val capture = io.enq && !io.redirect && !isFull(metadata.io.enqPtr, metadata.io.deqPtr)
  val captureRow = physicalRow(metadata.io.enqPtr.value, nextPhase)
  when(capture || io.hasFree) {
    for (row <- 0 until count) {
      val partner = if ((row ^ 1) < count) row ^ 1 else row
      rows(row) := Mux(capture && captureRow === row.U, ~io.enqData, rows(partner)) & ~io.freePhyRegOH
    }
    phase := nextPhase
  }
  io.selected := ~Mux1H(UIntToOH(physicalRow(io.select, phase), count), rows)
}

// Keep the original enqueue/dequeue/flush rules and only replace payload storage.
class NewFreeListSnapshotMetadata(implicit p: Parameters)
  extends xiangshan.backend.rename.SnapshotGenerator[UInt](0.U(1.W))
