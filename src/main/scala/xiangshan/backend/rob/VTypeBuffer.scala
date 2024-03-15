package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{CircularQueuePtr, CircularShift, HasCircularQueuePtrHelper, OneHot, SyncDataModuleTemplate, GatedValidRegNext}
import utils.{QueuePerf, XSError, XSPerfAccumulate}
import xiangshan.backend.Bundles.DynInst
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.rename.SnapshotGenerator
import xiangshan.{SnapshotPort, XSBundle, XSCoreParamsKey, XSModule}

class VTypeBufferPtr(size: Int) extends CircularQueuePtr[VTypeBufferPtr](size) {
  def this()(implicit p: Parameters) = this(p(XSCoreParamsKey).VTypeBufferSize)
}

object VTypeBufferPtr {
  def apply(flag: Boolean = false, v: Int = 0)(implicit p: Parameters): VTypeBufferPtr = {
    val ptr = Wire(new VTypeBufferPtr(p(XSCoreParamsKey).VTypeBufferSize))
    ptr.flag := flag.B
    ptr.value := v.U
    ptr
  }
}

class VTypeBufferEntry(implicit p: Parameters) extends Bundle {
  val vtype = new VType()
}

class VTypeBufferIO(size: Int)(implicit p: Parameters) extends XSBundle {
  val redirect = Input(ValidIO(new Bundle{}))

  val req = Vec(RenameWidth, Flipped(ValidIO(new DynInst)))

  val fromRob = new Bundle {
    val walkSize = Input(UInt(log2Up(size).W))
    val walkEnd = Input(Bool())
    val commitSize = Input(UInt(log2Up(size).W))
  }

  val snpt = Input(new SnapshotPort)

  val canEnq = Output(Bool())

  val toDecode = Output(new Bundle {
    val isResumeVType = Bool()
    val commitVType = ValidIO(VType())
    val walkVType = ValidIO(VType())
  })

  val status = Output(new Bundle {
    val walkEnd = Bool()
  })
}

class VTypeBuffer(size: Int)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {
  val io = IO(new VTypeBufferIO(size))

  // alias
  private val useSnpt = io.snpt.useSnpt
  private val snptSelect = io.snpt.snptSelect

  private val s_idle :: s_spcl_walk :: s_walk :: Nil = Enum(3)
  private val state = RegInit(s_idle)
  private val stateNext = WireInit(state) // otherwise keep state value
  private val stateLast = RegEnable(state, state =/= stateNext)
  private val stateLastCycle = RegNext(state)

  // +1 read port to get walk initial state
  private val vtypeBuffer = Module(new SyncDataModuleTemplate(new VTypeBufferEntry(), size, numWrite = RenameWidth, numRead = CommitWidth + 1))
  private val vtypeBufferReadAddrVec = vtypeBuffer.io.raddr
  private val vtypeBufferReadDataVec = vtypeBuffer.io.rdata
  private val vtypeBufferWriteEnVec = vtypeBuffer.io.wen
  private val vtypeBufferWriteAddrVec = vtypeBuffer.io.waddr
  private val vtypeBufferWriteDataVec = vtypeBuffer.io.wdata

  // pointer
  private val enqPtrVec = RegInit(VecInit.tabulate(RenameWidth)(idx => VTypeBufferPtr(flag = false, idx)))
  private val enqPtr = enqPtrVec.head
  private val enqPtrOH = RegInit(1.U(size.W))
  private val enqPtrOHShift = CircularShift(enqPtrOH)
  // may shift [0, RenameWidth] steps
  private val enqPtrOHVec = VecInit.tabulate(RenameWidth + 1)(enqPtrOHShift.left)
  private val enqPtrVecNext = WireInit(enqPtrVec)

  private val deqPtrVec = RegInit(VecInit.tabulate(CommitWidth)(idx => VTypeBufferPtr(flag = false, idx)))
  private val deqPtr = deqPtrVec.head
  private val deqPtrOH = RegInit(1.U(size.W))
  private val deqPtrOHShift = CircularShift(deqPtrOH)
  private val deqPtrOHVec = VecInit.tabulate(CommitWidth + 1)(deqPtrOHShift.left)
  private val deqPtrVecNext = WireInit(deqPtrVec)
  XSError(deqPtr.toOH =/= deqPtrOH, p"wrong one-hot reg between $deqPtr and $deqPtrOH")

  private val walkPtrVec = RegInit(VecInit.tabulate(CommitWidth)(idx => VTypeBufferPtr(flag = false, idx)))
  private val walkPtr = Reg(new VTypeBufferPtr)
  private val walkPtrOH = walkPtr.toOH
  private val walkPtrOHVec = VecInit.tabulate(CommitWidth + 1)(CircularShift(walkPtrOH).left)
  private val walkPtrNext = Wire(new VTypeBufferPtr)
  private val walkPtrVecNext = VecInit((0 until CommitWidth).map(x => walkPtrNext + x.U))

  private val walkPtrSnapshots = SnapshotGenerator(enqPtr, io.snpt.snptEnq, io.snpt.snptDeq, io.redirect.valid, io.snpt.flushVec)

  private val robWalkEndReg = RegInit(false.B)
  private val robWalkEnd = io.fromRob.walkEnd || robWalkEndReg

  when(io.redirect.valid) {
    robWalkEndReg := false.B
  }.elsewhen(io.fromRob.walkEnd) {
    robWalkEndReg := true.B
  }

  // There are two uops mapped to one vset inst.
  // Only record the last here.
  private val needAllocVec = VecInit(io.req.map(req => req.valid && req.bits.isVset && req.bits.lastUop))
  private val enqCount = PopCount(needAllocVec)

  private val commitCount   = Wire(UInt(log2Up(CommitWidth).W))
  private val walkCount     = Wire(UInt(log2Up(CommitWidth).W))
  private val spclWalkCount = Wire(UInt(log2Up(CommitWidth).W))

  private val commitSize   = RegInit(0.U(log2Up(size).W))
  private val walkSize     = RegInit(0.U(log2Up(size).W))
  private val spclWalkSize = RegInit(0.U(log2Up(size).W))

  private val commitSizeNext   = Wire(UInt(log2Up(CommitWidth).W))
  private val walkSizeNext     = Wire(UInt(log2Up(CommitWidth).W))
  private val spclWalkSizeNext = Wire(UInt(log2Up(CommitWidth).W))

  private val newCommitSize   = io.fromRob.commitSize
  private val newWalkSize     = io.fromRob.walkSize
  private val newSpclWalkSize = Mux(io.redirect.valid && !io.snpt.useSnpt, commitSizeNext, 0.U)

  commitSizeNext   := commitSize + newCommitSize - commitCount
  walkSizeNext     := walkSize + newWalkSize - walkCount
  spclWalkSizeNext := spclWalkSize + newSpclWalkSize - spclWalkCount

  commitSize := Mux(io.redirect.valid && !io.snpt.useSnpt, 0.U, commitSizeNext)
  spclWalkSize := spclWalkSizeNext
  walkSize := Mux(io.redirect.valid && !io.snpt.useSnpt, 0.U, walkSizeNext)

  walkPtrNext := MuxCase(walkPtr, Seq(
    (state === s_idle && stateNext === s_walk) -> walkPtrSnapshots(snptSelect),
    (state === s_spcl_walk && stateNext === s_walk) -> deqPtrVecNext.head,
    (state === s_walk && io.snpt.useSnpt && io.redirect.valid) -> walkPtrSnapshots(snptSelect),
    (state === s_walk) -> (walkPtr + walkCount),
  ))

  walkPtr := walkPtrNext

  // update enq ptr
  private val enqPtrNext = Mux(
    state === s_walk && stateNext === s_idle,
    walkPtrNext,
    enqPtr + enqCount
  )

  private val enqPtrOHNext = Mux(
    state === s_walk && stateNext === s_idle,
    walkPtrNext.toOH,
    enqPtrOHVec(enqCount)
  )

  enqPtrOH := enqPtrOHNext
  enqPtrVecNext.zipWithIndex.map{ case(ptr, i) => ptr := enqPtrNext + i.U }
  enqPtrVec := enqPtrVecNext

  // update deq ptr
  private val deqPtrSteps = Mux1H(Seq(
    (state === s_idle) -> commitCount,
    (state === s_spcl_walk) -> spclWalkCount,
  ))

  private val deqPtrNext = deqPtr + deqPtrSteps
  private val deqPtrOHNext = deqPtrOHVec(deqPtrSteps)
  deqPtrOH := deqPtrOHNext
  deqPtrVecNext.zipWithIndex.map{ case(ptr, i) => ptr := deqPtrNext + i.U }
  deqPtrVec := deqPtrVecNext

  private val allocPtrVec: Vec[VTypeBufferPtr] = VecInit((0 until RenameWidth).map(i => enqPtrVec(PopCount(needAllocVec.take(i)))))
  private val vtypeBufferReadPtrVecNext: Vec[VTypeBufferPtr] = Mux1H(Seq(
    (stateNext === s_idle) -> VecInit(deqPtrVecNext ++ VecInit(0.U.asTypeOf(deqPtrVecNext.head))),
    (stateNext === s_walk) -> VecInit(walkPtrVecNext ++ VecInit((walkPtrNext - 1.U))),
    (stateNext === s_spcl_walk) -> VecInit(deqPtrVecNext ++ VecInit(0.U.asTypeOf(deqPtrVecNext.head))),
  ))

  /**
   * connection of [[vtypeBuffer]]
   */
  vtypeBufferWriteAddrVec := allocPtrVec.map(_.value)
  vtypeBufferWriteEnVec := needAllocVec
  vtypeBufferWriteDataVec.zip(io.req.map(_.bits)).foreach { case (entry: VTypeBufferEntry, inst) =>
    entry.vtype := inst.vpu.vtype
  }
  vtypeBufferReadAddrVec := vtypeBufferReadPtrVecNext.map(_.value)

  private val commitValidVec = Wire(Vec(CommitWidth, Bool()))
  private val walkValidVec = Wire(Vec(CommitWidth, Bool()))
  private val infoVec = Wire(Vec(CommitWidth, VType()))
  private val walkInitVType = Wire(VType())

  for (i <- 0 until CommitWidth) {
    commitValidVec(i) := state === s_idle && i.U < commitSize || state === s_spcl_walk && i.U < spclWalkSize
    walkValidVec(i) := state === s_walk && i.U < walkSize || state === s_spcl_walk && i.U < spclWalkSize

    infoVec(i) := vtypeBufferReadDataVec(i).vtype
  }
  walkInitVType := vtypeBufferReadDataVec.last.vtype

  commitCount   := Mux(state === s_idle,      PopCount(commitValidVec), 0.U)
  walkCount     := Mux(state === s_walk,      PopCount(walkValidVec), 0.U)
  spclWalkCount := Mux(state === s_spcl_walk, PopCount(walkValidVec), 0.U)

  private val walkEndNext = walkSizeNext === 0.U
  private val spclWalkEndNext = spclWalkSizeNext === 0.U

  state := stateNext

  when (io.redirect.valid) {
    when (io.snpt.useSnpt) {
      stateNext := s_walk
    }.otherwise {
      stateNext := s_spcl_walk
    }
  }.otherwise {
    switch (state) {
      is(s_idle) {
        stateNext := s_idle
      }
      is(s_spcl_walk) {
        when (spclWalkEndNext) {
          stateNext := s_walk
        }
      }
      is(s_walk) {
        when (robWalkEnd && walkEndNext) {
          stateNext := s_idle
        }
      }
    }
  }

  val numValidEntries = distanceBetween(enqPtr, deqPtr)
  val allowEnqueue = GatedValidRegNext(
    numValidEntries + enqCount <= (size - RenameWidth).U,
    true.B
  )

  private val decodeResumeVType = Reg(ValidIO(VType()))
  private val newestVType = PriorityMux(walkValidVec.zip(infoVec).map { case(walkValid, info) => walkValid -> info }.reverse)
  when (reset.asBool) {
    decodeResumeVType.valid := false.B
  }.elsewhen (state === s_walk && stateLastCycle =/= s_walk) {
    decodeResumeVType.valid := true.B
    decodeResumeVType.bits := Mux(walkCount =/= 0.U, newestVType, walkInitVType)
  }.elsewhen (state === s_walk && stateLastCycle === s_walk && walkCount =/= 0.U) {
    decodeResumeVType.valid := true.B
    decodeResumeVType.bits := newestVType
  }.otherwise {
    decodeResumeVType.valid := false.B
  }

  io.canEnq := allowEnqueue && state === s_idle
  io.status.walkEnd := walkEndNext
  // update vtype in decode when VTypeBuffer resumes from walk state
  // note that VTypeBuffer can still send resuming request in the first cycle of s_idle
  io.toDecode.isResumeVType := state =/= s_idle || decodeResumeVType.valid
  io.toDecode.walkVType.valid := (state === s_walk || stateLast === s_walk && state === s_idle) && decodeResumeVType.valid
  io.toDecode.walkVType.bits := Mux(io.toDecode.walkVType.valid, decodeResumeVType.bits, 0.U.asTypeOf(VType()))

  private val newestArchVType = PriorityMux(commitValidVec.zip(infoVec).map { case(commitValid, info) => commitValid -> info }.reverse)
  io.toDecode.commitVType.valid := commitValidVec.asUInt.orR
  io.toDecode.commitVType.bits := newestArchVType

  XSError(isBefore(enqPtr, deqPtr) && !isFull(enqPtr, deqPtr), "\ndeqPtr is older than enqPtr!\n")

  QueuePerf(size, numValidEntries, numValidEntries === size.U)

  XSPerfAccumulate("s_idle_to_idle", state === s_idle      && stateNext === s_idle)
  XSPerfAccumulate("s_idle_to_swlk", state === s_idle      && stateNext === s_spcl_walk)
  XSPerfAccumulate("s_idle_to_walk", state === s_idle      && stateNext === s_walk)
  XSPerfAccumulate("s_swlk_to_idle", state === s_spcl_walk && stateNext === s_idle)
  XSPerfAccumulate("s_swlk_to_swlk", state === s_spcl_walk && stateNext === s_spcl_walk)
  XSPerfAccumulate("s_swlk_to_walk", state === s_spcl_walk && stateNext === s_walk)
  XSPerfAccumulate("s_walk_to_idle", state === s_walk      && stateNext === s_idle)
  XSPerfAccumulate("s_walk_to_swlk", state === s_walk      && stateNext === s_spcl_walk)
  XSPerfAccumulate("s_walk_to_walk", state === s_walk      && stateNext === s_walk)

  dontTouch(enqPtrVec)
  dontTouch(deqPtrVec)
  dontTouch(deqPtr)
  dontTouch(numValidEntries)
  dontTouch(commitCount)
  dontTouch(walkCount)
  dontTouch(spclWalkCount)
  dontTouch(commitSize)
  dontTouch(walkSize)
  dontTouch(spclWalkSize)
  dontTouch(commitSizeNext)
  dontTouch(walkSizeNext)
  dontTouch(spclWalkSizeNext)
  dontTouch(newCommitSize)
  dontTouch(newWalkSize)
  dontTouch(newSpclWalkSize)
  dontTouch(commitValidVec)
  dontTouch(walkValidVec)
  dontTouch(infoVec)
}







