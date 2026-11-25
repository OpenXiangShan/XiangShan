/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] James E. Smith, and Andrew R. Pleszkun. "[Implementation of precise interrupts in pipelined processors.]
* (https://dl.acm.org/doi/10.5555/327010.327125)" 12th Annual International Symposium on Computer Architecture (ISCA).
* 1985.
***************************************************************************************/

package xiangshan.backend.rob

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import difftest.common._
import difftest.plugin.topdown._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import utils._
import xiangshan._
import xiangshan.PerfDebugInfo
import xiangshan.backend.{BackendParams, Bundles, GPAMemEntry, RatToVecExcpMod, RegWriteFromRab, VecExcpInfo}
import xiangshan.backend.Bundles._
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.NewCSR.CSREvents.TargetPCBundle
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import xiangshan.backend.ctrlblock.{DebugLSIO, DebugLsInfo, LsTopdownInfo}
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.rename.{SnapshotGenerator, CompressType, NoCompressReason}
import xiangshan.backend.trace._
import chisel3.experimental.BundleLiterals._
import chisel3.util.experimental.decode.TruthTable
import xiangshan.TopDownCounters._
import xiangshan.backend.dispatch._
import xiangshan.backend.rob.Rob._

class Rob(params: BackendParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  lazy val module = new RobImp(this)(p, params)
}

class RobImp(override val wrapper: Rob)(implicit p: Parameters, params: BackendParams) extends LazyModuleImp(wrapper)
  with HasXSParameter with HasCircularQueuePtrHelper with HasPerfEvents with HasCriticalErrors {

  private val LduCnt = params.LduCnt
  private val StaCnt = params.StaCnt
  require(RenameWidth <= CommitWidth)

  val in = IO(new In(params))
  val out = IO(new Out(params))

  override lazy val io_perf: Vec[PerfEvent] = out.perfEvents
  override lazy val io_error: Vec[Bool] = out.criticalErrors

  /**
   * Rob's sub-module: Rename Buffer and VType Buffer
   */
  val rab = Module(new RenameBuffer(RabSize))
  val vtypeBuffer = Module(new VTypeBuffer(VTypeBufferSize))

  /**
   * Banked ROB Entries
   */
  val robEntries = RegInit(VecInit.fill(RobSize)((new RobEntryBundle).Lit(_.valid -> false.B)))
  private val enqBanking = new RobBanking(RobSize, RenameWidth)
  private val deqBanking = new RobBanking(RobSize, CommitWidth)
  require(isPow2(CommitWidth), "ROB commit rows require a power-of-two CommitWidth")
  // Per-slot functional state used by exception handling and performance
  // accounting.
  // TODO: move slotUopBuf back to RoB entries
  val slotUopBuf = Reg(Vec(RobSize, Vec(2, new RobSlotInfo)))

  /**
   * ROB Pointers and Allocation
   */
  // Enqueue: consecutive allocation candidates starting at the ROB tail (enqPtr).
  val enqPtrVec = Wire(Vec(RenameWidth, new RobPtr))
  val enqPtr = enqPtrVec(0)

  // For each lane, advance the candidate pointer only for earlier valid first uops.
  val allocatePtrVec = VecInit((0 until RenameWidth).map { i =>
    enqPtrVec(PopCount(in.enq.req.take(i).map(req => req.valid && req.bits.firstUop)))
  })

  // Dequeue: the current commit candidates start at the ROB head (deqPtr).
  val deqPtrVec = Wire(Vec(CommitWidth, new RobPtr))
  val deqPtr = deqPtrVec.head

  // Dequeue pointers for the next cycle after the current commit decision.
  val deqPtrVecNext = Wire(Vec(CommitWidth, Output(new RobPtr)))
  // Let outside know Rob's current deqPtr
  out.robDeqPtr := deqPtr

  // When enqPtr equal deqPtr, rob is empty.
  // When their value is the same but have different flag, rob is full.
  val isEmpty = enqPtr.isSameEntry(deqPtr)

  // Width of an entry offset within a commit row.
  val commitOffsetWidth = log2Ceil(CommitWidth)

  // Walk: read one row at a time. walkPtr points to the first entry in the current row.
  val walkPtrVec = Reg(Vec(CommitWidth, new RobPtr))
  val walkPtr = walkPtrVec(0)
  // Next cycle's read pointers. On redirect, walkPtrNext selects the bank row.
  val walkPtrVecNext = Wire(Vec(CommitWidth, Output(new RobPtr)))
  val walkPtrNext = walkPtrVecNext(0)
  // Delay the read pointers by one cycle to align them with walkValid.
  val walkingPtrVec = RegNext(walkPtrVec)

  // Exact current and next walk positions, including the slot. The start can
  // be inside the first row read.
  val walkPtrTrue = Reg(new RobPtr)
  val walkPtrTrueNext = Wire(new RobPtr)
  // Inclusive limit from the redirect. walkFinished is true once walkPtrTrue passes it.
  val lastWalkPtr = Reg(new RobPtr)
  // Starting entry offset, used to mask earlier entries in the first row.
  val walkPtrLowBits = Reg(UInt(commitOffsetWidth.W))

  // Without a snapshot, align the next dequeue pointer to a row and enumerate its entries.
  val deqPtrVecForWalk = VecInit(
    (0 until CommitWidth).map(_.U).map(deqPtrVecNext(0).lineHeadPtr.addEntries)
  )

  // Allocation controls.
  val allowEnqueue = RegInit(true.B)
  val allowEnqueueForDispatch = RegInit(true.B)
  val hasBlockBackward = RegInit(false.B)
  val hasWaitForward = RegInit(false.B)

  /**
   * Enqueue (from dispatch)
   */

  // Dispatched uops reach the ROB one cycle after they are from dispatch stage, see CtrlBlock.
  // "canAcceptForDispatch" admits the batch at the dispatch stage.
  // "canAccept" admits the batch already at the ROB enq port.
  in.enq.canAcceptForDispatch := allowEnqueueForDispatch && !hasBlockBackward && rab.in.canEnqForDispatch &&
    vtypeBuffer.in.canEnqForDispatch && !in.fromVecExcpMod.busy
  in.enq.canAccept := allowEnqueue && !hasBlockBackward && rab.in.canEnq &&
    vtypeBuffer.in.canEnq && !in.fromVecExcpMod.busy
  in.enq.isEmpty := RegNext(isEmpty && !VecInit(in.enq.req.map(_.valid)).asUInt.orR)

  // Due to rob compression, a uop may enq rob but share an entry with other uops.
  // In other words, some uops may not allocate a new ROB entry and belong to an existing one.

  // Accepted uops: including those that continue an existing entry.
  val enqFireVec = VecInit(in.enq.req.map(req => req.valid && in.enq.canAccept))
  // Allocated uops: only the first uop allocates an entry and supplies its entry metadata.
  val enqFirstUopFireVec = VecInit(in.enq.req.zip(enqFireVec).map { case (req, fire) =>
    fire && req.bits.firstUop
  })

  // Allocation follows the assigned pointer; other uops already carry their ROB index.
  // Compare with rob's allocate robIdx.
  // One hot that indicates which allocation port hits this entry.
  private def allocatingOHFor(entryIdx: UInt): Vec[Bool] =
    VecInit(enqFirstUopFireVec.zip(allocatePtrVec).map { case (fire, ptr) =>
      fire && ptr.value === entryIdx
    })

  // Compare with enq req's robIdx.
  // Multi-hot that means this enq req hits the given rob entry.
  private def enqUopHitsFor(entryIdx: UInt): Seq[Bool] =
    enqFireVec.zip(in.enq.req).map { case (fire, req) =>
      fire && req.bits.robIdx.value === entryIdx
    }

  // The uop that writes the rob entry.
  // Only the first uop of a batch will write the rob entry (for most fields).
  val allocatingOHByEntry = (0 until RobSize).map(i => allocatingOHFor(i.U))
  val allocatingByEntry = allocatingOHByEntry.map(_.asUInt.orR)
  // The uops that belong to the rob entry.
  // One rob entry may be hit by multiple uops due to rob compression.
  val enqUopHitsByEntry = (0 until RobSize).map(i => enqUopHitsFor(i.U))

  // Share enqueue payload selection using a view sized for the allocation bandwidth.
  val enqBankSelectUops =
    enqBanking.bankWriteSelect(enqFirstUopFireVec.toSeq, allocatePtrVec.map(_.value), in.enq.req.map(_.bits))

  // Write the ROB entries
  for (i <- 0 until RobSize) {
    // Should not be allocated twice
    assert(PopCount(allocatingOHByEntry(i)) < 2.U, s"ROB entry $i allocation lanes are not one-hot")

    when(allocatingByEntry(i) && !in.redirect.valid) {
      robEntries(i).connectEnq(enqBankSelectUops(i))

      // uop belong to this rob entry
      val uopsBelongEntry = enqUopHitsByEntry(i).zip(in.enq.req.map(_.bits))
      // Select the youngest uop that hits this rob entry to update the traceBlockInPipe fields.
      val lastUop = ParallelPosteriorityMux(uopsBelongEntry)
      robEntries(i).traceBlockInPipe.itype := lastUop.traceBlockInPipe.itype
      robEntries(i).traceBlockInPipe.ilastsize := lastUop.traceBlockInPipe.ilastsize

      // Update the formerTraceBlockInPipe fields for the uop that is marked as former.
      robEntries(i).formerTraceBlockInPipe.iretire := enqBankSelectUops(i).formerTraceIretire
      // The allocating uop is in the former slot, so the selection is never empty here.
      val formerUopsBelongEntry = uopsBelongEntry.map { case (hit, uop) =>
        (hit && uop.robIdx.isFormer, uop)
      }
      val lastFormerUop = ParallelPosteriorityMux(formerUopsBelongEntry)
      robEntries(i).formerTraceBlockInPipe.itype := lastFormerUop.traceBlockInPipe.itype
      robEntries(i).formerTraceBlockInPipe.ilastsize := lastFormerUop.traceBlockInPipe.ilastsize
    }
  }

  // when enq, update rab which records the logic and physical register mapping
  rab.in.req.zip(in.enq.req).zip(enqFireVec).foreach { case ((dest, src), fire) =>
    dest.bits := src.bits
    dest.valid := fire
  }

  /**
   * Dequeue for commit and recovery walk
   *
   * Commit and walk share a two-cycle read path:
   *
   * 1. Select: `deqBankRaddr` reads the current and next rows into `deqBankRdata`.
   *    Forward enqueue, writeback, and redirect changes through `deqBankRdataUpdate`.
   *    `updateDeqEntries` registers one row in `deqEntriesBankOrder` in bank order.
   *    When commit finishes the current row, it registers the next row.
   * 2. Consume: The low bits of `deqPtrVec` select `commitEntries` in dequeue order.
   *    Commit checks which entries can retire. Walk uses the low bits of
   *    `walkPtrVec` to select `walkEntries` and `walkValid` to qualify recovery.
   *
   * The read address tracks the row containing `deqPtr` during commit. On redirect,
   * `walkPtrNext` selects the row containing the snapshot or, without a snapshot,
   * the next dequeue pointer. The address advances by row during walk and returns
   * to the `deqPtr` row after reading the last walk row.
   */

  /**
   * Dequeue select stage
   *
   * Signal declaration
   */

  // dequeue bank one-hot row selector
  val deqBankRaddr = RegInit(deqBanking.firstRowSelect)
  val deqBankRaddrNext = WireInit(deqBankRaddr)
  deqBankRaddr := deqBankRaddrNext

  // dequeue bank read data (CommitWidth * 2 entries)
  val deqBankRdata =
    deqBanking.readRows(robEntries.toSeq, deqBankRaddr, backendParams.debugEn)
  // The Update suffix marks read data with same-cycle updates for deqEntriesBankOrder.
  // This data does not drive the next state of robEntries.
  val deqBankRdataUpdate = WireInit(deqBankRdata)

  // Match forwarding to the rows actually read, including during recovery walk.
  val rdataUpdateRobIdx = deqBanking.rowIndices(deqBankRaddr)

  /**
   * Commit and walk stage
   *
   * Signal declaration
   */

  // The selected row in bank order, shared by commit and walk.
  val deqEntriesBankOrder = Reg(Vec(CommitWidth, new RobCommitEntryBundle))
  // Entries in dequeue order. Commit eligibility is checked separately.
  val commitEntries = (0 until CommitWidth).map { i =>
    deqEntriesBankOrder(deqPtrVec(i).value(commitOffsetWidth - 1, 0))
  }
  val commitHeadEntry = commitEntries.head
  val commitHeadValid = commitHeadEntry.commit_v

  // When find no snapshot, walk from deqPtr(oldest instruction)
  // Walk will take over deq bank addr and commit is blocked.
  val walkEntries = (0 until CommitWidth).map { i =>
    deqEntriesBankOrder(walkPtrVec(i).value(commitOffsetWidth - 1, 0))
  }

  // Commit eligibility in bank order.
  val commitValidThisLine = Wire(Vec(CommitWidth, Bool()))
  // Whether each commit entry has already committed
  val hasCommitted = RegInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  // All commit entries have committed
  val allCommitted = Wire(Bool())
  // Track the length of committed instructions before the exception
  val formerLenCommit = Wire(Vec(CommitWidth, UInt(log2Ceil(RenameWidth * 4 + 1).W)))

  /**
   * Dequeue select stage
   *
   * Select and pointer bump
   */

  // Determine deqBankRaddrNext to update deqBankRaddr
  val changeBankAddrToDeqPtr = walkPtr.addEntries(CommitWidth.U).isAfterSlot(lastWalkPtr)
  when(in.redirect.valid) {
    // Use walkPtrNext so the read address and the recovery pointer update
    // on the same clock edge.
    deqBankRaddrNext := deqBanking.rowSelect(walkPtrNext.value)
  }.elsewhen(allCommitted || out.commits.isWalk && !changeBankAddrToDeqPtr) {
    // Advance one row when the current row finishes committing or the walk
    // continues into the next row.
    deqBankRaddrNext := deqBanking.nextRowSelect(deqBankRaddr)
  }.elsewhen(out.commits.isWalk && changeBankAddrToDeqPtr) {
    // Select the dequeue row for commit when the next row starts past the walk limit.
    deqBankRaddrNext := deqBanking.rowSelect(deqPtr.value)
  }

  /**
   * Commit checks
   */

  // update commit status.
  allCommitted := out.commits.isCommit && commitValidThisLine.last
  when(allCommitted) {
    // if allCommitted is true, then let next line come in, hasCommitted will be reset
    hasCommitted := 0.U.asTypeOf(hasCommitted)
  }.elsewhen(out.commits.isCommit){
    for (i <- 0 until CommitWidth){
      hasCommitted(i) := commitValidThisLine(i) || hasCommitted(i)
    }
  }

  /**
   * Writeback (from execution units)
   */
  val exuWBs: Seq[ValidIO[WriteBackRobBundle]] = in.exuWriteback
  val excpWBs = in.writeback.filter(x => x.bits.params.needExceptionGen).toSeq
  val redirectWBs = in.writeback.filter(x => x.bits.redirect.nonEmpty).toSeq
  val branchWBs = in.exuWriteback.filter(_.bits.params.hasBrhFu).toSeq
  // val csrWBs = in.exuWriteback.filter(x => x.bits.params.hasCSR).toSeq
  // val ldWBs = in.exuWriteback.filter(x => x.bits.params.hasLoadFu).toSeq

  // The entry-level trace block describes the youngest instruction in the
  // surviving compressed entry, while debug/difftest keep both slots exact.
  for (wb <- branchWBs) {
    val wbIdx = wb.bits.robIdx.value
    when(wb.valid && wb.bits.robIdx.isFormer && wb.bits.redirect.get.bits.taken &&
      Itype.isNonTaken(robEntries(wbIdx).formerTraceBlockInPipe.itype)) {
      robEntries(wbIdx).formerTraceBlockInPipe.itype := Itype.Taken
    }
  }

  /**
   * Walk when trap, branch misprediction and memory violation occur.
   */

  // Shared walk signals.
  // The ROB has reached the end once its pointer passes the inclusive walk limit.
  val walkFinished = walkPtrTrue.isAfterSlot(lastWalkPtr)
  // Mask all lanes while the read is prepared, then skip lanes before the walk start.
  val donotNeedWalk = RegInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  // Per-lane walk validity for the row read in the previous cycle.
  val shouldWalkVec = Wire(Vec(CommitWidth, Bool()))

  /**
   * Walk state
   *
   * s_idle: normal state
   * s_walk: walk state, enqueue and commit will be blocked
   *
   * State transition:
   * (1) redirect: switch to s_walk
   * (2) walk: when walking comes to the end, switch to s_idle
   */
  val s_idle :: s_walk :: Nil = Enum(2)
  val state = RegInit(s_idle)
  val stateNext = Wire(chiselTypeOf(state))

  val normalState = state === s_idle
  val walkState = state === s_walk

  stateNext := Mux(
    // when redirect is valid or was valid in the previous cycle, switch to s_walk
    in.redirect.valid || RegNext(in.redirect.valid),
    s_walk,
    Mux(
      // when walking is finished and both rab and vtypeBuffer indicate walk end, switch to s_idle
      walkState && walkFinished &&
        rab.in.status.walkEnd && vtypeBuffer.in.status.walkEnd,
      s_idle,
      state
    )
  )
  state := stateNext

  /**
   * Recovery snapshots
   *
   * Rob's snapshot records the robIdx when instructiuon is enqueued
   */
  val snptEnq = in.enq.canAccept && in.enq.req.map(x => x.valid && x.bits.snapshot).reduce(_ || _)
  val snapshotPtrVec = Wire(Vec(CommitWidth, new RobPtr))
  snapshotPtrVec(0) := in.enq.req(0).bits.robIdx
  for (i <- 1 until CommitWidth) {
    snapshotPtrVec(i) := snapshotPtrVec(0).addEntries(i.U).asFormer
  }
  // TODO: currently, only the first entry is used, rm the rest
  val snapshots = SnapshotGenerator(
    enqData = snapshotPtrVec,
    enq = snptEnq,
    deq = in.snpt.snptDeq,
    redirect = in.redirect.valid,
    flushVec = in.snpt.flushVec
  )

  val realDestSizeSeq = VecInit(deqEntriesBankOrder.zip(hasCommitted).map{case (r, h) => Mux(h, 0.U, r.realDestSize)})
  val walkDestSizeSeq = VecInit(deqEntriesBankOrder.zip(donotNeedWalk).map{case (r, d) => Mux(d, 0.U, r.realDestSize)})
  val commitSizeSumSeq = VecInit((0 until CommitWidth).map(i => realDestSizeSeq.take(i + 1).reduce(_ +& _)))
  val walkSizeSumSeq   = VecInit((0 until CommitWidth).map(i => walkDestSizeSeq.take(i + 1).reduce(_ +& _)))
  val commitSizeSumCond = VecInit(commitValidThisLine.zip(hasCommitted).map{case (c,h) => (c || h) && out.commits.isCommit})
  val walkSizeSumCond   = VecInit(out.commits.walkValid.zip(donotNeedWalk).map{case (w,d) => (w || d) && out.commits.isWalk})
  val commitSizeSum = PriorityMuxDefault(commitSizeSumCond.reverse.zip(commitSizeSumSeq.reverse), 0.U)
  val walkSizeSum   = PriorityMuxDefault(walkSizeSumCond.reverse.zip(walkSizeSumSeq.reverse), 0.U)

  val deqVlsExcpNeedCommit = RegInit(false.B)
  val deqVlsExcpCommitSize = RegInit(0.U(log2Up(MaxUopSize + 1).W))
  val deqVlsCanCommit= RegInit(false.B)

  // Connect ROB's redirect signal to RAB
  rab.in.redirect := in.redirect.valid
  rab.in.fromRob.commitSize := Mux(deqVlsExcpNeedCommit, deqVlsExcpCommitSize, commitSizeSum)
  rab.in.fromRob.walkSize := walkSizeSum
  rab.in.snpt := in.snpt
  rab.in.snpt.snptEnq := snptEnq

  // pipe rab commits for better timing and area
  out.rabCommits := RegNext(rab.out.commits)
  out.vlCommits := RegNext(vtypeBuffer.out.commits)

  /**
   * VType buffer interface
   */
  vtypeBuffer.in.req.zip(in.enq.req).zip(enqFireVec).foreach { case ((sink, source), fire) =>
    sink.valid := fire
    sink.bits := source.bits
  }

  // Whether commit inst need Vtype buffer
  private val commitIsVTypeVec = VecInit(out.commits.commitValid.zip(out.commits.info).map {
    case (valid, info) =>
      out.commits.isCommit && valid && info.needVTB
  })
  // Whether walk inst need Vtype buffer
  private val walkIsVTypeVec = VecInit(out.commits.walkValid.zip(walkEntries).map {
    case (valid, info) => out.commits.isWalk && valid && info.needVTB
  })
  vtypeBuffer.in.fromRob.commitSize := PopCount(commitIsVTypeVec)
  vtypeBuffer.in.fromRob.walkSize := PopCount(walkIsVTypeVec)
  vtypeBuffer.in.redirect := in.redirect.valid
  vtypeBuffer.in.snpt := in.snpt
  vtypeBuffer.in.snpt.snptEnq := snptEnq

  out.toDecode.walkToArchVType := vtypeBuffer.out.toDecode.walkToArchVType
  out.toDecode.commitVType := vtypeBuffer.out.toDecode.commitVType
  out.toDecode.walkVType := vtypeBuffer.out.toDecode.walkVType

  /**
   * Dispatch serialization and WFI
   */
  // Allow enqueue again once the ROB is empty.
  when(isEmpty) {
    hasBlockBackward := false.B
  }
  // Clear waitForward when any instruction commits or walks.
  when(out.commits.hasWalkInstr || out.commits.hasCommitInstr) {
    hasWaitForward := false.B
  }

  val enqUopsWithFire = enqFireVec.zip(in.enq.req.map(_.bits))
  val enqHasBlockBackward = VecInit(enqUopsWithFire.map { case (fire, uop) =>
    fire && uop.blockBackward
  }).asUInt.orR
  val enqHasWaitForward = VecInit(enqUopsWithFire.map { case (fire, uop) =>
    fire && uop.waitForward
  }).asUInt.orR

  when(enqHasBlockBackward) {
    hasBlockBackward := true.B
  }
  when(enqHasWaitForward) {
    hasWaitForward := true.B
  }

  // The wait-for-interrupt (WFI) instruction waits in the ROB until an interrupt might need servicing.
  // in.csr.wfiEvent will be asserted if the WFI can resume execution, and we change the state to s_wfi_idle.
  // It does not affect how interrupts are serviced. Note that WFI is noSpecExec and it does not trigger interrupts.
  val hasWFI = RegInit(false.B)
  val wfiSafe = out.wfi.safeFromMem && out.wfi.safeFromFrontend
  out.wfi.wfiReq := hasWFI
  out.cpuWfi := hasWFI && wfiSafe

  // WFI Timeout: 2^20 = 1M cycles
  val wfiCycles = RegInit(0.U(20.W))
  if (wfiResume) {
    when(hasWFI) {
      wfiCycles := wfiCycles + 1.U
    }.elsewhen(!hasWFI && RegNext(hasWFI)) {
      wfiCycles := 0.U
    }
  }

  // When wfi time out, clear WFI
  val wfiTimeout = wfiCycles.andR
  when(RegNext(RegNext(in.csr.wfiEvent)) || out.flushOut.valid || wfiTimeout) {
    hasWFI := false.B
  }

  // When enq has wfi ,set WFI.
  val enqHasWFI = VecInit(enqUopsWithFire.map { case (fire, uop) =>
    fire && uop.isWFI && !uop.exceptionVec.orR
  }).asUInt.orR
  when(enqHasWFI) {
    hasWFI := true.B
  }

  when(!in.wfiEnable) {
    hasWFI := false.B
  }

  /**
   * Trap: Interrupt and Exceptions
   */
  // ExceptionGen accept exception enqueue and writeback, and generate oldest exception
  val excpGen = Module(new ExceptionGen(params))
  val excpState = excpGen.out.state

  // Slot-specific views of the head entry for exception and flush checks.
  val deqPtrFormer = WireInit(deqPtr).asFormer
  val deqPtrLatter = WireInit(deqPtr).asLatter

  val deqHasFlushed = RegInit(false.B)
  val formerNotFlushed = !deqPtrFormer.needFlush(in.redirect)
  val latterNotFlushed = !deqPtrLatter.needFlush(in.redirect)

  // Interrupt enable/pending signal.
  val intrBitSetReg = RegNext(in.csr.intrBitSet)
  val intrEnable = Wire(Bool())
  intrEnable := intrBitSetReg && !hasWaitForward && commitHeadEntry.interruptSafe && !deqHasFlushed

  // Exception infomation. Use the dequeue slot's info
  // TODO: move slot uop back to rob but notice the read port.
  val deqExcpIsFormer = intrEnable || commitHeadEntry.slotNeedFlush(0)
  val excpUopInfo = Mux(deqExcpIsFormer, slotUopBuf(deqPtr.value)(0), slotUopBuf(deqPtr.value)(1))
  // Select the exact slot (former/latter) that should provide exception state for the head entry.
  val deqExcpRobIdx = Mux(commitHeadEntry.slotNeedFlush(0), deqPtrFormer, deqPtrLatter)

  // Forward vector load exception metadata to the rename buffer.
  rab.in.fromRob.vecLoadExcp.valid := RegNext(excpState.valid && excpState.bits.isVecLoad)
  rab.in.fromRob.vecLoadExcp.bits.isStrided := RegEnable(excpState.bits.isStrided, excpState.valid)
  rab.in.fromRob.vecLoadExcp.bits.isVlm := RegEnable(excpState.bits.isVlm, excpState.valid)

  // Why delay one cycle?
  val commit_w_delay = RegNext(commitHeadEntry.commit_w)
  val deqNeedFlush = (
    commitHeadEntry.slotNeedFlush(0) && formerNotFlushed ||
    commitHeadEntry.slotNeedFlush(1) &&
      CompressType.isNotNORMAL(commitHeadEntry.entryPairType) &&
      latterNotFlushed
  ) && commitHeadEntry.commit_v && commitHeadEntry.commit_w
  val deqNeedFlushAndHitExcpGenState = deqNeedFlush && excpState.valid && excpState.bits.robIdx.isSameSlot(deqExcpRobIdx)

  val excpGenStateIsExcp = excpState.bits.excpVec.orR
  val deqHasExcp = deqNeedFlushAndHitExcpGenState && excpGenStateIsExcp && commit_w_delay
  val deqHasFlushPipe = deqNeedFlushAndHitExcpGenState && excpState.bits.flushPipe && !deqHasExcp && commit_w_delay
  val deqHasReplayInst = deqNeedFlushAndHitExcpGenState && excpState.bits.replayInst

  /**
   * VLS exception
   */

  // delay 2 cycle wait excpGen out
  // vls exception can be committed only when RAB commit all its reg pairs
  deqVlsCanCommit := RegNext(commit_w_delay) && rab.in.status.commitEnd
  val deqIsVlsExcp = deqHasExcp && excpUopInfo.isVectorMemory && !excpState.bits.isEnqExcp
  val deqIsVlsFlushPipe = deqHasFlushPipe && excpUopInfo.isVectorMemory
  val deqCanExcp = deqHasExcp && (!deqIsVlsExcp || deqVlsCanCommit)
  val deqCanFlushPipe = deqHasFlushPipe && (!deqIsVlsFlushPipe || deqVlsCanCommit)

  // lock at assertion of deqVlsExcpNeedCommit until condition not assert
  val deqVlsExcpLock = RegInit(false.B)
  val handleVlsExcp = deqIsVlsExcp && deqVlsCanCommit && !deqVlsExcpLock && normalState
  when(handleVlsExcp) {
    deqVlsExcpLock := true.B
  }.elsewhen(deqPtrVec.head =/= deqPtrVecNext.head) {
    deqVlsExcpLock := false.B
  }

  // Only assert once when deqVlsExcp occurs until condition not assert to avoid multi message passed to RAB
  when (deqVlsExcpNeedCommit) {
    deqVlsExcpNeedCommit := false.B
  }.elsewhen(handleVlsExcp){
    deqVlsExcpCommitSize := commitHeadEntry.realDestSize
    deqVlsExcpNeedCommit := true.B
  }

  /**
   * Flush pipe
   */


  // vsetvl instruction need another one cycle to write to vtype gen
  val isVsetFlushPipe = commitHeadEntry.commit_w && deqHasFlushed && excpState.bits.isVset
  val isVsetFlushPipeReg = RegNext(isVsetFlushPipe)
  out.toDecode.isResumeVType := vtypeBuffer.out.toDecode.isResumeVType || isVsetFlushPipeReg

  // out.flushOut will trigger redirect at the next cycle.
  // Block any redirect or commit at the next cycle.
  val lastCycleFlush = RegNext(out.flushOut.valid)
  out.flushOut.valid := normalState && commitHeadValid && (intrEnable || deqCanExcp || deqCanFlushPipe) && !lastCycleFlush
  out.flushOut.bits := DontCare
  val flushIsFormer = intrEnable || commitHeadEntry.slotNeedFlush(0)
  val flushBaseIsRVC = Mux(flushIsFormer, commitHeadEntry.slotHeadRvcMask(0), Mux(commitHeadEntry.predTaken, commitHeadEntry.slotHeadRvcMask(1), commitHeadEntry.slotHeadRvcMask(0)))
  out.flushOut.bits.isRVC := flushBaseIsRVC
  out.flushOut.bits.robIdx := deqPtr
  out.flushOut.bits.robIdx.isFormer := flushIsFormer
  out.flushOut.bits.ftqIdx := Mux(
    flushIsFormer,
    commitHeadEntry.ftqIdx,
    Mux(commitHeadEntry.predTaken, commitHeadEntry.ftqIdx + 1.U, commitHeadEntry.ftqIdx)
  )
  out.flushOut.bits.ftqOffset := Mux(
    flushIsFormer,
    commitHeadEntry.ftqOffset,
    Mux(
      commitHeadEntry.predTaken,
      Mux(commitHeadEntry.slotHeadRvcMask(1), 0.U.asTypeOf(commitHeadEntry.ftqOffset), 1.U.asTypeOf(commitHeadEntry.ftqOffset)),
      commitHeadEntry.ftqOffset
    )
  )
  out.flushPcInfo.formerLen := Mux(
    flushIsFormer || commitHeadEntry.predTaken,
    0.U.asTypeOf(out.flushPcInfo.formerLen),
    formerLenCommit(0)
  )
  out.flushPcInfo.flushIsRVC := Mux(flushIsFormer, commitHeadEntry.slotHeadRvcMask(0), commitHeadEntry.slotHeadRvcMask(1))
  // TODO: it could use the former instr's ftqIdx and ftqOffset to calculate the latter instr's PC
  // rather than calculate the latter instr's ftqIdx and ftqOffset which could be wrong!!!

  out.flushOut.bits.level := Mux(
    deqHasReplayInst || intrEnable || deqHasExcp,
    RedirectLevel.flush,
    RedirectLevel.flushAfter
  ) // TODO use this to implement "exception next"

  val isFlushPipe = commitHeadEntry.commit_w && (deqHasFlushPipe || deqHasReplayInst)
  out.flushOut.bits.interrupt := !isFlushPipe
  out.flushOut.bits.satpFlush := isFlushPipe && excpState.bits.satpFlush

  val excpHappen = normalState && commitHeadValid && (intrEnable || deqCanExcp) && !lastCycleFlush
  val excpIsFormer = deqExcpIsFormer
  out.exception.valid := RegNext(excpHappen)
  out.exception.bits.gpaddr := out.readGPAMemData.gpaddr
  out.exception.bits.isForVSnonLeafPTE := out.readGPAMemData.isForVSnonLeafPTE
  out.exception.bits.isFormer := RegEnable(excpIsFormer, excpHappen)
  out.exception.bits.exceptionVec extendFrom RegEnable(excpState.bits.excpVec, excpHappen)
  out.exception.bits.satpFlushFirstFetchFault := RegEnable(excpState.bits.satpFlushFirstFetchFault && deqHasExcp, excpHappen)
  out.exception.bits.isFetchMalAddr := RegEnable(excpState.bits.isFetchMalAddr && deqHasExcp, excpHappen)
  out.exception.bits.crossPageIPFFix := RegEnable(excpState.bits.crossPageIPFFix, excpHappen)
  out.exception.bits.isInterrupt := RegEnable(intrEnable, excpHappen)
  out.exception.bits.isHls := RegEnable(excpUopInfo.isHls, excpHappen)
  out.exception.bits.vls := RegEnable(excpUopInfo.isVectorMemory, excpHappen)
  out.exception.bits.trigger := RegEnable(excpState.bits.trigger, excpHappen)

  // Difftest drives this observation gate in the section below.
  val diffTrapPending = WireDefault(false.B)

  // data will be one cycle after valid
  out.readGPAMemAddr.valid := excpHappen
  out.readGPAMemAddr.bits.ftqPtr := excpState.bits.ftqPtr
  out.readGPAMemAddr.bits.ftqOffset := excpState.bits.ftqOffset
  out.readGPAMemAddr.bits.isRVC := excpState.bits.isRVC

  /**
   * Commits (and walk)
   * They share the same width.
   */
  // T redirect.valid, T+1 use walkPtrVec read robEntries, T+2 start walk, shouldWalkVec used in T+2
  when(in.redirect.valid){
    shouldWalkVec := 0.U.asTypeOf(shouldWalkVec)
  }.elsewhen(RegNext(in.redirect.valid)){
    shouldWalkVec := 0.U.asTypeOf(shouldWalkVec)
  }.elsewhen(walkState){
    shouldWalkVec := VecInit(walkingPtrVec.map(_.isNotAfterSlot(lastWalkPtr)).zip(donotNeedWalk).map(x => x._1 && !x._2))
  }.otherwise(
    shouldWalkVec := 0.U.asTypeOf(shouldWalkVec)
  )
  rab.in.fromRob.walkEnd := walkState && walkFinished
  vtypeBuffer.in.fromRob.walkEnd := walkState && walkFinished


  /**
   * CSR flag tracking
   */
  // update when enq
  val enqFlagTracker = Module(new RobSetFlagTracker(
    2,
    in.enq.req.size,
    CommitWidth,
    orderedUpdatePorts = true
  ))
  enqFlagTracker.in.update.zip(in.enq.req).zip(enqFireVec).foreach {
    case ((update, req), fire) =>
      update.valid := fire
      update.bits.robIdx := req.bits.robIdx
      update.bits.setMask := Cat(req.bits.dirtyVs, req.bits.dirtyFs || req.bits.fflagsWen)
  }
  enqFlagTracker.in.redirect := in.redirect
  enqFlagTracker.in.commit.zip(out.commits.commitValid).zip(out.commits.robIdx).foreach {
    case ((commit, valid), robIdx) =>
      commit.valid := out.commits.isCommit && valid
      commit.bits := robIdx
  }
  val dirtyFs = enqFlagTracker.out.commitSetMask(0)
  val dirtyVs = enqFlagTracker.out.commitSetMask(1)

  // Use the former slots of deqPtr and enqPtr for the fflags and vxsat writeback
  // window [deqPtrCmp, enqPtrCmp). It includes the head and excludes the allocation tail.
  val deqPtrCmp = WireInit(deqPtr).asFormer
  val enqPtrCmp = WireInit(enqPtr).asFormer
  private def wbSlotStillValid(robIdx: RobPtr): Bool = {
    val wbEntry = robEntries(robIdx.value) // TODO: this may cause timing issue
    wbEntry.valid && (robIdx.isFormer || CompressType.isNotNORMAL(wbEntry.entryPairType))
  }

  // update when writeback
  val fflagsWidth = 5

  private val writebackFflagsCandidates = (0 until in.writeback.size).filter { index =>
    in.writeback(index).bits.fflags.nonEmpty
  }
  private val writebackVxsatCandidates = (0 until in.writeback.size).filter { index =>
    in.writeback(index).bits.vxsat.nonEmpty
  }
  private val writebackFlagCandidates =
    Seq.fill(fflagsWidth)(writebackFflagsCandidates) :+ writebackVxsatCandidates
  val writebackFlagTracker = Module(new RobSetFlagTracker(
    fflagsWidth + 1,
    in.writeback.size,
    CommitWidth,
    writebackFlagCandidates
  ))
  writebackFlagTracker.in.update.zip(in.writeback).foreach { case (update, wb) =>
    val wbInRobWindow = wb.bits.robIdx.isNotBeforeSlot(deqPtrCmp) && wb.bits.robIdx.isBeforeSlot(enqPtrCmp)
    update.valid := wb.valid && wbInRobWindow && wbSlotStillValid(wb.bits.robIdx)
    update.bits.robIdx := wb.bits.robIdx
    update.bits.setMask := Cat(
      wb.bits.vxsat.getOrElse(false.B),
      wb.bits.fflags
        .map(flags => Mux(wb.bits.fflagsWen.getOrElse(false.B), flags, 0.U(fflagsWidth.W)))
        .getOrElse(0.U(fflagsWidth.W))
    )
  }
  writebackFlagTracker.in.redirect := in.redirect
  writebackFlagTracker.in.commit.zip(out.commits.commitValid).zip(out.commits.robIdx).foreach {
    case ((commit, valid), robIdx) =>
      commit.valid := out.commits.isCommit && valid
      commit.bits := robIdx
  }


  /**
   * Vector Exception Info
   */
  val vecExcpInfo = RegInit(Valid(new VecExcpInfo).Lit(_.valid -> false.B))
  vecExcpInfo.valid := excpHappen && !intrEnable &&
    excpState.bits.vstartEn && excpState.bits.isVecLoad &&
    !excpState.bits.isEnqExcp
  when (excpHappen) {
    vecExcpInfo.bits.nf := excpState.bits.nf
    vecExcpInfo.bits.vsew := excpState.bits.vsew
    vecExcpInfo.bits.veew := excpState.bits.veew
    vecExcpInfo.bits.vlmul := excpState.bits.vlmul
    vecExcpInfo.bits.isStride := excpState.bits.isStrided
    vecExcpInfo.bits.isIndexed := excpState.bits.isIndexed
    vecExcpInfo.bits.isWhole := excpState.bits.isWhole
    vecExcpInfo.bits.isVlm := excpState.bits.isVlm
    vecExcpInfo.bits.vstart := excpState.bits.vstart
  }

  val resetVstart = dirtyVs && !in.vstartIsZero
  out.csr.vstart.valid := RegNext(Mux(excpHappen && deqHasExcp, excpState.bits.vstartEn, resetVstart))
  out.csr.vstart.bits := RegNext(Mux(excpHappen && deqHasExcp, excpState.bits.vstart, 0.U))

  /**
   * Retirement eligibility
   */
  // when mispredict branches writeback, stop commit in the next 2 cycles
  // TODO: don't check all exu write back
  val misPredWb = Cat(VecInit(redirectWBs.map(wb =>
    wb.bits.redirect.get.bits.isMisPred && wb.bits.redirect.get.valid && wb.valid
  ).toSeq)).orR
  val misPredBlockCounter = Reg(UInt(3.W))
  misPredBlockCounter := Mux(misPredWb,
    "b111".U,
    misPredBlockCounter >> 1.U
  )
  val misPredBlock = misPredBlockCounter(0)
  val deqFlushBlockCounter = Reg(UInt(3.W))
  val deqFlushBlock = deqFlushBlockCounter(0)
  val deqHasCommitted = out.commits.isCommit && out.commits.commitValid(0)
  // TODO *** WARNING ***
  // Blocking commit. Don't change this before we fully understand the logic.
  val deqHitRedirectReg = RegNext(in.redirect.valid && in.redirect.bits.robIdx.isSameEntry(deqPtr)) ||
    RegNext(RegNext(in.redirect.valid && in.redirect.bits.robIdx.isSameEntry(deqPtr)))
  val criticalErrorState = in.csr.criticalErrorState
  when(deqNeedFlush && deqHitRedirectReg){
    deqFlushBlockCounter := "b111".U
  }.otherwise{
    deqFlushBlockCounter := deqFlushBlockCounter >> 1.U
  }

  when(deqHasCommitted){
    deqHasFlushed := false.B
  }.elsewhen(deqNeedFlush && out.flushOut.valid && !out.flushOut.bits.flushItself()){
    deqHasFlushed := true.B
  }

  val traceBlock = out.trace.blockCommit
  val blockCommit = misPredBlock || lastCycleFlush || hasWFI || in.redirect.valid ||
    (deqNeedFlush && !deqHasFlushed) || deqFlushBlock || criticalErrorState || traceBlock

  out.commits.isWalk := walkState
  out.commits.isCommit := normalState && !blockCommit

  val walk_v = VecInit(walkingPtrVec.map(ptr => robEntries(ptr.value).valid))
  val commit_vDeqGroup = VecInit(deqEntriesBankOrder.map(_.commit_v))
  val commit_wDeqGroup = VecInit(deqEntriesBankOrder.map(_.commit_w))
  val commit_block = VecInit((0 until CommitWidth).map(i => !commit_wDeqGroup(i) && !hasCommitted(i)))
  val allowOnlyOneCommit = VecInit(deqEntriesBankOrder.map(x =>
    x.commit_v && (x.slotNeedFlush(0) || x.slotNeedFlush(1) && CompressType.isNotNORMAL(x.entryPairType))
  )).asUInt.orR || intrBitSetReg || diffTrapPending
  // for instructions that may block others, we don't allow them to commit
  out.commits.commitValid := PriorityMux(commitValidThisLine, (0 until CommitWidth).map(i => (commitValidThisLine.asUInt >> i).asUInt.asTypeOf(out.commits.commitValid)))

  for (i <- 0 until CommitWidth) {
    // defaults: state === s_idle and instructions commit
    // when intrBitSetReg, allow only one instruction to commit at each clock cycle
    val isBlocked = intrEnable || (deqNeedFlush && !deqHasFlushed)
    val isBlockedByOlder = if (i != 0) commit_block.asUInt(i, 0).orR || allowOnlyOneCommit && !hasCommitted.asUInt(i - 1, 0).andR else false.B
    commitValidThisLine(i) := commit_vDeqGroup(i) && commit_wDeqGroup(i) && !isBlocked && !isBlockedByOlder && !hasCommitted(i)

    val entry = commitEntries(i)
    out.commits.info(i) := entry
    out.commits.info(i).ftqOffset := 0.U
    out.commits.info(i).ftqIdx := entry.ftqIdx - 1.U
    out.commits.robIdx(i) := deqPtrVec(i)

    out.commits.walkValid(i) := shouldWalkVec(i)
    XSError(
      walkState &&
      out.commits.isWalk && walkState && shouldWalkVec(i) &&
      !walk_v(i),
      s"The walking entry($i) should be valid\n")
  }

  // sync fflags/dirtyFs/vxsat to csr
  for(i <- 0 until fflagsWidth) {
    out.csr.fflags(i) := RegNext(writebackFlagTracker.out.commitSetMask(i), false.B)
  }
  out.csr.dirtyFs := GatedValidRegNext(dirtyFs)
  out.csr.dirtyVs := GatedValidRegNext(dirtyVs)
  out.csr.vxsat    := RegNext(writebackFlagTracker.out.commitSetMask(fflagsWidth), false.B)

  /**
   * LSQ pending slot
   */
  val pendingEntry = robEntries(deqPtr.value)

  val pendingBackPtr = Wire(new RobPtr)
  pendingBackPtr := deqPtr
  pendingBackPtr.isFormer := false.B

  val flushesHeadEntry = in.redirect.valid &&
    in.redirect.bits.robIdx.isSameEntry(deqPtr) &&
    in.redirect.bits.robIdx.isFormer &&
    in.redirect.bits.flushItself()
  val clearsHeadLatter = in.redirect.valid &&
    in.redirect.bits.robIdx.isSameEntry(deqPtr) &&
    CompressType.isNotNORMAL(pendingEntry.entryPairType) && (
      in.redirect.bits.robIdx.isFormer && !in.redirect.bits.flushItself() ||
      !in.redirect.bits.robIdx.isFormer && in.redirect.bits.flushItself()
    )
  val resetsPendingSlot = flushesHeadEntry || clearsHeadLatter
  // Entry validity is cleared by redirectValidReg one cycle after the redirect.
  // Keep the former slot selected until that delayed clear has taken effect.
  val resetsPendingSlotReg = RegNext(resetsPendingSlot, false.B)
  val pendingPtrIsFormer = RegInit(true.B)
  when (
    resetsPendingSlot || resetsPendingSlotReg ||
      out.commits.isCommit && out.commits.commitValid.asUInt.orR
  ) {
    pendingPtrIsFormer := true.B
  }.elsewhen(
    !pendingBackPtr.needFlush(in.redirect) &&
    pendingEntry.valid &&
    CompressType.isNotNORMAL(pendingEntry.entryPairType) &&
    pendingEntry.formerNumWb === 0.U &&
    pendingEntry.latterNumWb =/= 0.U
  ) {
    pendingPtrIsFormer := false.B
  }

  val pendingPtrInput = WireInit(deqPtr)
  pendingPtrInput.isFormer := Mux(resetsPendingSlot, true.B, pendingPtrIsFormer)
  val pendingPtrReg = RegInit(RobPtr(false.B, 0.U))
  pendingPtrReg := pendingPtrInput

  // VirtualStoreQueue samples this boundary in the redirect cycle.  Do not let
  // a stale latter state survive either a whole-entry flush or a redirect that
  // removes only the latter half of the head entry.
  out.lsq.pendingPtr := Mux(resetsPendingSlot, deqPtr.asFormer, pendingPtrReg)

  out.lsq.pendingPtrNext := RegNext(deqPtrVecNext.head)

  /**
   * Pointer updates and capacity
   */
  val deqPtrGenModule = Module(new RobDeqPtrWrapper)
  deqPtrGenModule.in.state := state
  deqPtrGenModule.in.deq_v := commit_vDeqGroup
  deqPtrGenModule.in.deq_w := commit_wDeqGroup
  deqPtrGenModule.in.excp_state := excpState
  deqPtrGenModule.in.intrBitSetReg := intrBitSetReg
  deqPtrGenModule.in.hasNoSpecExec := hasWaitForward
  deqPtrGenModule.in.allowOnlyOneCommit := allowOnlyOneCommit
  deqPtrGenModule.in.interruptSafe := deqEntriesBankOrder(deqPtr.value(commitOffsetWidth - 1, 0)).interruptSafe
  deqPtrGenModule.in.blockCommit := blockCommit
  deqPtrGenModule.in.hasCommitted := hasCommitted
  deqPtrVec := deqPtrGenModule.out.ptrVec
  deqPtrVecNext := deqPtrGenModule.out.nextPtrVec

  val enqPtrGenModule = Module(new RobEnqPtrWrapper)
  enqPtrGenModule.in.redirect := in.redirect
  enqPtrGenModule.in.allowEnqueue := allowEnqueue && rab.in.canEnq && !in.fromVecExcpMod.busy
  enqPtrGenModule.in.hasBlockBackward := hasBlockBackward
  enqPtrGenModule.in.enq := VecInit(in.enq.req.map(req => req.valid && req.bits.firstUop))
  enqPtrVec := enqPtrGenModule.out.ptrVec

  // next walkPtrVec:
  // (1) redirect occurs: update according to state
  // (2) walk: move forwards
  val snapPtrReadBank = snapshots(in.snpt.snptSelect)(0).lineHeadPtr
  val snapPtrVecForWalk = VecInit((0 until CommitWidth).map(i => snapPtrReadBank.addEntries(i.U).asFormer))
  walkPtrVecNext := Mux(in.redirect.valid,
    Mux(in.snpt.useSnpt, snapPtrVecForWalk, deqPtrVecForWalk),
    Mux(walkState && !walkFinished, VecInit(walkPtrVec.map(_.addEntries(CommitWidth.U).asFormer)), walkPtrVec)
  )
  walkPtrTrueNext := Mux(in.redirect.valid,
    Mux(in.snpt.useSnpt, snapshots(in.snpt.snptSelect)(0), deqPtrVecNext(0)),
    Mux(walkState && !walkFinished, walkPtrNext, walkPtrTrue)
  )
  walkPtrVec := walkPtrVecNext
  walkPtrTrue := walkPtrTrueNext
  // T in.redirect.valid, T+1 walkPtrLowBits update, T+2 donotNeedWalk update
  when(in.redirect.valid){
    walkPtrLowBits := Mux(
      in.snpt.useSnpt,
      snapshots(in.snpt.snptSelect)(0).value(commitOffsetWidth-1, 0),
      deqPtrVecNext(0).value(commitOffsetWidth-1, 0)
    )
  }
  when(in.redirect.valid) {
    donotNeedWalk := Fill(donotNeedWalk.length, true.B).asTypeOf(donotNeedWalk)
  }.elsewhen(RegNext(in.redirect.valid)){
    donotNeedWalk := (0 until CommitWidth).map(i => (i.U < walkPtrLowBits))
  }.otherwise{
    donotNeedWalk := 0.U.asTypeOf(donotNeedWalk)
  }
  val numValidEntries = distanceBetween(enqPtr, deqPtr)
  val commitCnt = PopCount(out.commits.commitValid)

  val dispatchNum = Mux(in.enq.canAccept, PopCount(in.enq.req.map(req => req.valid && req.bits.firstUop)), 0.U)
  allowEnqueue := numValidEntries + dispatchNum <= (RobSize - RenameWidth).U
  allowEnqueueForDispatch := numValidEntries + dispatchNum <= (RobSize - 2 * RenameWidth).U

  when(in.redirect.valid) {
    // TODO: lastWalkPtr points the last valid entry
    lastWalkPtr := Mux(
      in.redirect.bits.robIdx.isFormer && in.redirect.bits.flushItself(),
      in.redirect.bits.robIdx.subEntries(1.U).asFormer,
      in.redirect.bits.robIdx
    )
  }


  /**
   * ROB entry state updates
   */

  val redirectValidReg = RegNext(in.redirect.valid)
  val redirectBegin = Reg(UInt(log2Up(RobSize).W))
  val redirectEnd = Reg(UInt(log2Up(RobSize).W))
  val redirectAll = RegInit(false.B)
  when(in.redirect.valid){
    redirectBegin := Mux(in.redirect.bits.robIdx.isFormer && in.redirect.bits.flushItself(),
      Mux(in.redirect.bits.robIdx.value === 0.U, (RobSize - 1).U, in.redirect.bits.robIdx.value - 1.U),
      in.redirect.bits.robIdx.value
    )
    redirectEnd := enqPtr.value
    // Only former+flushItself removes the redirect entry itself.  A latter-slot
    // redirect must retain the former slot even when the physical range wraps.
    redirectAll := in.redirect.bits.robIdx.isFormer && in.redirect.bits.flushItself() &&
      (in.redirect.bits.robIdx.value === enqPtr.value) && (in.redirect.bits.robIdx.flag ^ enqPtr.flag)
  }

  // update robEntries valid
  for (i <- 0 until RobSize) {
    val commitCond = out.commits.isCommit && out.commits.commitValid.zip(deqPtrVec.map(_.value === i.U)).map(x => x._1 && x._2).reduce(_ || _)
    // TODO: fix the needFlush logic for CROB
    // redirectBegin is still valid, but redirectBegin+1 is invalid
    val needFlush = redirectValidReg && (Mux(
      redirectEnd > redirectBegin,
      (i.U > redirectBegin) && (i.U < redirectEnd),
      (i.U > redirectBegin) || (i.U < redirectEnd)
    ) || redirectAll)
    when(commitCond) {
      robEntries(i).valid := false.B
    }.elsewhen(allocatingByEntry(i) && !in.redirect.valid) {
      robEntries(i).valid := true.B
    }.elsewhen(needFlush){
      robEntries(i).valid := false.B
    }
  }

  /**
   * Redirect In
   */
  val redirectFlushesLatterOnly = in.redirect.valid &&
    (in.redirect.bits.robIdx.isFormer =/= in.redirect.bits.flushItself())
  val downgradeToFormerByEntry = (0 until RobSize).map { i =>
    val entry = robEntries(i)
    val downgradeToFormer = redirectFlushesLatterOnly && in.redirect.bits.robIdx.value === i.U &&
      CompressType.isNotNORMAL(entry.entryPairType)
    when(downgradeToFormer) {
      entry.entryPairType := CompressType.NORMAL
      entry.traceBlockInPipe := entry.formerTraceBlockInPipe
    }
    downgradeToFormer
  }

  // status field: writebacked
  // enqueue logic set 6 writebacked to false

  // writeback logic set numWbPorts writebacked to true

  // if the first uop of an instruction is valid , write writebackedCounter
  val enqNeedWriteRFSeq = in.enq.req.map(_.bits.needEnqRab)
  val enqFormerNumWbVec = VecInit(in.enq.req.map(req => req.bits.formerNumWb))
  val enqLatterNumWbVec = VecInit(in.enq.req.map(req => req.bits.latterNumWb))

  for (i <- 0 until RobSize) {
    val enqOH = allocatingOHByEntry(i)
    val uopCanEnqSeq = enqUopHitsByEntry(i)
    val instCanEnqFlag = allocatingByEntry(i)
    val isFirstEnq = !robEntries(i).valid && instCanEnqFlag
    val realDestEnqNum = PopCount(enqNeedWriteRFSeq.zip(uopCanEnqSeq).map { case (writeFlag, valid) => writeFlag && valid })
    val entryPairType = robEntries(i).entryPairType
    val downgradeToFormer = downgradeToFormerByEntry(i)
    when(isFirstEnq){
      robEntries(i).realDestSize := realDestEnqNum
    }.elsewhen(robEntries(i).valid && Cat(uopCanEnqSeq).orR){
      robEntries(i).realDestSize := robEntries(i).realDestSize + realDestEnqNum
    }.elsewhen(downgradeToFormer){
      when(CompressType.isCC(entryPairType) || CompressType.isCS(entryPairType)){
        robEntries(i).realDestSize := robEntries(i).complexSlotHasDest
      }.elsewhen(CompressType.isSC(entryPairType)){
        robEntries(i).realDestSize := robEntries(i).realDestSize - robEntries(i).complexSlotHasDest
      }
    }
    val enqFormerNumWb = Mux1H(enqOH, enqFormerNumWbVec)
    val enqLatterNumWb = Mux1H(enqOH, enqLatterNumWbVec)
    val formerWbCnt = PopCount(in.writeback.map(writeback => writeback.valid && writeback.bits.robIdx.value === i.U && writeback.bits.robIdx.isFormer))
    val latterWbCnt = PopCount(in.writeback.map(writeback => writeback.valid && writeback.bits.robIdx.value === i.U && !writeback.bits.robIdx.isFormer))
    val slotNeedFlush = robEntries(i).slotNeedFlush
    val canWbExcpNeedFlushSeq = excpWBs.zip(in.writebackNeedFlush).map { case (writeback, needFlushWb) =>
      writeback.valid && needFlushWb && writeback.bits.robIdx.value === i.U
    }
    val canWbFormerExcpNeedFlushSeq = canWbExcpNeedFlushSeq.zip(excpWBs).map {
      case (canWbExcpNeedFlush, writeback) => canWbExcpNeedFlush && writeback.bits.robIdx.isFormer
    }
    val canWbLaterExcpNeedFlushSeq = canWbExcpNeedFlushSeq.zip(excpWBs).map {
      case (canWbExcpNeedFlush, writeback) => canWbExcpNeedFlush && !writeback.bits.robIdx.isFormer
    }
    val needFlushWriteBack = Cat(canWbExcpNeedFlushSeq).orR
    val needFlushWriteBackFormer = Cat(canWbFormerExcpNeedFlushSeq).orR
    val needFlushWriteBackLater = Cat(canWbLaterExcpNeedFlushSeq).orR
    when(robEntries(i).valid && needFlushWriteBack){
      slotNeedFlush := slotNeedFlush | Cat(needFlushWriteBackLater, needFlushWriteBackFormer)
    }

    val entry = robEntries(i)
    val redirectClearsLatter = entry.valid && downgradeToFormer
    when(entry.valid) {
      // Keep canceled counts at zero if another writeback arrives before redirect.
      // A flush marked at enqueue can still have writebacks to count.
      val clearFormerNumWb = needFlushWriteBackFormer || slotNeedFlush(0) && !entry.formerNumWb.orR
      // An unpaired entry has no latter slot, even if a squashed uop writes back late.
      val clearLatterNumWb = CompressType.isNORMAL(entryPairType) || redirectClearsLatter ||
        needFlushWriteBackFormer || needFlushWriteBackLater || slotNeedFlush.orR && !entry.latterNumWb.orR
      assert(clearFormerNumWb || formerWbCnt <= entry.formerNumWb,
        s"ROB entry $i former writeback count underflow")
      assert(clearLatterNumWb || latterWbCnt <= entry.latterNumWb,
        s"ROB entry $i latter writeback count underflow")
      entry.formerNumWb := Mux(clearFormerNumWb, 0.U, entry.formerNumWb - formerWbCnt)
      entry.latterNumWb := Mux(clearLatterNumWb, 0.U, entry.latterNumWb - latterWbCnt)
    }.elsewhen(instCanEnqFlag) {
      entry.formerNumWb := enqFormerNumWb
      entry.latterNumWb := enqLatterNumWb
    }

    // Apply writeback to the surviving slot after restoring its trace above.
    val youngestSlotIsFormer = CompressType.isNORMAL(entryPairType) || redirectClearsLatter
    val traceType = Mux(
      redirectClearsLatter,
      robEntries(i).formerTraceBlockInPipe.itype,
      robEntries(i).traceBlockInPipe.itype
    )
    val taken = branchWBs.map(writeback =>
      writeback.valid &&
      writeback.bits.robIdx.value === i.U &&
      (writeback.bits.robIdx.isFormer === youngestSlotIsFormer) &&
      writeback.bits.redirect.get.bits.taken
    ).reduce(_ || _)
    when(robEntries(i).valid && Itype.isNonTaken(traceType) && taken) {
      robEntries(i).traceBlockInPipe.itype := Itype.Taken
    }
  }

  // Forward updates to the entries read for the next commit or walk cycle.
  // Each rdataUpdateValid bit indicates that no uops remain to be written back.
  val rdataUpdateValid = Wire(Vec(deqBankRdataUpdate.length, Bool()))
  for (i <- deqBankRdataUpdate.indices) {
    val enqHeadOH = allocatingOHFor(rdataUpdateRobIdx(i))
    val uopCanEnqSeq = enqUopHitsFor(rdataUpdateRobIdx(i))
    val instCanEnqFlag = enqHeadOH.asUInt.orR
    val realDestEnqNum = PopCount(enqNeedWriteRFSeq.zip(uopCanEnqSeq).map { case (writeFlag, valid) => writeFlag && valid })
    when(!deqBankRdataUpdate(i).valid && instCanEnqFlag) {
      deqBankRdataUpdate(i).realDestSize := realDestEnqNum
    }.elsewhen(deqBankRdataUpdate(i).valid && instCanEnqFlag) {
      deqBankRdataUpdate(i).realDestSize := deqBankRdata(i).realDestSize + realDestEnqNum
    }
    val enqFormerNumWb = Mux1H(enqHeadOH, enqFormerNumWbVec)
    val enqLatterNumWb = Mux1H(enqHeadOH, enqLatterNumWbVec)
    val formerWbCnt = PopCount(in.writeback.map(writeback =>
      writeback.valid && writeback.bits.robIdx.value === rdataUpdateRobIdx(i) && writeback.bits.robIdx.isFormer
    ))
    val latterWbCnt = PopCount(in.writeback.map(writeback =>
      writeback.valid && writeback.bits.robIdx.value === rdataUpdateRobIdx(i) && !writeback.bits.robIdx.isFormer
    ))
    val slotNeedFlush = deqBankRdata(i).slotNeedFlush
    val canWbExcpNeedFlushSeq = excpWBs.zip(in.writebackNeedFlush).map { case (writeback, needFlushWb) =>
      writeback.valid && needFlushWb && (writeback.bits.robIdx.value === rdataUpdateRobIdx(i))
    }
    val canWbFormerExcpNeedFlushSeq = canWbExcpNeedFlushSeq.zip(excpWBs).map {
      case (canWbExcpNeedFlush, writeback) => canWbExcpNeedFlush && writeback.bits.robIdx.isFormer
    }
    val canWbLaterExcpNeedFlushSeq = canWbExcpNeedFlushSeq.zip(excpWBs).map {
      case (canWbExcpNeedFlush, writeback) => canWbExcpNeedFlush && !writeback.bits.robIdx.isFormer
    }
    val needFlushWriteBack = Cat(canWbExcpNeedFlushSeq).orR
    val needFlushWriteBackFormer = Cat(canWbFormerExcpNeedFlushSeq).orR
    val needFlushWriteBackLater = Cat(canWbLaterExcpNeedFlushSeq).orR
    when(deqBankRdataUpdate(i).valid && needFlushWriteBack) {
      deqBankRdataUpdate(i).slotNeedFlush :=
        slotNeedFlush | Cat(needFlushWriteBackLater, needFlushWriteBackFormer)
    }

    val redirectHitThisEntry = deqBankRdataUpdate(i).valid &&
      in.redirect.valid &&
      in.redirect.bits.robIdx.value === rdataUpdateRobIdx(i)
    val entryPairType = deqBankRdata(i).entryPairType
    val redirectClearsLatter = redirectHitThisEntry &&
      CompressType.isNotNORMAL(entryPairType) && (
        in.redirect.bits.robIdx.isFormer && !in.redirect.bits.flushItself() ||
        !in.redirect.bits.robIdx.isFormer && in.redirect.bits.flushItself()
      )
    val readEntry = deqBankRdata(i)
    val updatedEntry = deqBankRdataUpdate(i)
    when(readEntry.valid) {
      val clearFormerNumWb = needFlushWriteBackFormer || slotNeedFlush(0) && !readEntry.formerNumWb.orR
      val clearLatterNumWb = CompressType.isNORMAL(entryPairType) || redirectClearsLatter ||
        needFlushWriteBackFormer || needFlushWriteBackLater || slotNeedFlush.orR && !readEntry.latterNumWb.orR
      assert(clearFormerNumWb || formerWbCnt <= readEntry.formerNumWb,
        s"ROB read slot $i former writeback count underflow")
      assert(clearLatterNumWb || latterWbCnt <= readEntry.latterNumWb,
        s"ROB read slot $i latter writeback count underflow")
      updatedEntry.formerNumWb := Mux(clearFormerNumWb, 0.U, readEntry.formerNumWb - formerWbCnt)
      updatedEntry.latterNumWb := Mux(clearLatterNumWb, 0.U, readEntry.latterNumWb - latterWbCnt)
    }.elsewhen(instCanEnqFlag) {
      updatedEntry.formerNumWb := enqFormerNumWb
      updatedEntry.latterNumWb := enqLatterNumWb
    }
    rdataUpdateValid(i) := deqBankRdataUpdate(i).isWritebacked

    // trace
    val youngestSlotIsFormer = CompressType.isNORMAL(deqBankRdata(i).entryPairType) || redirectClearsLatter
    val taken = branchWBs.map(writeback =>
      writeback.valid &&
      writeback.bits.robIdx.value === rdataUpdateRobIdx(i) &&
      (writeback.bits.robIdx.isFormer === youngestSlotIsFormer) &&
      writeback.bits.redirect.get.bits.taken
    ).reduce(_ || _)
    when(deqBankRdata(i).valid && Itype.isNonTaken(deqBankRdata(i).traceBlockInPipe.itype) && taken){
      // BranchType code(notaken itype = 4) must be correctly replaced!
      deqBankRdataUpdate(i).traceBlockInPipe.itype := Itype.Taken
    }
  }
  deqBanking.updateDeqEntries(deqEntriesBankOrder, deqBankRdataUpdate, rdataUpdateValid, allCommitted)

  /**
   * Exception generator interface
   */
  val commitReadAddrNext = Mux(stateNext === s_idle,
    VecInit(deqPtrVecNext.map(_.value)),
    VecInit(walkPtrVecNext.map(_.value))
  )

  excpGen.in.redirect <> in.redirect
  excpGen.in.flush := out.flushOut.valid

  for (i <- 0 until RenameWidth) {
    excpGen.in.enq(i).valid := enqFireVec(i)
    excpGen.in.enq(i).bits.robIdx := in.enq.req(i).bits.robIdx
    excpGen.in.enq(i).bits.ftqPtr := in.enq.req(i).bits.ftqPtr
    excpGen.in.enq(i).bits.ftqOffset := in.enq.req(i).bits.ftqOffset
    excpGen.in.enq(i).bits.isRVC := in.enq.req(i).bits.isRVC
    excpGen.in.enq(i).bits.excpVec := in.enq.req(i).bits.exceptionVec
    excpGen.in.enq(i).bits.satpFlushFirstFetchFault := in.enq.req(i).bits.satpFlushFirstFetchFault
    excpGen.in.enq(i).bits.hasExcp := in.enq.req(i).bits.hasException
    excpGen.in.enq(i).bits.isEnqExcp := in.enq.req(i).bits.hasException
    excpGen.in.enq(i).bits.isFetchMalAddr := in.enq.req(i).bits.isFetchMalAddr
    excpGen.in.enq(i).bits.flushPipe := in.enq.req(i).bits.flushPipe
    excpGen.in.enq(i).bits.satpFlush := false.B
    excpGen.in.enq(i).bits.isVset := in.enq.req(i).bits.isVset
    excpGen.in.enq(i).bits.replayInst := false.B
    XSError(enqFirstUopFireVec(i) && in.enq.req(i).bits.replayInst, "enq should not set replayInst")
    excpGen.in.enq(i).bits.crossPageIPFFix := in.enq.req(i).bits.crossPageIPFFix
    excpGen.in.enq(i).bits.trigger := in.enq.req(i).bits.trigger
    excpGen.in.enq(i).bits.vstartEn := false.B //DontCare
    excpGen.in.enq(i).bits.vstart := 0.U //DontCare
    excpGen.in.enq(i).bits.vuopIdx := 0.U
    excpGen.in.enq(i).bits.isVecLoad := false.B
    excpGen.in.enq(i).bits.isVlm := false.B
    excpGen.in.enq(i).bits.isStrided := false.B
    excpGen.in.enq(i).bits.isIndexed := false.B
    excpGen.in.enq(i).bits.isWhole := false.B
    excpGen.in.enq(i).bits.nf := 0.U
    excpGen.in.enq(i).bits.vsew := 0.U
    excpGen.in.enq(i).bits.veew := 0.U
    excpGen.in.enq(i).bits.vlmul := 0.U
  }

  require(excpWBs.length == excpGen.in.wb.length,
    f"excpWBs.length: ${excpWBs.length}, " +
      f"excpGen.in.wb.length: ${excpGen.in.wb.length}")
  for (((wb, exc_wb), i) <- excpWBs.zip(excpGen.in.wb).zipWithIndex) {
    exc_wb.valid       := wb.valid
    exc_wb.bits.robIdx := wb.bits.robIdx
    // only enq inst use ftqPtr to read gpa
    exc_wb.bits.ftqPtr          := 0.U.asTypeOf(exc_wb.bits.ftqPtr)
    exc_wb.bits.ftqOffset       := 0.U.asTypeOf(exc_wb.bits.ftqOffset)
    exc_wb.bits.isRVC           := false.B
    exc_wb.bits.excpVec         := wb.bits.exceptionVec
    exc_wb.bits.satpFlushFirstFetchFault := false.B
    exc_wb.bits.hasExcp         := wb.bits.exceptionVec.orR // Todo: use in.writebackNeedFlush(i) instead
    exc_wb.bits.isEnqExcp       := false.B
    exc_wb.bits.isFetchMalAddr  := false.B
    exc_wb.bits.flushPipe       := wb.bits.flushPipe.getOrElse(false.B)
    exc_wb.bits.satpFlush       := wb.bits.satpFlush.getOrElse(false.B)
    exc_wb.bits.isVset          := false.B
    exc_wb.bits.replayInst      := wb.bits.replay.getOrElse(false.B)
    exc_wb.bits.crossPageIPFFix := false.B
    val trigger = wb.bits.trigger.getOrElse(TriggerAction.None).asTypeOf(exc_wb.bits.trigger)
    // EBREAK is the only breakpoint exception produced by the CSR execution unit.
    exc_wb.bits.trigger := (if (wb.bits.params.hasCSR) {
      Mux(wb.bits.exceptionVec(ExceptionNO.breakPoint), TriggerAction.Ebreak, trigger)
    } else trigger)
    exc_wb.bits.vstartEn := false.B // Todo[Vector]: support vector ls exception
    exc_wb.bits.vstart := 0.U // Todo[Vector]: support vector ls exception
    exc_wb.bits.vuopIdx :=  0.U // Todo[Vector]: support vector ls exception
    exc_wb.bits.isVecLoad := false.B // Todo[Vector]: support vector ls exception
    exc_wb.bits.isVlm := false.B // Todo[Vector]: support vector ls exception
    exc_wb.bits.isStrided := false.B // Todo[Vector]: remove it
    exc_wb.bits.isIndexed := false.B // Todo[Vector]: remove it
    exc_wb.bits.isWhole := false.B // Todo[Vector]: remove it
    exc_wb.bits.nf := 0.U // Todo[Vector]: support vector ls exception
    exc_wb.bits.vsew := 0.U // Todo[Vector]: support vector ls exception
    exc_wb.bits.veew := 0.U // Todo[Vector]: support vector ls exception
    exc_wb.bits.vlmul := 0.U // Todo[Vector]: support vector ls exception
  }

  out.robFull := !allowEnqueue

  out.toVecExcpMod.logicPhyRegMap := rab.out.toVecExcpMod.logicPhyRegMap
  out.toVecExcpMod.excpInfo := vecExcpInfo

  /**
   * Assertion
   */
  XSError(enqPtr.isBeforeSlot(deqPtr) && !isFull(enqPtr, deqPtr), "\ndeqPtr is older than enqPtr!\n")

  /**
   * Commit stuck detection.
   *
   * Commit stuck for a long time will trigger critical error
   */
  // max commit-stuck cycle
  val mmioBusy = out.lsq.mmioBusy // lsq know uncache request is rob head
  val commitStuck = (!out.commits.commitValid.reduce(_ || _) || !out.commits.isCommit) && !mmioBusy && (wfiResume.B || !hasWFI)
  val commitStuckCounter = Module(new CommitStuckCounter(
    width = log2Up(maxCommitStuck),
    forceEnable = (env.EnableDifftest || env.FullBasicDiff).B
  ))
  val commitStuckOverflowEnabled = if (wfiResume) true.B else !hasWFI
  commitStuckCounter.in.stuck := commitStuck
  commitStuckCounter.in.runtimeEnable := in.commitStuckCheckEnable
  commitStuckCounter.in.overflowEnabled := commitStuckOverflowEnabled
  // check if stuck > 2^maxCommitStuckCycle
  val commitStuckOverflow = commitStuckCounter.out.overflow
  val criticalErrors = Seq(
    ("rob_commit_stuck  ", commitStuckOverflow),
  )
  require(criticalErrors.length == out.criticalErrors.length)
  generateCriticalErrors()

  /**
   * Debug and difftest metadata
   */
  private val numExuWbPorts = in.exuWriteback.length
  println(s"Rob: size $RobSize, numExuWbPorts: $numExuWbPorts, commitwidth: $CommitWidth")

  println(s"ExceptionGen:")
  println(s"num of exceptions: ${params.numException}")

  // Per-slot copy of the full enqueue uop for debug and difftest.
  val debug_microOp = Reg(Vec(RobSize, Vec(2, new EnqRobUop)))
  val debug_lqIdx = Reg(Vec(RobSize, Vec(2, new LqPtr)))
  val debug_sqIdx = Reg(Vec(RobSize, Vec(2, new SqPtr)))
  val debug_exuData = Reg(Vec(RobSize, Vec(2, UInt(XLEN.W))))
  val debug_lsInfo = RegInit(VecInit(Seq.fill(RobSize)(DebugLsInfo.init)))
  val debug_lsTopdownInfo = RegInit(VecInit(Seq.fill(RobSize)(LsTopdownInfo.init)))
  val debug_lqIdxValid = RegInit(VecInit.fill(RobSize)(VecInit(Seq.fill(2)(false.B))))
  val debug_lsIssued = RegInit(VecInit.fill(RobSize)(false.B))
  val enqHalfWritten = RegInit(VecInit(Seq.fill(RobSize)(VecInit(Seq.fill(2)(false.B)))))

  private def slotIndex(ptr: RobPtr): UInt = (!ptr.isFormer).asUInt
  private def debugMeta(uop: EnqRobUop): RenameOutUopDebug =
    uop.debug.getOrElse(0.U.asTypeOf(new RenameOutUopDebug))

  val debug_lsIssue = WireDefault(debug_lsIssued)
  debug_lsIssue(deqPtr.value) := in.debugHeadLsIssue

  // Enqueue slot metadata.
  val timer = GTimer()
  for (i <- 0 until RenameWidth) {
    // we don't check whether in.redirect is valid here since redirect has higher priority
    val hasEarlierSameHalf =
      if (i == 0) {
        false.B
      } else {
        VecInit((0 until i).map { j =>
          enqFireVec(j) &&
          in.enq.req(j).bits.robIdx.isSameSlot(in.enq.req(i).bits.robIdx)
        }).asUInt.orR
      }
    val hasEarlierEntryReset =
      if (i == 0) {
        false.B
      } else {
        VecInit((0 until i).map { j =>
          enqFirstUopFireVec(j) &&
          in.enq.req(j).bits.robIdx.isSameEntry(in.enq.req(i).bits.robIdx)
        }).asUInt.orR
      }
    when(enqFireVec(i)) {
      val enqUop = in.enq.req(i).bits
      val enqIndex = in.enq.req(i).bits.robIdx.value
      val subIndex = !in.enq.req(i).bits.robIdx.isFormer
      val slotWritten = Mux(enqUop.firstUop || hasEarlierEntryReset, false.B, enqHalfWritten(enqIndex)(subIndex))
      when(enqUop.firstUop && !hasEarlierEntryReset) {
        enqHalfWritten(enqIndex)(0) := false.B
        enqHalfWritten(enqIndex)(1) := false.B
      }
      when(!hasEarlierSameHalf && !slotWritten) {
        // Keep the first instruction metadata in each rob-half.  The functional
        // fields feed exception handling; the full uop is only for debug/difftest.
        // TODO: move slotUopBuf to robEntries, out of debug part.
        slotUopBuf(enqIndex)(subIndex).commitType := enqUop.commitType
        slotUopBuf(enqIndex)(subIndex).fuType := enqUop.fuType
        slotUopBuf(enqIndex)(subIndex).fuOpType := enqUop.fuOpType
        debug_microOp(enqIndex)(subIndex) := enqUop
        debug_microOp(enqIndex)(subIndex).debug.foreach { debug =>
          debug.perfDebugInfo.dispatchTime := timer
          debug.perfDebugInfo.enqRsTime := timer
          debug.perfDebugInfo.selectTime := timer
          debug.perfDebugInfo.issueTime := timer
          debug.perfDebugInfo.writebackTime := timer
          debug.perfDebugInfo.tlbFirstReqTime := timer
          debug.perfDebugInfo.tlbRespTime := timer
        }
        debug_lqIdxValid(enqIndex)(subIndex) := false.B
        enqHalfWritten(enqIndex)(subIndex) := true.B
      }
      when(enqUop.firstUop && !hasEarlierEntryReset) {
        debug_lsInfo(enqIndex) := DebugLsInfo.init
        debug_lsTopdownInfo(enqIndex) := LsTopdownInfo.init
        debug_lsIssued(enqIndex) := false.B
      }
    }
  }

  // LSQ allocation precedes writeback so completion metadata retains priority.
  in.debugEnqLsq.needAlloc.map(_(0)).zip(in.debugEnqLsq.req).foreach { case (alloc, req) =>
    when(in.debugEnqLsq.canAccept && alloc && req.valid) {
      val ptr = req.bits.uop.robIdx
      debug_lqIdx(ptr.value)(slotIndex(ptr)) := req.bits.uop.lqIdx
      debug_lqIdxValid(ptr.value)(slotIndex(ptr)) := true.B
    }
  }

  // Head load/store issue.
  when(in.debugHeadLsIssue) {
    debug_lsIssued(deqPtr.value) := true.B
  }

  // Writeback metadata.
  for (wb <- exuWBs) {
    val wbIdx = wb.bits.robIdx.value
    val subIdx = slotIndex(wb.bits.robIdx)
    when(wb.valid) {
      debug_exuData(wbIdx)(subIdx) := wb.bits.data
      wb.bits.perfDebugInfo.foreach { x =>
        debug_microOp(wbIdx)(subIdx).debug.foreach { slotDebug =>
          slotDebug.perfDebugInfo.enqRsTime := x.enqRsTime
          slotDebug.perfDebugInfo.selectTime := x.selectTime
          slotDebug.perfDebugInfo.issueTime := x.issueTime
          slotDebug.perfDebugInfo.writebackTime := x.writebackTime
          slotDebug.perfDebugInfo.tlbFirstReqTime := x.tlbFirstReqTime
          slotDebug.perfDebugInfo.tlbRespTime := x.tlbRespTime
        }
      }
      debug_lqIdx(wbIdx)(subIdx) := wb.bits.lqIdx.getOrElse(0.U.asTypeOf(new LqPtr))
      debug_sqIdx(wbIdx)(subIdx) := wb.bits.sqIdx.getOrElse(0.U.asTypeOf(new SqPtr))
    }
    val debug_Uop = debug_microOp(wbIdx)(subIdx)
    val debug = debugMeta(debug_Uop)
    XSInfo(wb.valid,
      p"writebacked pc 0x${Hexadecimal(debug.pc)} wen ${debug_Uop.rfWen} " +
        p"data 0x${Hexadecimal(wb.bits.data)} ldst ${debug_Uop.ldest} pdst ${debug_Uop.pdest} " +
        p"skip ${wb.bits.debug.isSkipDiff} robIdx: ${wb.bits.robIdx}\n"
    )
  }

  for (i <- 0 until LduCnt) {
    debug_lsTopdownInfo(in.lsTopdownInfo(i).s1.robIdx).s1SignalEnable(in.lsTopdownInfo(i))
    debug_lsTopdownInfo(in.lsTopdownInfo(i).s2.robIdx).s2SignalEnable(in.lsTopdownInfo(i))
  }

  val excpDebug = debugMeta(Mux(excpIsFormer,
    debug_microOp(deqPtr.value)(0), debug_microOp(deqPtr.value)(1)))
  // TODO: pc is unused.
  out.exception.bits.pc := RegEnable(excpDebug.pc, excpHappen)
  // instr is only used for difftest? need check
  out.exception.bits.instr := RegEnable(excpDebug.instr, excpHappen)

  /**
   * Trace extension
   */
  private val traceInstrCountWidth = log2Ceil(RenameWidth + 1)
  private val traceEncodeTable = (1 to RenameWidth).flatMap { instrNum =>
    (0 to instrNum).map(nonRVCNum => (instrNum, nonRVCNum))
  }
  private val traceDecodeTable = traceEncodeTable.map { case (instrNum, nonRVCNum) =>
    (instrNum, instrNum + nonRVCNum)
  }
  private def decodeTraceInstrCount(traceIretire: UInt): UInt = {
    chisel3.util.experimental.decode.decoder(
      traceIretire,
      TruthTable(
        traceDecodeTable.zipWithIndex.map { case ((instrNum, _), encode) =>
          BitPat((encode + 1).U(IretireWidthEncoded.W)) -> BitPat(instrNum.U(traceInstrCountWidth.W))
        },
        BitPat.N(traceInstrCountWidth)
      )
    )
  }
  private def decodeTraceHalfWords(traceIretire: UInt): UInt = {
    chisel3.util.experimental.decode.decoder(
      traceIretire,
      TruthTable(
        traceDecodeTable.zipWithIndex.map { case ((_, halfWords), encode) =>
          BitPat((encode + 1).U(IretireWidthEncoded.W)) -> BitPat(halfWords.U(IretireWidthCommited.W))
        },
        BitPat.N(IretireWidthCommited)
      )
    )
  }
  private def traceHalfWordsToBytes(halfWords: UInt): UInt = {
    val formerLenWidth = log2Ceil(RenameWidth * 4 + 1)
    Cat(halfWords, 0.U(1.W))(formerLenWidth - 1, 0)
  }

  // Shared trace results used by flush handling, commit metadata, and stall diagnostics.
  // The Trace extension section below drives these combinational wires without extra registers.
  val instrSizeCommit = Wire(Vec(CommitWidth, UInt(log2Ceil(RenameWidth + 1).W)))
  val formerInstrCntCommit = Wire(Vec(CommitWidth, UInt(log2Ceil(RenameWidth + 1).W)))
  val latterInstrCntCommit = Wire(Vec(CommitWidth, UInt(log2Ceil(RenameWidth + 1).W)))
  val commitInstrCntByEntry = instrSizeCommit.toSeq

  /*
    decode iretireEncoded => iretires, instrSize
    (instrNum((log2Ceil(RenameWidth + 1)).W), iretire(IretireWidthInPipe.W), encode(IretireWidthEncode.W))
    val instrSizeTable = Seq(
      (1, 1, 1), (1, 2, 2),
      (2, 2, 3), (2, 3, 4), (2, 4, 5),
      (3, 3, 6), (3, 4, 7), (3, 5, 8), (3, 6, 9),
      (4, 4, 10), (4, 5, 11), (4, 6, 12), (4, 7, 13), (4, 8, 14),
      (5, 5, 15), (5, 6, 16), (5, 7, 17), (5, 8, 18), (5, 9, 19), (5, 10, 20),
      (6, 6, 21), (6, 7, 22), (6, 8, 23), (6, 9, 24), (6, 10, 25), (6, 11, 26), (6, 12, 27),
    )
  */
  val iretireCommit = Wire(Vec(CommitWidth, UInt(IretireWidthCommited.W)))
  val formerHalfWordsCommit = Wire(Vec(CommitWidth, UInt(IretireWidthCommited.W)))

  for (i <- 0 until CommitWidth) {
    iretireCommit(i) := decodeTraceHalfWords(commitEntries(i).traceBlockInPipe.iretire)
    instrSizeCommit(i) := decodeTraceInstrCount(commitEntries(i).traceBlockInPipe.iretire)
    formerInstrCntCommit(i) := decodeTraceInstrCount(commitEntries(i).formerTraceIretire)
    formerHalfWordsCommit(i) := decodeTraceHalfWords(commitEntries(i).formerTraceIretire)
    latterInstrCntCommit(i) := Mux(
      CompressType.isNotNORMAL(commitEntries(i).entryPairType),
      instrSizeCommit(i) - formerInstrCntCommit(i),
      0.U
    )
    formerLenCommit(i) := traceHalfWordsToBytes(formerHalfWordsCommit(i))
    when(commitEntries(i).commit_v && CompressType.isNotNORMAL(commitEntries(i).entryPairType)) {
      assert(instrSizeCommit(i) >= formerInstrCntCommit(i))
    }
    when(commitEntries(i).commit_v && CompressType.isNORMAL(commitEntries(i).entryPairType)) {
      assert(latterInstrCntCommit(i) === 0.U)
    }
  }

  // Retired instruction totals are shared by CSR, performance counters, and difftest.
  val isCommit = out.commits.isCommit
  val isCommitReg = GatedValidRegNext(out.commits.isCommit)
  val fusionCommitCountNow = out.commits.commitValid.zip(out.commits.info).zip(deqPtrVec).map {
    case ((valid, info), ptr) =>
      val formerFusionCount = debugMeta(debug_microOp(ptr.value)(0)).fusionNum
      val latterFusionCount = debugMeta(debug_microOp(ptr.value)(1)).fusionNum
      val survivingFusionCount = formerFusionCount +& Mux(
        CompressType.isNotNORMAL(info.entryPairType),
        latterFusionCount,
        0.U
      )
      Mux(valid, survivingFusionCount, 0.U)
  }.reduce(_ +& _)
  val fuseCommitCnt = RegEnable(fusionCommitCountNow, isCommit)
  val trueCommitCnt = RegEnable(
    out.commits.commitValid.zip(commitInstrCntByEntry).map { case (v, instrCnt) =>
      Mux(v, instrCnt, 0.U)
    }.reduce(_ +& _),
    isCommit
  )
  val retireCounter = Mux(isCommitReg, trueCommitCnt, 0.U)
  out.csr.perfinfo.retiredInstr := retireCounter

  val traceValids = out.trace.traceCommitInfo.blocks.map(_.valid)
  val traceBlocks = out.trace.traceCommitInfo.blocks
  val traceBlockInPipe = out.trace.traceCommitInfo.blocks.map(_.bits.tracePipe)

  for (i <- 0 until CommitWidth) {
    traceBlocks(i).bits.ftqIdx.foreach(_ := commitEntries(i).ftqIdx)
    traceBlocks(i).bits.ftqOffset.foreach(_ := commitEntries(i).ftqOffset)
    traceBlockInPipe(i).itype := commitEntries(i).traceBlockInPipe.itype
    traceBlockInPipe(i).iretire := iretireCommit(i)
    traceBlockInPipe(i).ilastsize := commitEntries(i).traceBlockInPipe.ilastsize
    traceValids(i) := out.commits.isCommit && out.commits.commitValid(i)
    // exception only occur in block(0).
    if(i == 0) {
      when(out.exception.valid){ // trace exception
        traceBlocks(i).bits.tracePipe.itype := Mux(out.exception.bits.isInterrupt,
          Itype.Interrupt,
          Itype.Exception
        )
        traceValids(i) := true.B
        traceBlockInPipe(i).iretire := 0.U
      }
    }
  }

  // Shared retirement views must precede HPM, simulation, database, and difftest users.
  // Normal RTL view used by the always-on performance events.
  val commitSlotInfo = deqPtrVec.map(_.value).map(slotUopBuf(_)).flatten
  val commitSlotValid = out.commits.commitValid.zip(out.commits.info).flatMap { case (valid, info) =>
    Seq(valid, valid && !CompressType.isNORMAL(info.entryPairType))
  }
  val commitLoadValid = commitSlotValid.zip(commitSlotInfo).map { case (v, uop) =>
    v && uop.commitType === CommitType.LOAD
  }
  val commitBranchValid = commitSlotValid.zip(commitSlotInfo).map { case (v, uop) =>
    v && FuType.isBrh(uop.fuType)
  }
  val commitStoreValid = commitSlotValid.zip(commitSlotInfo).map { case (v, uop) =>
    v && uop.commitType === CommitType.STORE
  }

  def ifCommit(counter: UInt): UInt = Mux(isCommit, counter, 0.U)
  def ifCommitReg(counter: UInt): UInt = Mux(isCommitReg, counter, 0.U)

  /**
   * Performance counters for HPM (after silicon)
   */
  val isBrhOrJmpWBs = in.exuWriteback.filter(x => (x.bits.params.hasBrhFu || x.bits.params.hasJmpFu)).toSeq
  val misPred = in.redirect.valid && in.redirect.bits.isMisPred
  val brhJump = PopCount(isBrhOrJmpWBs.map(wb => wb.valid))

  val commitLoadVec = VecInit(commitLoadValid)
  val commitBranchVec = VecInit(commitBranchValid)
  val commitStoreVec = VecInit(commitStoreValid)
  val perfEvents = Seq(
    ("rob_interrupt_num      ", out.flushOut.valid && intrEnable),
    ("rob_exception_num      ", out.flushOut.valid && deqHasExcp),
    ("rob_flush_pipe_num     ", out.flushOut.valid && isFlushPipe),
    ("rob_replay_inst_num    ", out.flushOut.valid && isFlushPipe && deqHasReplayInst),
    ("rob_commitUop          ", ifCommit(commitCnt)),
    ("rob_commitInstr        ", ifCommitReg(trueCommitCnt)),
    ("rob_commitInstrFused   ", ifCommitReg(fuseCommitCnt)),
    ("rob_commitInstrLoad    ", ifCommitReg(PopCount(RegEnable(commitLoadVec, isCommit)))),
    ("rob_commitInstrBranch  ", ifCommitReg(PopCount(RegEnable(commitBranchVec, isCommit)))),
    ("rob_commitInstrStore   ", ifCommitReg(PopCount(RegEnable(commitStoreVec, isCommit)))),
    ("rob_walkInstr          ", Mux(out.commits.isWalk, PopCount(out.commits.walkValid), 0.U)),
    ("rob_walkCycle          ", (walkState)),
    ("rob_1_4_valid          ", numValidEntries <= (RobSize / 4).U),
    ("rob_2_4_valid          ", numValidEntries > (RobSize / 4).U && numValidEntries <= (RobSize / 2).U),
    ("rob_3_4_valid          ", numValidEntries > (RobSize / 2).U && numValidEntries <= (RobSize * 3 / 4).U),
    ("rob_4_4_valid          ", numValidEntries > (RobSize * 3 / 4).U),
    ("BRANCH_JUMP            ", brhJump),
    ("BR_MIS_PRED            ", misPred),
    ("TOTAL_FLUSH            ", in.redirect.valid)
  )
  require(perfEvents.length == out.perfEvents.length)
  generatePerfEvent()

  /**************************** Post-Silicon / Pre-Silicon Boundary ****************************/

  // The logic below supports pre-silicon simulation, including performance
  // counters and Difftest verification. It must not affect hardware behavior in silicon.

  /**
   * Performance counters for simulation (pre-silicon)
   */

  // Keep simulation payloads separate from the entries used for retirement and recovery.
  val robEntryDebug = OptionWrapper(backendParams.basicDebugEn, Reg(Vec(RobSize, new RobEntryDebugBundle)))
  robEntryDebug.foreach { entries =>
    for (i <- 0 until RobSize) {
      when(allocatingByEntry(i) && !in.redirect.valid) {
        entries(i).connectEnq(enqBankSelectUops(i))
      }
      entries(i).commit.noCompressReason.foreach { reason =>
        when(downgradeToFormerByEntry(i)) {
          reason := NoCompressReason.flushedHalf
        }
      }
    }

    // Sample the same row on the same edge as the functional dequeue cache.
    val deqDebugRead = deqBanking.readRows(entries.map(_.commit), deqBankRaddr, backendParams.debugEn)
    for (bank <- 0 until CommitWidth) {
      val debug = Mux(allCommitted, deqDebugRead(bank + CommitWidth), deqDebugRead(bank))
      deqEntriesBankOrder(bank).connectCommitDebug(debug)
    }
    if (backendParams.debugEn) {
      dontTouch(entries)
      dontTouch(deqDebugRead)
    }
  }

  // Debug/difftest-only view of the full uop.
  val commitDebugUop = deqPtrVec.map(_.value).map(debug_microOp(_)).flatten
  // Simulation diagnostics.
  out.debugWaitForward.foreach(_ := hasWaitForward)
  out.debugBlockBackward.foreach(_ := hasBlockBackward)

  // debug trigger for simulation
  if (backendParams.debugEn) {
    val debug_sim_trig = dontTouch(Wire(Bool()))
    debug_sim_trig := out.commits.isCommit && (commitSlotValid zip commitDebugUop map {
      case (valid, uop) => valid && debugMeta(uop).debug_sim_trig
    }).reduce(_ || _)
  }

  XSDebug(deqHasExcp && excpState.bits.trigger === TriggerAction.SingleStep, "Debug Mode: Deq has singlestep exception\n")
  XSDebug(deqHasExcp && TriggerAction.isDmode(excpState.bits.trigger), "Debug Mode: Deq has trigger entry debug Mode\n")

  XSDebug(out.flushOut.valid,
    p"generate redirect: pc 0x${Hexadecimal(out.exception.bits.pc)} intr $intrEnable " +
      p"excp $deqHasExcp flushPipe $isFlushPipe " +
      p"Trap_target 0x${Hexadecimal(in.csr.trapTarget.pc)} exceptionVec ${Binary(excpState.bits.excpVec.asUInt)}\n")

  // Retirement diagnostics.
  for (i <- 0 until CommitWidth) {
    XSInfo(out.commits.isCommit && out.commits.commitValid(i),
      "retired pc %x wen %d ldest %d pdest %x data %x\n",
      debugMeta(debug_microOp(deqPtrVec(i).value).head).pc,
      out.commits.info(i).rfWen.getOrElse(false.B),
      out.commits.info(i).basicDebug.map(_.ldest).getOrElse(0.U),
      out.commits.info(i).basicDebug.map(_.pdest).getOrElse(0.U),
      debug_exuData(deqPtrVec(i).value)(0)
    )
    XSInfo(walkState && out.commits.walkValid(i), "walked pc %x wen %d ldst %d data %x\n",
      debugMeta(debug_microOp(walkPtrVec(i).value).head).pc,
      out.commits.info(i).rfWen.getOrElse(false.B),
      out.commits.info(i).basicDebug.map(_.ldest).getOrElse(0.U),
      debug_exuData(walkPtrVec(i).value)(0)
    )
  }

  val noCommitCycleCnt = RegInit(0.U(20.W))
  when(out.commits.isCommit && out.commits.commitValid(0)) {
    noCommitCycleCnt := 0.U
  }.otherwise {
    noCommitCycleCnt := noCommitCycleCnt + 1.U
  }
  // The dequeue row caches readiness, so read the counts from the ROB entry.
  val deqPtrRobEntry: RobEntryBundle = robEntries(deqPtr.value)
  when(noCommitCycleCnt === 14999.U) {
    printf(p"[CROB-STUCK] deq=${Hexadecimal(deqPtr.value)} valid=${commitHeadEntry.commit_v} " +
      p"realDest=${commitHeadEntry.realDestSize} " +
      p"ctype=${Binary(commitHeadEntry.entryPairType)} slotNeedFlush=${Binary(commitHeadEntry.slotNeedFlush)} " +
      p"formerCnt=${formerInstrCntCommit(0)} latterCnt=${latterInstrCntCommit(0)} " +
      p"formerNumWb=${deqPtrRobEntry.formerNumWb} latterNumWb=${deqPtrRobEntry.latterNumWb} " +
      p"headPC=${Hexadecimal(debugMeta(debug_microOp(deqPtr.value).head).pc)} " +
      p"tailPC=${Hexadecimal(debugMeta(debug_microOp(deqPtr.value)(1)).pc)}\n")
  }

  /**
   * Simple performance counters like XSPerf
   */
  XSPerfAccumulate("flush_num", out.flushOut.valid)
  XSPerfAccumulate("interrupt_num", out.flushOut.valid && intrEnable)
  XSPerfAccumulate("exception_num", out.flushOut.valid && deqHasExcp)
  XSPerfAccumulate("flush_pipe_num", out.flushOut.valid && isFlushPipe)
  XSPerfAccumulate("replay_inst_num", out.flushOut.valid && isFlushPipe && deqHasReplayInst)

  XSPerfAccumulate("s_idle_to_idle", normalState && stateNext === s_idle)
  XSPerfAccumulate("s_idle_to_walk", normalState && stateNext === s_walk)
  XSPerfAccumulate("s_walk_to_idle", walkState && stateNext === s_idle)
  XSPerfAccumulate("s_walk_to_walk", walkState && stateNext === s_walk)

  XSPerfAccumulate("branch_jump", brhJump)
  XSPerfAccumulate("br_mis_pred", misPred)
  XSPerfAccumulate("total_flush", in.redirect.valid)

  XSPerfAccumulate("clock_cycle", 1.U, XSPerfLevel.CRITICAL)
  QueuePerf(RobSize, numValidEntries, numValidEntries === RobSize.U)
  XSPerfAccumulate("commitUop", ifCommit(commitCnt))
  XSPerfAccumulate("commitInstr", ifCommitReg(trueCommitCnt), XSPerfLevel.CRITICAL)
  XSPerfRolling("ipc", ifCommitReg(trueCommitCnt), 1000, clock, reset)
  XSPerfRolling("cpi", perfCnt = 1.U /*Cycle*/ , eventTrigger = ifCommitReg(trueCommitCnt), granularity = 1000, clock, reset)
  XSPerfAccumulate("commitInstrFused", ifCommitReg(fuseCommitCnt))

  val commitIsLoad = deqPtrVec.map(ptr => debug_microOp(ptr.value)(0).commitType === CommitType.LOAD)
  XSPerfAccumulate("commitInstrLoad", ifCommit(PopCount(commitLoadValid)))
  val commitIsBranch = deqPtrVec.map(ptr => FuType.isBrh(debug_microOp(ptr.value)(0).fuType))
  XSPerfAccumulate("commitInstrBranch", ifCommit(PopCount(commitBranchValid)))
  val commitIsStore = deqPtrVec.map(ptr => debug_microOp(ptr.value)(0).commitType === CommitType.STORE)
  XSPerfAccumulate("commitInstrStore", ifCommit(PopCount(commitStoreValid)))
  XSPerfAccumulate("writeback", PopCount((0 until RobSize).map(i => robEntries(i).valid && robEntries(i).isWritebacked)))
  XSPerfAccumulate("walkInstr", Mux(out.commits.isWalk, PopCount(out.commits.walkValid), 0.U))
  XSPerfAccumulate("walkCycleTotal", walkState)
  XSPerfAccumulate("waitRabWalkEnd", walkState && walkFinished && !rab.in.status.walkEnd)
  private val walkCycle = RegInit(0.U(8.W))
  private val waitRabWalkCycle = RegInit(0.U(8.W))
  walkCycle := Mux(in.redirect.valid, 0.U, Mux(walkState, walkCycle + 1.U, 0.U))
  waitRabWalkCycle := Mux(walkState && walkFinished, 0.U, Mux(walkState, walkCycle + 1.U, 0.U))

  XSPerfHistogram("walkRobCycleHist", walkCycle, walkState && walkFinished, 0, 32)
  XSPerfHistogram("walkRabExtraCycleHist", waitRabWalkCycle, walkState && walkFinished && rab.in.status.walkEnd, 0, 32)
  XSPerfHistogram("walkTotalCycleHist", walkCycle, walkState && stateNext === s_idle, 0, 32)

  private val deqNotWritebacked = robEntries(deqPtr.value).valid && !robEntries(deqPtr.value).isWritebacked
  private val deqHeadInfo = debug_microOp(deqPtr.value).head
  private val deqHeadInfoFuType = deqHeadInfo.fuType
  val deqUopCommitType = deqHeadInfo.commitType

  XSPerfAccumulate("waitAluCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.alu.U)
  XSPerfAccumulate("waitMulCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.mul.U)
  XSPerfAccumulate("waitDivCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.div.U)
  XSPerfAccumulate("waitBrhCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.brh.U)
  XSPerfAccumulate("waitJmpCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.jmp.U)
  XSPerfAccumulate("waitLinkCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.link.U)
  XSPerfAccumulate("waitCsrCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.csr.U)
  XSPerfAccumulate("waitFenCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.fence.U)
  XSPerfAccumulate("waitBkuCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.bku.U)
  XSPerfAccumulate("waitLduCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.ldu.U)
  XSPerfAccumulate("waitStuCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.stu.U)
  XSPerfAccumulate("waitAtmCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.mou.U)

  XSPerfAccumulate("waitfaluCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.falu.U)
  XSPerfAccumulate("waitfmulCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.fmul.U)
  XSPerfAccumulate("waitfcvtCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.fcvt.U)
  XSPerfAccumulate("waitfDivSqrtCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.fDivSqrt.U)
  XSPerfAccumulate("waitfcmpCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.fcmp.U)

  XSPerfAccumulate("waitVfmacCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.vfmac.U)
  XSPerfAccumulate("waitVfdivCycle", deqNotWritebacked && deqHeadInfoFuType === FuType.vfdiv.U)

  XSPerfAccumulate("waitNormalCycle", deqNotWritebacked && deqUopCommitType === CommitType.NORMAL)
  XSPerfAccumulate("waitBranchCycle", deqNotWritebacked && FuType.isBrh(deqHeadInfoFuType))
  XSPerfAccumulate("waitLoadCycle", deqNotWritebacked   && deqUopCommitType === CommitType.LOAD)
  XSPerfAccumulate("waitStoreCycle", deqNotWritebacked  && deqUopCommitType === CommitType.STORE)
  XSPerfReference("robHeadPC",
    RegEnable(out.commits.info(0).debug_pc.getOrElse(0.U),
              0.U,
              out.commits.isCommit && out.commits.commitValid(0)))
  XSPerfAccumulate("commitCompressCntAll", PopCount(out.commits.commitValid.zip(commitInstrCntByEntry).map { case (valid, instrCnt) => out.commits.isCommit && valid && instrCnt > 1.U }))
  (1 to RenameWidth).foreach(i =>
    XSPerfAccumulate(s"commitCompressCnt${i}", PopCount(out.commits.commitValid.zip(commitInstrCntByEntry).map { case (valid, instrCnt) => out.commits.isCommit && valid && instrCnt === i.U }))
  )
  XSPerfAccumulate("compressSize", out.commits.commitValid.zip(commitInstrCntByEntry).map { case (valid, instrCnt) => Mux(out.commits.isCommit && valid && instrCnt > 1.U, instrCnt, 0.U) }.reduce(_ +& _))
  val compressTypeIsNORMAL = out.commits.commitValid.zip(out.commits.info).map(x => x._1 && CompressType.isNORMAL(x._2.entryPairType))
  val compressTypeIsCC = out.commits.commitValid.zip(out.commits.info).map(x => x._1 && CompressType.isCC(x._2.entryPairType))
  val compressTypeIsCS = out.commits.commitValid.zip(out.commits.info).map(x => x._1 && CompressType.isCS(x._2.entryPairType))
  val compressTypeIsSC = out.commits.commitValid.zip(out.commits.info).map(x => x._1 && CompressType.isSC(x._2.entryPairType))
  XSPerfAccumulate("compressType_NORMAL", ifCommit(PopCount(compressTypeIsNORMAL)))
  XSPerfAccumulate("compressType_CC", ifCommit(PopCount(compressTypeIsCC)))
  XSPerfAccumulate("compressType_CS", ifCommit(PopCount(compressTypeIsCS)))
  XSPerfAccumulate("compressType_SC", ifCommit(PopCount(compressTypeIsSC)))
  XSPerfAccumulate("RobEntriesCommited", ifCommit(PopCount(out.commits.commitValid)))

  // By CommitType
  val commitIsNormal = out.commits.info.map(_.commitType.map(_ === CommitType.NORMAL).getOrElse(false.B))

  XSPerfAccumulate("NoCompressIsLoad", ifCommit(PopCount(compressTypeIsNORMAL.zip(commitIsLoad).map(x => x._1 && x._2))))
  XSPerfAccumulate("NoCompressIsStore", ifCommit(PopCount(compressTypeIsNORMAL.zip(commitIsStore).map(x => x._1 && x._2))))
  XSPerfAccumulate("NoCompressIsBranch", ifCommit(PopCount(compressTypeIsNORMAL.zip(commitIsBranch).map(x => x._1 && x._2))))
  XSPerfAccumulate("NoCompressIsNORMAL", ifCommit(PopCount(compressTypeIsNORMAL.zip(commitIsNormal).map(x => x._1 && x._2))))

  // By FuType
  for (fuType <- FuType.functionNameMap.keys) {
    val fuName = FuType.functionNameMap(fuType)
    val commitIsFuType = compressTypeIsNORMAL.zip(deqPtrVec.map(ptr => debug_microOp(ptr.value).head)).map {
      case (commit, uop) => commit && uop.fuType === fuType.U
    }
    XSPerfAccumulate(s"NoCompress_$fuName", ifCommit(PopCount(commitIsFuType)))
    val NoCompress_Source = commitIsFuType.zip(out.commits.info).map {
      case (valid, info) =>
        val noEnoughInstr  = valid && info.noCompressReason.map(_ === NoCompressReason.noEnoughInstr).getOrElse(false.B)
        val flushedHalf    = valid && info.noCompressReason.map(_ === NoCompressReason.flushedHalf).getOrElse(false.B)
        val cannotCompress = valid && info.noCompressReason.map(_ === NoCompressReason.cannotCompress).getOrElse(false.B)
        (noEnoughInstr, flushedHalf, cannotCompress)
    }
    XSPerfAccumulate(s"NoCompress_${fuName}_by_noEnoughInstr", ifCommit(PopCount(NoCompress_Source.map(_._1))))
    XSPerfAccumulate(s"NoCompress_${fuName}_by_flushedHalf", ifCommit(PopCount(NoCompress_Source.map(_._2))))
    XSPerfAccumulate(s"NoCompress_${fuName}_by_cannotCompress", ifCommit(PopCount(NoCompress_Source.map(_._3))))
  }

  val dispatchLatency = commitDebugUop.map(uop => debugMeta(uop).perfDebugInfo.dispatchTime - debugMeta(uop).perfDebugInfo.renameTime)
  val enqRsLatency = commitDebugUop.map(uop => debugMeta(uop).perfDebugInfo.enqRsTime - debugMeta(uop).perfDebugInfo.dispatchTime)
  val selectLatency = commitDebugUop.map(uop => debugMeta(uop).perfDebugInfo.selectTime - debugMeta(uop).perfDebugInfo.enqRsTime)
  val issueLatency = commitDebugUop.map(uop => debugMeta(uop).perfDebugInfo.issueTime - debugMeta(uop).perfDebugInfo.selectTime)
  val executeLatency = commitDebugUop.map(uop => debugMeta(uop).perfDebugInfo.writebackTime - debugMeta(uop).perfDebugInfo.issueTime)
  val rsFuLatency = commitDebugUop.map(uop => debugMeta(uop).perfDebugInfo.writebackTime - debugMeta(uop).perfDebugInfo.enqRsTime)
  val commitLatency = commitDebugUop.map(uop => timer - debugMeta(uop).perfDebugInfo.writebackTime)
  val tlbLatency = commitDebugUop.map(uop => debugMeta(uop).perfDebugInfo.tlbRespTime - debugMeta(uop).perfDebugInfo.tlbFirstReqTime)

  def latencySum(cond: Seq[Bool], latency: Seq[UInt]): UInt = {
    cond.zip(latency).map(x => Mux(x._1, x._2, 0.U)).reduce(_ +& _)
  }

  for (fuType <- FuType.functionNameMap.keys) {
    val fuName = FuType.functionNameMap(fuType)
    val commitIsFuType = commitSlotValid.zip(commitDebugUop).map(x => x._1 && x._2.fuType === fuType.U)
    XSPerfRolling(s"ipc_futype_${fuName}", ifCommit(PopCount(commitIsFuType)), 1000, clock, reset)
    XSPerfAccumulate(s"${fuName}_instr_cnt", ifCommit(PopCount(commitIsFuType)))
    XSPerfAccumulate(s"${fuName}_latency_dispatch", ifCommit(latencySum(commitIsFuType, dispatchLatency)))
    XSPerfAccumulate(s"${fuName}_latency_enq_rs", ifCommit(latencySum(commitIsFuType, enqRsLatency)))
    XSPerfAccumulate(s"${fuName}_latency_select", ifCommit(latencySum(commitIsFuType, selectLatency)))
    XSPerfAccumulate(s"${fuName}_latency_issue", ifCommit(latencySum(commitIsFuType, issueLatency)))
    XSPerfAccumulate(s"${fuName}_latency_execute", ifCommit(latencySum(commitIsFuType, executeLatency)))
    XSPerfAccumulate(s"${fuName}_latency_enq_rs_execute", ifCommit(latencySum(commitIsFuType, rsFuLatency)))
    XSPerfAccumulate(s"${fuName}_latency_commit", ifCommit(latencySum(commitIsFuType, commitLatency)))
  }
  XSPerfAccumulate(s"redirect_use_snapshot", in.redirect.valid && in.snpt.useSnpt)

  /**
   * Complex performance counters like TopDown
   */

  // Topdown
  out.debugTopDown.toCore.robHeadVaddr.valid := debug_lsTopdownInfo(deqPtr.value).s1.vaddr_valid
  out.debugTopDown.toCore.robHeadVaddr.bits := debug_lsTopdownInfo(deqPtr.value).s1.vaddr_bits
  out.debugTopDown.toCore.robHeadPaddr.valid := debug_lsTopdownInfo(deqPtr.value).s2.paddr_valid
  out.debugTopDown.toCore.robHeadPaddr.bits := debug_lsTopdownInfo(deqPtr.value).s2.paddr_bits
  out.debugTopDown.toDispatch.robTrueCommit := ifCommitReg(trueCommitCnt)
  out.debugTopDown.toDispatch.robHeadLsIssue := debug_lsIssue(deqPtr.value)
  out.debugTopDown.robHeadLqIdx.valid := debug_lqIdxValid(deqPtr.value)(0)
  out.debugTopDown.robHeadLqIdx.bits := debug_lqIdx(deqPtr.value)(0)

  val notIssue = !debug_lsIssue(deqPtr.value)
  val tlbReplay = in.debugTopDown.fromCore.fromMem.robHeadTlbReplay
  val tlbMiss = in.debugTopDown.fromCore.fromMem.robHeadTlbMiss
  val vioReplay = in.debugTopDown.fromCore.fromMem.robHeadLoadVio
  val mshrReplay = in.debugTopDown.fromCore.fromMem.robHeadLoadMSHR
  val l1Miss = in.debugTopDown.fromCore.fromMem.robHeadMissInDCache
  val l2Miss = in.debugTopDown.fromCore.l2MissMatch
  val l3Miss = in.debugTopDown.fromCore.l3MissMatch
  val ldReason = Mux(l3Miss, LoadMemStall.id.U,
    Mux(l2Miss, LoadL3Stall.id.U,
      Mux(l1Miss, LoadL2Stall.id.U,
        Mux(notIssue, MemNotReadyStall.id.U,
          Mux(tlbMiss, LoadTLBStall.id.U,
            Mux(tlbReplay, LoadTLBStall.id.U,
              Mux(mshrReplay, LoadMSHRReplayStall.id.U,
                Mux(vioReplay, LoadVioReplayStall.id.U,
                  LoadL1Stall.id.U))))))))

  val allIssueParams = backendParams.allIssueParams.filter(_.StdCnt == 0)
  val allExuParams = allIssueParams.map(_.exuBlockParams).flatten
  val allFuConfigs = allExuParams.map(_.fuConfigs).flatten.toSet.toSeq
  val sortedFuConfigs = allFuConfigs.sortBy(_.fuType.id)
  val sortedFulatency = sortedFuConfigs.map(_.latency.latencyVal.getOrElse(4).asUInt)
  val bypassLatency = 3.U
  val writeBackLatency = 1.U

  if (backendParams.debugEn) {
    val entryDebug = robEntryDebug.get
    val topdownRobInfoCollect = Module(new TopdownRobInfoCollect(backendParams.iqEntryNum, RobSize))
    topdownRobInfoCollect.io.in.zip(in.topdownIQInfoVec.get).foreach{ case (sink, source) =>
      sink.valid := source.valid
      sink.robIdx := source.bits.robIdx.value
      sink.robFlag := source.bits.robIdx.flag
      sink.issued := source.bits.issued
      sink.cancelSource := source.bits.cancelSource
      sink.idealIssueTime := source.bits.idealIssueTime
    }
    val topdownRobCandidate = topdownRobInfoCollect.io.out
    for (i <- 0 until RobSize) {
      when(robEntries(i).valid){
        val debug = entryDebug(i)
        val hasWriteBack = robEntries(i).isWritebacked
        val isRobHead = i.U === deqPtr.value

        val topdownIQInfoCandidate = Mux(topdownRobCandidate(i).valid, topdownRobCandidate(i),
          0.U.asTypeOf(new TopdownRobInfo))
        val topdownIQInfoValid = topdownIQInfoCandidate.valid
        val topdownIQIssued = topdownIQInfoCandidate.issued
        val topdownIQIdealIssue = topdownIQInfoCandidate.idealIssueTime

        val topdownIQCancelSource = topdownIQInfoCandidate.cancelSource

        val topdownIssuedUpdate = topdownIQIssued || debug.topdownIssued.get
        val topdownCanceled= !topdownIQIssued && debug.topdownIssued.get && topdownIQInfoValid &&
          topdownIQCancelSource =/= IQCancelSource.none
        val topdownCanceledUpdate = topdownCanceled || debug.topdownCanceled.get
        val topdownRobHeadUpdate = isRobHead || debug.topdownRobHead.get
        val topdownIdealIssueUpdate = topdownIQIdealIssue || debug.topdownIdealIssue.get
        val topdownCancelSrcUpdate = Mux(topdownCanceled, topdownIQCancelSource,
          debug.topdownCancelSource.get)
        val topdownCancelTimeUpdate = topdownCanceled
        debug.topdownCanceled.foreach(_ := topdownCanceledUpdate)
        debug.topdownIssued.foreach(_ := topdownIssuedUpdate)
        debug.topdownRobHead.foreach(_ := topdownRobHeadUpdate)
        debug.topdownIdealIssue.foreach(_ := topdownIdealIssueUpdate)

        debug.topdownIssueTime.foreach(_ := Mux(hasWriteBack , 0.U,
          debug.topdownIssueTime.get +& topdownIssuedUpdate))
        debug.topdownLastIssueTime.foreach(_ := Mux(hasWriteBack , 0.U,
          Mux(topdownCanceled, 0.U,
            debug.topdownLastIssueTime.get +& topdownIssuedUpdate)
        ))
        debug.topdownRobHeadTime.foreach(_ := Mux(hasWriteBack , 0.U,
          debug.topdownRobHeadTime.get +& topdownRobHeadUpdate))
        // only caculate rising edge of ideal issue
        debug.topdownIdealIssueTime.foreach(_ := Mux(hasWriteBack , 0.U,
          Mux(topdownIQIdealIssue && !debug.topdownIdealIssue.get , 1.U,
            debug.topdownIdealIssueTime.get +& debug.topdownIdealIssue.get)
        ))
        debug.topdownCancelSource.foreach(_ := topdownCancelSrcUpdate)
        debug.topdownCancelTimeVec.foreach(_.zipWithIndex.foreach{ case (cancelTime, index) =>
          val cancelMatch = topdownCancelSrcUpdate === index.asUInt
          cancelTime := Mux(hasWriteBack, 0.U,
            Mux(topdownCanceled && cancelMatch, debug.topdownCancelTimeVec.get(index) + debug.topdownLastIssueTime.get + 1.U,
              debug.topdownCancelTimeVec.get(index) +& (cancelMatch & topdownCancelTimeUpdate)))
        })
      }
    }
    out.debugRobHeadStall.foreach{ case stall =>
      val deqEntry = robEntries(deqPtr.value)
      val debug = entryDebug(deqPtr.value)
      val deqEntryNormalLatency = Mux1H(debug.commit.debug_fuType.get, sortedFulatency) +& bypassLatency +& writeBackLatency
      val issueTime = debug.topdownIssueTime.get
      val lastIssueTime = debug.topdownLastIssueTime.get
      val idealIssueTime = debug.topdownIdealIssueTime.get
      val robHeadTime = debug.topdownRobHeadTime.get

      val topdownIQInfoCandidate = topdownRobCandidate(deqPtr.value)
      val issued = topdownIQInfoCandidate.issued && topdownIQInfoCandidate.valid

      when(deqEntry.valid){
        assert(issueTime >= lastIssueTime)
        assert(issueTime <= idealIssueTime)
      }
      val waitTime = idealIssueTime - robHeadTime
      val idealWaitTime = idealIssueTime - issueTime
      val robHeadFutype = debug.commit.debug_fuType.get
      val robHeadExecStall = deqEntry.valid && (lastIssueTime > deqEntryNormalLatency) && (lastIssueTime > waitTime)
      val robHeadNotIssued = deqEntry.valid && (!debug.topdownIssued.get && !issued)
      val robHeadIssueCancel = deqEntry.valid && debug.topdownCanceled.get && (issueTime > deqEntryNormalLatency)
      // issueDelay priority than cancel; only calculate idealWaitTime stall
      val robHeadIssueDelay = deqEntry.valid && (idealIssueTime > deqEntryNormalLatency) &&
        (robHeadTime < idealWaitTime)
      val robHeadExecStallReason =  MuxCase(OtherNotReadyStall.id.U, Seq(
        FuType.isAMO(robHeadFutype)          -> AtomicStall.id.U          ,
        FuType.isStore(robHeadFutype)  -> StoreStall.id.U           ,
        FuType.isLoad(robHeadFutype)    -> ldReason                  ,
        FuType.isDivSqrt(robHeadFutype)      -> DivStall.id.U             ,
        FuType.isInt(robHeadFutype)          -> IntNotReadyStall.id.U     ,
        FuType.isFArith(robHeadFutype)       -> FPNotReadyStall.id.U      ,
      ))

      val robHeadCancelSource = debug.topdownCancelSource.get

      val cancelTimeVec = debug.topdownCancelTimeVec.get
      val cancelTimeFixVec = debug.topdownCancelTimeFixVec.get
      val fixedCancelTimeVec = cancelTimeVec.zip(cancelTimeFixVec).map{ case (time, fix) =>
        Mux(time < fix, 0.U, Mux((time === fix) && time.orR, 1.U, time - fix))
      }
      val maxCancelTime = fixedCancelTimeVec.reduce{ (a,b) => Mux(a > b, a, b)}
      val maxCancelTimeVec = VecInit(fixedCancelTimeVec.map(_ === maxCancelTime)).asUInt
      val lastMaxCancelTimeOH = Reverse(PriorityEncoderOH(Reverse(maxCancelTimeVec)))
      val lastMaxIdx = OHToUInt(lastMaxCancelTimeOH)
      val cancelTimeFixUpdateVec = VecInit(cancelTimeFixVec.zip(lastMaxCancelTimeOH.asBools).map{
        case (time, hit) => time +& hit
      })

      dontTouch(VecInit(lastMaxCancelTimeOH.asBools))

      val cancelStallReason = MuxCase(IssueCancelStallOther.id.U, Seq(
        IQCancelSource.isog0(lastMaxIdx) -> IssueCancelStallOg0.id.U  ,
        IQCancelSource.isog1(lastMaxIdx) -> IssueCancelStallOg1.id.U  ,
        IQCancelSource.isld(lastMaxIdx)  -> IssueCancelStallLd.id.U   ,
        IQCancelSource.isst(lastMaxIdx)  -> IssueCancelStallSt.id.U   ,
      ))
      debug.topdownCancelTimeFixVec.foreach(_ := cancelTimeFixUpdateVec)

      stall.valid := robHeadExecStall || robHeadNotIssued || robHeadIssueCancel || robHeadIssueDelay
      stall.bits := MuxCase(BackendOtherCoreStall.id.U, Seq(
        robHeadNotIssued                    -> RobHeadNotIssued.id.U     ,
        robHeadExecStall                    -> robHeadExecStallReason    ,
        robHeadIssueDelay                   -> IssueDelayStall.id.U      ,
        robHeadIssueCancel                  -> cancelStallReason         ,
      ))
    }
  }

  /**
   * PerfCCT instruction timeline (pre-silicon)
   */
  for (i <- 0 until CommitWidth) {
    val deqDebugInst = debug_microOp(deqPtrVec(i).value).head
    deqDebugInst.debug.foreach { debug =>
      PerfCCT.commitInstMeta(
        order_id = i.U,
        sn = debug.debug_seqNum.seqNum,
        block_size = commitInstrCntByEntry(i),
        en = out.commits.isCommit && out.commits.commitValid(i),
        clock = clock,
        reset = reset
      )
    }
  }

  if (backendParams.debugEn) {
    PerfCCT.tick(clock, reset)
    in.exuWriteback.zipWithIndex.foreach { case (wb, i) =>
      PerfCCT.updateInstPos(
        seq = wb.bits.debug_seqNum.get,
        pos = PerfCCT.InstPos.AtWriteVal.id.U,
        en = wb.valid,
        clock = clock,
        reset = reset
      )
    }
  }

  /**
   * Rolling to capture statistics to database:
   * */

  // connect rolling outside
  out.debugRolling.robTrueCommit := ifCommitReg(trueCommitCnt)

  // log when writeback
  if (!env.FPGAPlatform) {
    val instTableName = "InstTable" + p(XSCoreParamsKey).HartId.toString
    val instSiteName = "Rob" + p(XSCoreParamsKey).HartId.toString
    val debug_instTable = ChiselDB.createTable(instTableName, new InstInfoEntry, basicDB = true)

    // Load/store pipeline observations.
    for (i <- 0 until (LduCnt + StaCnt)) {
      debug_lsInfo(in.debug_ls.debugLsInfo(i).s1_robIdx).s1SignalEnable(in.debug_ls.debugLsInfo(i))
      debug_lsInfo(in.debug_ls.debugLsInfo(i).s2_robIdx).s2SignalEnable(in.debug_ls.debugLsInfo(i))
      debug_lsInfo(in.debug_ls.debugLsInfo(i).s3_robIdx).s3SignalEnable(in.debug_ls.debugLsInfo(i))
    }

    for (wb <- exuWBs) {
      when(wb.valid) {
        val debug_instData = Wire(new InstInfoEntry)
        val idx = wb.bits.robIdx.value
        debug_instData.robIdx := idx
        debug_instData.dvaddr := wb.bits.debug.vaddr
        debug_instData.dpaddr := wb.bits.debug.paddr
        debug_instData.issueTime := wb.bits.perfDebugInfo.get.issueTime
        debug_instData.writebackTime := wb.bits.perfDebugInfo.get.writebackTime
        debug_instData.dispatchLatency := wb.bits.perfDebugInfo.get.dispatchTime - wb.bits.perfDebugInfo.get.renameTime
        debug_instData.enqRsLatency := wb.bits.perfDebugInfo.get.enqRsTime - wb.bits.perfDebugInfo.get.dispatchTime
        debug_instData.selectLatency := wb.bits.perfDebugInfo.get.selectTime - wb.bits.perfDebugInfo.get.enqRsTime
        debug_instData.issueLatency := wb.bits.perfDebugInfo.get.issueTime - wb.bits.perfDebugInfo.get.selectTime
        debug_instData.executeLatency := wb.bits.perfDebugInfo.get.writebackTime - wb.bits.perfDebugInfo.get.issueTime
        debug_instData.rsFuLatency := wb.bits.perfDebugInfo.get.writebackTime - wb.bits.perfDebugInfo.get.enqRsTime
        debug_instData.tlbLatency := wb.bits.perfDebugInfo.get.tlbRespTime - wb.bits.perfDebugInfo.get.tlbFirstReqTime
        debug_instData.exceptType := wb.bits.exceptionVec.asUInt
        debug_instData.lsInfo := debug_lsInfo(idx)
        // debug_instData.globalID := wb.bits.uop.ctrl.debug_globalID
        // debug_instData.instType := wb.bits.uop.ctrl.fuType
        // debug_instData.ivaddr := wb.bits.uop.cf.pc
        // debug_instData.mdpInfo.ssid := wb.bits.uop.cf.ssid
        // debug_instData.mdpInfo.waitAllStore := wb.bits.uop.cf.loadWaitStrict && wb.bits.uop.cf.loadWaitBit
        debug_instTable.log(
          data = debug_instData,
          en = wb.valid,
          site = instSiteName,
          clock = clock,
          reset = reset
        )
      }
    }
  }

  // log when committing
  val load_debug_table = ChiselDB.createTable("LoadDebugTable" + p(XSCoreParamsKey).HartId.toString, new LoadInfoEntry, basicDB = true)
  for (i <- 0 until CommitWidth) {
    val commit_index = out.commits.robIdx(i).value
    for (j <- 0 until 2) {
      val slot = 2 * i + j
      val uop = commitDebugUop(slot)
      val log_enable = out.commits.isCommit && commitSlotValid(slot) && uop.commitType === CommitType.LOAD
      val load_debug_data = Wire(new LoadInfoEntry)

      load_debug_data.pc := debugMeta(uop).pc
      load_debug_data.vaddr := debug_lsTopdownInfo(commit_index).s1.vaddr_bits
      load_debug_data.paddr := debug_lsTopdownInfo(commit_index).s2.paddr_bits
      load_debug_data.cacheMiss := debug_lsTopdownInfo(commit_index).s2.first_real_miss
      load_debug_data.tlbQueryLatency := tlbLatency(slot)
      load_debug_data.exeLatency := executeLatency(slot)

      load_debug_table.log(
        data = load_debug_data,
        en = log_enable,
        site = "LoadDebugTable",
        clock = clock,
        reset = reset
      )
    }
  }

  /**
   * TIP simulation statistics
   */
  val tip_computing :: tip_stalled :: tip_walk :: tip_drained :: Nil = Enum(4)
  val tip_state = WireInit(0.U(4.W))
  when(!isEmpty) {  // One or more inst in ROB
    when(walkState || in.redirect.valid) {
      tip_state := tip_walk
    }.elsewhen(out.commits.isCommit && PopCount(out.commits.commitValid) =/= 0.U) {
      tip_state := tip_computing
    }.otherwise {
      tip_state := tip_stalled
    }
  }.otherwise {
    tip_state := tip_drained
  }
  val tip_table = ChiselDB.createTable("Tip_" + p(XSCoreParamsKey).HartId.toString, new TipEntry)
  val tip_data = Wire(new TipEntry())
  tip_data.state := tip_state
  tip_data.commits := out.commits
  tip_data.redirect := in.redirect
  tip_data.redirect_pc := debugMeta(debug_microOp(in.redirect.bits.robIdx.value)(slotIndex(in.redirect.bits.robIdx))).pc
  tip_data.debugLsInfo := debug_lsInfo(out.commits.robIdx(0).value)
  tip_table.log(tip_data, true.B, "", clock, reset)

  /**
   * Difftest
   */
  val hasCommit = out.commits.isCommit && out.commits.commitValid.asUInt.orR
  val retiredSlotPtrs = out.commits.robIdx.zip(out.commits.info).map { case (ptr, info) =>
    val retired = WireDefault(ptr)
    retired.isFormer := CompressType.isNORMAL(info.entryPairType)
    retired
  }
  val newestCommit = PriorityMuxDefault(
    out.commits.commitValid.zip(retiredSlotPtrs).reverse,
    deqPtr
  )
  out.diffRatCommitRobIdx.foreach { commitRobIdx =>
    commitRobIdx.valid := hasCommit || deqVlsExcpNeedCommit
    commitRobIdx.bits := Mux(deqVlsExcpNeedCommit, deqPtr.asFormer, newestCommit)
  }
  out.diffRatCommitRobIdxVec.foreach { commitRobIdxVec =>
    assert(!(deqVlsExcpNeedCommit && hasCommit), "VLS and normal commits overlap")
    for (i <- 0 until CommitWidth) {
      commitRobIdxVec(i).valid := out.commits.isCommit && out.commits.commitValid(i) &&
        !deqVlsExcpNeedCommit
      commitRobIdxVec(i).bits := retiredSlotPtrs(i)
    }
    when(deqVlsExcpNeedCommit) {
      commitRobIdxVec.head.valid := true.B
      commitRobIdxVec.head.bits := deqPtr.asFormer
    }
  }

  // A latter exception flushes itself but preserves its older former slot.
  // Hold only the difftest observation until that slot really retires after walk.
  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    val pending = RegInit(false.B)
    val pendingPtr = Reg(new RobPtr)
    val release = pending && out.commits.isCommit && out.commits.commitValid.head &&
      deqPtr.isSameEntry(pendingPtr)
    when(excpHappen && !excpIsFormer) {
      assert(!pending, "A latter exception observation is already pending")
      assert(!excpUopInfo.isVectorMemory, "Vector memory instructions must not be compressed")
      pending := true.B
      pendingPtr := deqPtr
    }.elsewhen(release) {
      pending := false.B
    }
    when(release) {
      assert(CompressType.isNORMAL(out.commits.info.head.entryPairType))
      assert(PopCount(out.commits.commitValid) === 1.U)
      assert(in.diffArchEvent.get.valid && in.diffArchEvent.get.bits,
        "The surviving former and its latter exception must be observed together")
    }
    diffTrapPending := pending
    out.diffCommitForTrap.get := release
  }

  val instrCntReg = RegInit(0.U(64.W))
  val instrCnt = instrCntReg + retireCounter
  when(isCommitReg){
    instrCntReg := instrCnt
  }

  val firstValidCommit = deqPtr.addEntries(PriorityMux(out.commits.commitValid, VecInit(List.tabulate(CommitWidth)(_.U(log2Up(CommitWidth).W))))).value

  val wdata = Wire(Vec(2 * CommitWidth, UInt(XLEN.W)))
  val wpc = Wire(Vec(2 * CommitWidth, UInt(XLEN.W)))

  for (i <- 0 until CommitWidth; j <- 0 until 2) {
    val idx = deqPtrVec(i).value
    wdata(2 * i + j) := debug_exuData(idx)(j)
    wpc(2 * i + j) := SignExt(debugMeta(commitDebugUop(2 * i + j)).pc, XLEN)
  }

  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    // Keep the mainline debug payload, independently sampled at each slot head.
    val fullBasicDiff = env.EnableDifftest || env.FullBasicDiff
    val dt_basicDebug = Reg(Vec(RobSize, Vec(2, new BasicDebugInfo)))
    val dt_pcTransType = Option.when(fullBasicDiff)(Reg(Vec(RobSize, Vec(2, new AddrTransType))))
    val dt_exuDebug = Reg(Vec(RobSize, Vec(2, new DebugBundle)))
    // Share vector destination history across all commit lanes.
    val debug_VecOtherPdest = Option.when(fullBasicDiff) {
      RegInit(VecInit.fill(RobSize)(VecInit.fill(8)(0.U(PhyRegIdxWidth.W))))
    }

    for (i <- 0 until RenameWidth) {
      val hasEarlierSameHalf = (0 until i).map { j =>
        enqFireVec(j) &&
          in.enq.req(j).bits.robIdx.isSameSlot(in.enq.req(i).bits.robIdx)
      }.foldLeft(false.B)(_ || _)
      val hasEarlierEntryReset = (0 until i).map { j =>
        enqFirstUopFireVec(j) &&
          in.enq.req(j).bits.robIdx.isSameEntry(in.enq.req(i).bits.robIdx)
      }.foldLeft(false.B)(_ || _)
      when(enqFireVec(i)) {
        val uop = in.enq.req(i).bits
        val ptr = uop.robIdx.value
        val slot = slotIndex(uop.robIdx)
        val slotWritten = Mux(uop.firstUop || hasEarlierEntryReset, false.B, enqHalfWritten(ptr)(slot))
        when(!hasEarlierSameHalf && !slotWritten) {
          dt_basicDebug(ptr)(slot).connectBasicDebug(uop)
          dt_pcTransType.foreach(_(ptr)(slot) := in.debugInstrAddrTransType)
        }
      }
    }
    for (wb <- exuWBs) {
      when(wb.valid) {
        dt_exuDebug(wb.bits.robIdx.value)(slotIndex(wb.bits.robIdx)) := wb.bits.debug
      }
    }

    val event = in.diffArchEvent.get
    when(event.valid) {
      when(event.bits) {
        assert(out.diffCommitForTrap.get && out.commits.isCommit && out.commits.commitValid.head)
      }.otherwise {
        assert(!hasCommit, "An event at the former slot must not overlap ordinary retirement")
      }
    }
    for (i <- 0 until CommitWidth; j <- 0 until 2) {
      val index = 2 * i + j
      val ptr = deqPtrVec(i).value
      val uop = commitDebugUop(index)
      val uopDebug = debugMeta(uop)
      val exuOut = dt_exuDebug(ptr)(j)
      val basicDebug = dt_basicDebug(ptr)(j)
      val ordinaryValid = out.commits.isCommit && commitSlotValid(index)
      val slotInstrCount = if (j == 0) {
        Mux(CompressType.isNORMAL(out.commits.info(i).entryPairType), instrSizeCommit(i), formerInstrCntCommit(i))
      } else {
        latterInstrCntCommit(i)
      }
      val skip = !basicDebug.eliminatedMove && exuOut.isSkipDiff
      val isVLoad = uop.isVectorMemory && FuType.isLoad(uop.fuType)
      val diffMaxPhyRegs = Seq(MaxPhyRegs, 2 * (V0PhyRegs + VfPhyRegs)).max
      val difftest = DifftestModule(new DiffInstrCommit(diffMaxPhyRegs), delay = 3, dontCare = true)
      difftest.coreid := in.hartId
      difftest.index := index.U
      difftest.valid := ordinaryValid
      difftest.skip := ordinaryValid && skip
      difftest.isRVC := ordinaryValid && uop.isRVC
      difftest.rfwen := ordinaryValid && uop.rfWen && basicDebug.ldest =/= 0.U
      difftest.fpwen := ordinaryValid && basicDebug.fpWen
      difftest.vecwen := ordinaryValid && basicDebug.vecWen
      difftest.v0wen := ordinaryValid && (basicDebug.v0Wen || isVLoad && basicDebug.vd === 0.U)
      difftest.wpdest := basicDebug.pdest
      difftest.wdest := Mux(isVLoad, basicDebug.vd, basicDebug.ldest)
      difftest.nFused := Mux(ordinaryValid, slotInstrCount - 1.U, 0.U)
      when(ordinaryValid) {
        assert(slotInstrCount >= 1.U)
      }
      if (fullBasicDiff) {
        // Vector instructions are never compressed; retain mainline vector mapping.
        difftest.otherwpdest := debug_VecOtherPdest.get(ptr).zipWithIndex.flatMap { case (pdest, idx) =>
          val vecDest = if (idx == 0) Mux(difftest.v0wen, pdest, pdest + V0PhyRegs.U)
            else pdest + V0PhyRegs.U
          val splitDest = (vecDest << 1).asUInt
          Seq(splitDest, splitDest + 1.U)
        }
        val pcTransType = dt_pcTransType.get(ptr)(j)
        difftest.pc := pcTransType.extend(uopDebug.pc, XLEN)
        difftest.instr := uopDebug.instr
        difftest.robIdx := ZeroExt(ptr, 10)
        difftest.lqIdx := ZeroExt(debug_lqIdx(ptr)(j).value, 7)
        difftest.sqIdx := ZeroExt(debug_sqIdx(ptr)(j).value, 7)
        difftest.isLoad := ordinaryValid && uop.commitType === CommitType.LOAD
        difftest.isStore := ordinaryValid && uop.commitType === CommitType.STORE
      }
      if (env.EnableDifftest) {
        val difftestLoadEvent = DifftestModule(new DiffLoadEvent, delay = 3)
        val loadCheck = (FuType.isAMO(uop.fuType) || FuType.isLoad(uop.fuType) || isVLoad) && !skip
        difftestLoadEvent.coreid := in.hartId
        difftestLoadEvent.index := index.U
        difftestLoadEvent.valid := ordinaryValid && loadCheck
        difftestLoadEvent.paddr := exuOut.paddr
        difftestLoadEvent.opType := LSUOpType.formLoadEventOpcode(FuType.isAMO(uop.fuType), uop.fuOpType)
        difftestLoadEvent.isAtomic := FuType.isAMO(uop.fuType)
        difftestLoadEvent.isLoad := FuType.isLoad(uop.fuType)
        difftestLoadEvent.isVLoad := isVLoad
      }
    }
  }

  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    val trapVec = commitSlotValid.zip(commitDebugUop).map { case (valid, uop) =>
      out.commits.isCommit && valid && uop.isXSTrap
    }
    val hitTrap = trapVec.reduce(_ || _)
    val difftest = DifftestModule(new DiffTrapEvent, dontCare = true)
    difftest.coreid := in.hartId
    difftest.hasTrap := hitTrap
    difftest.cycleCnt := timer
    difftest.instrCnt := instrCnt
    difftest.hasWFI := hasWFI

    if (env.EnableDifftest || env.FullBasicDiff) {
      val trapCode = PriorityMux(wdata.zip(trapVec).map(x => x._2 -> x._1))
      val trapPC = SignExt(PriorityMux(wpc.zip(trapVec).map(x => x._2 -> x._1)), XLEN)
      difftest.code := trapCode
      difftest.pc := trapPC
    }
  }

  //store evetn difftest information
  in.storeDebugInfo := DontCare
  if (env.EnableDifftest || env.FullBasicDiff) {
    in.storeDebugInfo.map{port =>
      port.pc := debugMeta(debug_microOp(port.robidx.value)(slotIndex(port.robidx))).pc
      port.storeOffset.foreach(_ := decodeTraceInstrCount(robEntries(port.robidx.value).traceBlockInPipe.iretire) - 1.U)
      // Conservatively include stores from the current head entry, even when it retires this cycle.
      port.preCommit.foreach(_ := port.robidx.isSameEntry(deqPtr))
    }
  }

  // dontTouch for debug
  if (backendParams.debugEn) {
    dontTouch(enqPtrVec)
    dontTouch(deqPtrVec)
    dontTouch(robEntries)
    dontTouch(deqEntriesBankOrder)
    dontTouch(deqBankRaddr)
    dontTouch(deqBankRaddrNext)
    dontTouch(deqBankRdata)
    dontTouch(deqBankRdataUpdate)
    val excpWBsVec = MixedVecInit(excpWBs)
    dontTouch(excpWBsVec)
    dontTouch(commit_wDeqGroup)
    dontTouch(commit_vDeqGroup)
    dontTouch(commitSizeSumSeq)
    dontTouch(walkSizeSumSeq)
    dontTouch(commitSizeSumCond)
    dontTouch(walkSizeSumCond)
    dontTouch(commitSizeSum)
    dontTouch(walkSizeSum)
    dontTouch(realDestSizeSeq)
    dontTouch(walkDestSizeSeq)
    dontTouch(out.commits)
    dontTouch(commitIsVTypeVec)
    dontTouch(walkIsVTypeVec)
    dontTouch(commitValidThisLine)
    dontTouch(commitReadAddrNext)
    dontTouch(donotNeedWalk)
    dontTouch(walkPtrVecNext)
    dontTouch(walkPtrVec)
    dontTouch(deqPtrVecNext)
    dontTouch(deqPtrVecForWalk)
    dontTouch(snapPtrReadBank)
    dontTouch(snapPtrVecForWalk)
    dontTouch(shouldWalkVec)
    dontTouch(walkFinished)
    dontTouch(changeBankAddrToDeqPtr)
  }
  if (env.EnableDifftest || env.FullBasicDiff) {
    out.commits.info.map(info => dontTouch(info.debug_pc.get))
  }

}

object Rob {
  private val PerfEventCount = 19
  private val CriticalErrorCount = 1

  class In(params: BackendParams)(implicit p: Parameters) extends XSBundle {
    // Core identity.
    val hartId = Input(UInt(hartIdLen.W))

    // Enqueue requests and admission responses.
    val enq = new RobEnqIO

    // Execution results without redirect filtering.
    val exuWriteback: MixedVec[ValidIO[WriteBackRobBundle]] = Flipped(params.genWrite2RobBundles)

    // ROB writeback reports with redirect filtering.
    val writeback: MixedVec[ValidIO[WriteBackRobBundle]] = Flipped(params.genWrite2RobBundles)

    // Writeback exception summaries.
    val writebackNeedFlush = Input(Vec(params.getWrite2RobSize(_.needExceptionGen), Bool()))

    // Accepted redirect.
    val redirect = Input(Valid(new Redirect))

    // Recovery snapshots.
    val snpt = Input(new SnapshotPort)

    // CSR control and trap state.
    val csr = new RobCSRInIO

    // Commit watchdog control.
    val commitStuckCheckEnable = Input(Bool())

    // Vector restart state.
    val vstartIsZero = Input(Bool())

    // Vector exception backpressure.
    val fromVecExcpMod = Input(new RobFromVecExcpModIO)

    // WFI control.
    val wfiEnable = Input(Bool())

    // Difftest architectural events.
    val diffArchEvent = Option.when(env.EnableDifftest || env.AlwaysBasicDiff)(Input(Valid(Bool())))

    // Store debug queries and responses.
    val storeDebugInfo = Vec(EnsbufferWidth, new RobStoreDebugIO)

    // Load/store debug events.
    val debug_ls = Flipped(new DebugLSIO)

    // LSQ enqueue observation.
    val debugEnqLsq = Input(new LsqEnqIO)

    // Head memory issue observation.
    val debugHeadLsIssue = Input(Bool())

    // Instruction address translation mode.
    val debugInstrAddrTransType = Input(new AddrTransType)

    // Core TopDown observations.
    val debugTopDown = new RobTopDownInIO

    // Issue queue TopDown samples.
    val topdownIQInfoVec = Option.when(backendParams.debugEn)(Input(Vec(backendParams.iqEntryNum, Flipped(ValidIO(new TopdownIQExtendedInfo())))))

    // Load pipeline TopDown samples.
    val lsTopdownInfo = Vec(params.LduCnt + params.HyuCnt, Input(new LsTopdownInfo))
  }

  class Out(params: BackendParams)(implicit p: Parameters) extends XSBundle {
    // ROB occupancy.
    val robFull = Output(Bool())

    // ROB commit notifications.
    val commits = Output(new RobCommitIO)

    // Register mapping notifications.
    val rabCommits = Output(new RabCommitIO)

    // VL mapping notifications.
    val vlCommits = Output(new VlCommitBundle(RabCommitWidth))

    // ROB dequeue pointer status.
    val robDeqPtr = Output(new RobPtr)

    // Flush requests.
    val flushOut = ValidIO(new Redirect)
    val flushPcInfo = Output(new RobFlushPcInfo)

    // Precise exception reports.
    val exception = ValidIO(new ExceptionInfo)

    // Guest physical address requests and readback.
    val readGPAMemAddr = ValidIO(new RobGPAMemReadAddr)
    val readGPAMemData = Input(new GPAMemEntry)

    // CSR retirement side effects.
    val csr = new RobCSROutIO

    // Vector exception recovery.
    val toVecExcpMod = Output(new RobToVecExcpModIO)

    // Decode vector configuration.
    val toDecode = new RobToDecodeIO

    // Memory retirement and ordering feedback.
    val lsq = new RobLsqIO

    // WFI status.
    val cpuWfi = Output(Bool())

    // WFI requests and quiescence acknowledgments.
    val wfi = new RobWfiIO

    // Instruction trace and backpressure.
    val trace = new RobTraceIO

    // Difftest trap notifications.
    val diffCommitForTrap = Option.when(env.EnableDifftest || env.AlwaysBasicDiff)(Output(Bool()))

    // Difftest RAT selection.
    val diffRatCommitRobIdx = if (backendParams.basicDebugEn) Some(Output(Valid(new RobPtr))) else None
    val diffRatCommitRobIdxVec =
      if (backendParams.basicDebugEn) Some(Output(Vec(CommitWidth, Valid(new RobPtr)))) else None

    // Serialization debug state.
    val debugBlockBackward = Option.when(backendParams.debugEn)(Output(Bool()))
    val debugWaitForward   = Option.when(backendParams.debugEn)(Output(Bool()))

    // Core TopDown reports.
    val debugTopDown = new RobTopDownOutIO

    // ROB head stall reason.
    val debugRobHeadStall = Option.when(backendParams.debugEn)(ValidIO(UInt(log2Ceil(TopDownCounters.NumStallReasons.id).W)))

    // Rolling retirement statistics.
    val debugRolling = new RobDebugRollingIO

    // Hardware performance events.
    val perfEvents = Output(Vec(PerfEventCount, new PerfEvent))

    // Critical error notifications.
    val criticalErrors = Output(Vec(CriticalErrorCount, Bool()))
  }

  class RobTraceIO(implicit p: Parameters) extends XSBundle {
    // Trace backpressure.
    val blockCommit = Input(Bool())

    // Trace output.
    val traceCommitInfo = new TraceBundle(hasIaddr = false, CommitWidth, IretireWidthCommited)
  }

  class RobWfiIO extends Bundle {
    // WFI request.
    val wfiReq = Output(Bool())

    // Quiescence acknowledgments.
    val safeFromMem = Input(Bool())
    val safeFromFrontend = Input(Bool())
  }

  class RobCommitVTypeIO(implicit p: Parameters) extends XSBundle {
    // Committed VTYPE.
    val vtype = ValidIO(VType())

    // Committed vsetvl indication.
    val hasVsetvl = Output(Bool())
  }

  class RobToDecodeIO(implicit p: Parameters) extends XSBundle {
    // VTYPE recovery.
    val isResumeVType = Output(Bool())
    val walkToArchVType = Output(Bool())
    val walkVType = ValidIO(VType())

    // Committed VTYPE.
    val commitVType = new RobCommitVTypeIO
  }

  class RobFromVecExcpModIO extends Bundle {
    // Vector exception backpressure.
    val busy = Bool()
  }

  class RobGPAMemReadAddr(implicit p: Parameters) extends XSBundle {
    // Faulting instruction location.
    val ftqPtr = new FtqPtr()
    val ftqOffset = UInt(FetchBlockInstOffsetWidth.W)
    val isRVC = Bool()
  }

  class RobToVecExcpModIO(implicit p: Parameters) extends XSBundle {
    // Recovery register mappings.
    val logicPhyRegMap = Vec(RabCommitWidth, ValidIO(new RegWriteFromRab))

    // Vector exception metadata.
    val excpInfo = ValidIO(new VecExcpInfo)
  }

  class RobTopDownInIO(implicit p: Parameters) extends XSBundle {
    // Memory stall feedback.
    val fromCore = new CoreDispatchTopDownIO
  }

  class RobTopDownOutIO(implicit p: Parameters) extends XSBundle {
    // Core address observations.
    val toCore = new RobCoreTopDownIO

    // Dispatch retirement observations.
    val toDispatch = new RobDispatchTopDownIO

    // Head load queue identity.
    val robHeadLqIdx = Valid(new LqPtr)
  }

  class RobStoreDebugIO(implicit p: Parameters) extends XSBundle {
    // Store PC query.
    val robidx = Input(new RobPtr)
    val pc     = Output(UInt(VAddrBits.W))

    // Difftest store ordering.
    val storeOffset = Option.when(env.EnableDifftest)(Output(UInt(log2Ceil(RenameWidth + 1).W)))

    // Queried entry at ROB head.
    val preCommit = Option.when(env.EnableDifftest)(Output(Bool()))
  }

  class RobCSRPerfInfo extends Bundle {
    // Retired instruction count.
    val retiredInstr = Output(UInt(7.W))
  }

  class RobCSRInIO(implicit p: Parameters) extends XSBundle {
    // Pending interrupt.
    val intrBitSet = Input(Bool())

    // Trap target.
    val trapTarget = Input(new TargetPCBundle)

    // WFI wakeup.
    val wfiEvent   = Input(Bool())

    // Critical error status.
    val criticalErrorState = Input(Bool())
  }

  class RobCSROutIO(implicit p: Parameters) extends XSBundle {
    // Floating-point exception flags.
    val fflags     = Output(Vec(5, Bool()))

    // Vector saturation flag.
    val vxsat      = Output(Bool())

    // Vector restart index.
    val vstart     = Output(Valid(UInt(XLEN.W)))

    // Floating-point state dirtiness.
    val dirtyFs   = Output(Bool())

    // Vector state dirtiness.
    val dirtyVs   = Output(Bool())

    // Retired instruction count.
    val perfinfo   = new RobCSRPerfInfo
  }

  class RobCSRIO(implicit p: Parameters) extends XSBundle {
    // Pending interrupt.
    val intrBitSet = Input(Bool())

    // Trap target.
    val trapTarget = Input(new TargetPCBundle)

    // WFI wakeup.
    val wfiEvent   = Input(Bool())

    // Critical error status.
    val criticalErrorState = Input(Bool())

    // Floating-point exception flags.
    val fflags     = Output(Vec(5, Bool()))

    // Vector saturation flag.
    val vxsat      = Output(Bool())

    // Vector restart index.
    val vstart     = Output(Valid(UInt(XLEN.W)))

    // Floating-point state dirtiness.
    val dirtyFs   = Output(Bool())

    // Vector state dirtiness.
    val dirtyVs   = Output(Bool())

    // Retired instruction count.
    val perfinfo   = new RobCSRPerfInfo
  }

  class RobLsqIO(implicit p: Parameters) extends XSBundle {
    // Ordered replay boundary.
    val pendingPtr = Output(new RobPtr)
    val pendingPtrNext = Output(new RobPtr)

    // MMIO serialization.
    val mmioBusy = Input(Bool())
  }

  class RobEnqIO(implicit p: Parameters) extends XSBundle {
    // Enqueue permissions.
    val canAccept = Output(Bool())
    val canAcceptForDispatch = Output(Bool())

    // ROB occupancy.
    val isEmpty = Output(Bool())

    // Enqueue uops.
    val req = Vec(RenameWidth, Flipped(ValidIO(new EnqRobUop)))
  }

  class RobCoreTopDownIO(implicit p: Parameters) extends XSBundle {
    // Head memory addresses.
    val robHeadVaddr = Valid(UInt(VAddrBits.W))
    val robHeadPaddr = Valid(UInt(PAddrBits.W))
  }

  class RobDispatchTopDownIO extends Bundle {
    // Retired instruction count.
    val robTrueCommit = Output(UInt(64.W))

    // Head memory issue status.
    val robHeadLsIssue = Output(Bool())
  }

  class RobDebugRollingIO extends Bundle {
    // Retired instruction count.
    val robTrueCommit = Output(UInt(64.W))
  }

  class TipEntry()(implicit p: Parameters) extends XSBundle {
    val state = UInt(4.W)
    val commits = new RobCommitIO()      // info of commit
    val redirect = Valid(new Redirect)   // info of redirect
    val redirect_pc = UInt(VAddrBits.W)  // PC of the redirect uop
    val debugLsInfo = new DebugLsInfo()
  }
}
