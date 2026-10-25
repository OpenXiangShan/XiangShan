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
import xiangshan.frontend.ftq.FtqPtr
import xiangshan.mem.{LqPtr, LsqEnqIO, SqPtr}
import xiangshan.backend.ctrlblock.{DebugLSIO, DebugLsInfo, LsTopdownInfo}
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.rename.{SnapshotGenerator, CompressType, NoCompressReason}
import xiangshan.backend.rob.RobBundles._
import xiangshan.backend.trace._
import chisel3.experimental.BundleLiterals._
import chisel3.util.experimental.decode.TruthTable
import xiangshan.TopDownCounters._
import xiangshan.backend.dispatch._

/**
 * [ROB-01]
 * Diplomacy wrapper for the reorder buffer. RobImp owns the hardware state;
 * this wrapper preserves a distinct module boundary in the generated design.
 * The ROB retires physical entries, each of which may contain a former slot
 * and a younger latter slot. A slot can itself represent multiple instructions.
 */
class Rob(params: BackendParams)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  override def shouldBeInlined: Boolean = false

  lazy val module = new RobImp(this)(p, params)
}

class RobImp(override val wrapper: Rob)(implicit p: Parameters, params: BackendParams) extends LazyModuleImp(wrapper)
  with HasXSParameter with HasCircularQueuePtrHelper with HasPerfEvents with HasCriticalErrors {

  private val LduCnt = params.LduCnt
  private val StaCnt = params.StaCnt
  private val HyuCnt = params.HyuCnt

  /**
   * [ROB-02]
   * ROB boundary. Unless an explicit RegNext/RegEnable appears at the assignment,
   * outputs are combinational views of the current state. These Valid interfaces
   * have no per-lane ready handshake; enqueue uses the shared canAccept contract.
   * See RobBundles.scala for entry, pointer, CSR, LSQ, and exception field types.
   */
  val io = IO(new RobIO(params))

  /**
   * [ROB-03]
   * Elaboration-time views select only ports that implement the required optional
   * payload. Completion/exception accounting uses io.writeback; unfiltered EXU
   * views are retained for observation and trace. Do not interchange the streams.
   */
  val exuWBs: Seq[ValidIO[WriteBackRobBundle]] = io.exuWriteback
  val exceptionWBs = io.writeback.filter(x => x.bits.params.needExceptionGen).toSeq
  val redirectWBs = io.writeback.filter(x => x.bits.redirect.nonEmpty).toSeq
  val branchWBs = io.exuWriteback.filter(_.bits.params.hasBrhFu).toSeq
  val isBrhOrJmpWBs = io.exuWriteback.filter(x => (x.bits.params.hasBrhFu || x.bits.params.hasJmpFu)).toSeq
  val csrWBs = io.exuWriteback.filter(x => x.bits.params.hasCSR).toSeq
  val ldWBs = io.exuWriteback.filter(x => x.bits.params.hasLoadFu).toSeq

  if (backendParams.debugEn){
    PerfCCT.tick(clock, reset)
    io.exuWriteback.zipWithIndex.foreach{ case (wb, i) =>
      PerfCCT.updateInstPos(wb.bits.debug_seqNum.get, PerfCCT.InstPos.AtWriteVal.id.U, wb.valid, clock, reset)
    }
  }

  val numExuWbPorts = exuWBs.length
  val bankAddrWidth = log2Up(CommitWidth)

  println(s"Rob: size $RobSize, numExuWbPorts: $numExuWbPorts, commitwidth: $CommitWidth")

  /**
   * [ROB-04]
   * ROB metadata is separate from destination mappings (RAB) and vtype/VL state
   * (VTypeBuffer). Only valid is reset in the entry array; other fields are
   * meaningful after allocation. bankNum is fixed at eight in this implementation.
   */
  val rab = Module(new RenameBuffer(RabSize))
  val vtypeBuffer = Module(new VTypeBuffer(VTypeBufferSize))
  val bankNum = 8
  assert(RobSize % bankNum == 0, "RobSize % bankNum must be 0")
  val robEntries = RegInit(VecInit.fill(RobSize)((new RobEntryBundle).Lit(_.valid -> false.B)))
  // pointers
  // For enqueue ptr, we don't duplicate it since only enqueue needs it.
  /**
   * Allocation/dequeue vectors name physical entries and normally use former
   * slots. Recovery has both a bank-aligned read vector and an exact starting
   * pointer; keeping them separate prevents replaying already-retired prefixes.
   */
  val enqPtrVec = Wire(Vec(RenameWidth, new RobPtr))
  val deqPtrVec = Wire(Vec(CommitWidth, new RobPtr))
  val deqPtrVec_next = Wire(Vec(CommitWidth, Output(new RobPtr)))
  private val recoverySnapshotPointer = Wire(new RobPtr)
  private val recovery = new RobRecovery(
    io.redirect, io.snpt.useSnpt, recoverySnapshotPointer, deqPtrVec_next.head,
    rab.io.status.walkEnd, vtypeBuffer.io.status.walkEnd
  )
  val walkPtrVec = recovery.pointers
  val walkPtrTrue = recovery.exactPointer
  val lastWalkPtr = recovery.lastPointer
  val allowEnqueue = RegInit(true.B)
  val allowEnqueueForDispatch = RegInit(true.B)
  val vecExcpInfo = RegInit(ValidIO(new VecExcpInfo).Lit(
    _.valid -> false.B,
  ))

  /**
   * Enqueue (from dispatch)
   */
  // special cases
  /**
   * [ROB-05]
   * Admission combines registered capacity headroom, serialization, both side
   * buffers, and vector exception recovery. Only firstUop allocates a physical
   * entry; every accepted uop may contribute a RAB mapping and slot metadata.
   */
  val hasBlockBackward = RegInit(false.B)
  val hasWaitForward = RegInit(false.B)
  val enqPtr = enqPtrVec(0)
  val deqPtr = deqPtrVec(0)
  val walkPtr = walkPtrVec(0)
  /**
   * Prefix-count physical entry heads, rather than uops or architectural instructions.
   */
  private val enqueueEvents = new RobEnqueueEvents(io.enq.req, io.enq.canAccept, enqPtrVec)
  val allocatePtrVec = enqueueEvents.allocatedPointers
  io.enq.canAccept := allowEnqueue && !hasBlockBackward && rab.io.canEnq && vtypeBuffer.io.canEnq && !io.fromVecExcpMod.busy
  io.enq.canAcceptForDispatch := allowEnqueueForDispatch && !hasBlockBackward && rab.io.canEnqForDispatch && vtypeBuffer.io.canEnqForDispatch && !io.fromVecExcpMod.busy
  io.enq.resp := allocatePtrVec
  val uopCanEnqueue = enqueueEvents.acceptedUops
  val canEnqueue = enqueueEvents.allocatedEntries
  val timer = GTimer()

  /**
   * [ROB-06]
   * Rename encodes (instruction count, non-RVC count) into a dense nonzero code.
   * Decode it separately into instruction count and 16-bit halfword count;
   * halfwords = instructions + non-RVC instructions. Code zero is unspecified.
   * The byte length of the former slot is needed when only the latter redirects.
   */
  private val traceCodec = new RobTraceCodec
  import traceCodec.{decodeTraceInstrCount, decodeTraceHalfWords, traceHalfWordsToBytes, traceInstrCountWidth}

  // robEntries enqueue
  /**
   * [ROB-07]
   * Allocate entry-wide metadata from its unique head lane. The entry trace
   * describes the youngest accepted uop; a separate former trace survives a
   * latter-only squash. Multiple uops may target one entry, but only one head may.
   */
  for (i <- 0 until RobSize) {
    val enqOH = VecInit(canEnqueue.zip(allocatePtrVec.map(_.value === i.U)).map(x => x._1 && x._2))
    val entryEnqValid = uopCanEnqueue.zip(io.enq.req).map { case (valid, req) =>
      valid && req.bits.robIdx.value === i.U
    }
    assert(PopCount(enqOH) < 2.U, s"robEntries$i enqOH is not one hot")
    when(enqOH.asUInt.orR && !io.redirect.valid){
      // TODO: enq
      val entryHeadUop = Mux1H(enqOH, io.enq.req.map(_.bits))
      connectEnq(robEntries(i), entryHeadUop)
      val youngestEnqUop = PriorityMux(entryEnqValid.reverse, io.enq.req.map(_.bits).reverse)
      robEntries(i).traceBlockInPipe.itype := youngestEnqUop.traceBlockInPipe.itype
      robEntries(i).traceBlockInPipe.ilastsize := youngestEnqUop.traceBlockInPipe.ilastsize
      robEntries(i).formerTraceBlockInPipe.iretire := entryHeadUop.formerTraceIretire
      val formerEntryEnqValid = entryEnqValid.zip(io.enq.req).map { case (valid, req) =>
        valid && req.bits.robIdx.isFormer
      }
      when(VecInit(formerEntryEnqValid).asUInt.orR) {
        val youngestFormerUop = PriorityMux(formerEntryEnqValid.reverse, io.enq.req.map(_.bits).reverse)
        robEntries(i).formerTraceBlockInPipe.itype := youngestFormerUop.traceBlockInPipe.itype
        robEntries(i).formerTraceBlockInPipe.ilastsize := youngestFormerUop.traceBlockInPipe.ilastsize
      }
    }.elsewhen(io.redirect.valid && io.redirect.bits.robIdx.value === i.U){
      val entryPairType = robEntries(i).entryPairType
      val redirectIsFormer = io.redirect.bits.robIdx.isFormer
      val redirectFlushItSelf = io.redirect.bits.flushItself()
      /**
       * Keep the former slot for former+flushAfter or latter+flushItself. Re-encode
       * the physical entry as NORMAL and restore the exact former trace metadata.
       * The packed completion state and destination count are repaired below.
       */
      val downgradeToFormer = CompressType.isNotNORMAL(entryPairType) && (
        redirectIsFormer && !redirectFlushItSelf ||
        !redirectIsFormer && redirectFlushItSelf
      )
      when(downgradeToFormer) {
        robEntries(i).entryPairType := CompressType.NORMAL
        robEntries(i).noCompressReason := NoCompressReason.flushedHalf
        robEntries(i).traceBlockInPipe := robEntries(i).formerTraceBlockInPipe
      }
    }
  }
  // robBanks0 include robidx : 0 8 16 24 32 ...
  /**
   * [ROB-08]
   * Interleaved register banks: bank b holds entries b, b+8, b+16, ... . A shared
   * one-hot row address reads one line; rotating each bank's data reads the next
   * line with the same address. This is a register/mux organization, not an SRAM.
   */
  val robBanks = VecInit((0 until bankNum).map(i => VecInit(robEntries.zipWithIndex.filter(_._2 % bankNum == i).map(_._1))))
  // each Bank has 20 Entries, read addr is one hot
  // all banks use same raddr
  val eachBankEntrieNum = robBanks(0).length
  val robBanksRaddrThisLine = RegInit(1.U(eachBankEntrieNum.W))
  val robBanksRaddrNextLine = Wire(UInt(eachBankEntrieNum.W))
  robBanksRaddrThisLine := robBanksRaddrNextLine
  val bankNumWidth = log2Up(bankNum)
  val deqPtrWidth = deqPtr.value.getWidth
  val highDeqPtrThisLine = deqPtr.value(deqPtrWidth - 1, bankNumWidth)
  val highDeqPtrMax = (RobSize - 1).U(deqPtrWidth - 1, bankNumWidth)
  val highDeqPtrNextLine = Mux(highDeqPtrThisLine === highDeqPtrMax, 0.U, highDeqPtrThisLine + 1.U)
  val robIdxThisLine = VecInit((0 until bankNum).map(i => Cat(highDeqPtrThisLine, i.U(bankNumWidth.W))))
  val robIdxNextLine = VecInit((0 until bankNum).map(i => Cat(highDeqPtrNextLine, i.U(bankNumWidth.W))))
  // robBanks read
  val robBanksRdataThisLine = VecInit(robBanks.map{ case bank =>
    Mux1H(robBanksRaddrThisLine, bank)
  })
  val robBanksRdataNextLine = VecInit(robBanks.map{ case bank =>
    val shiftBank = bank.drop(1) :+ bank(0)
    Mux1H(robBanksRaddrThisLine, shiftBank)
  })
  val robBanksRdataThisLineUpdate = Wire(Vec(CommitWidth, new RobEntryBundle))
  val robBanksRdataNextLineUpdate = Wire(Vec(CommitWidth, new RobEntryBundle))
  val needUpdateCommitW = Wire(Vec(2 * CommitWidth, Bool()))
  val commitValidThisLine = Wire(Vec(CommitWidth, Bool()))
  /**
   * [ROB-09]
   * Track entries already retired from the currently cached row. This row mask
   * is indexed by bank position, not by the head-relative output lane. It lets
   * partial-row retirement retain the row until its last position retires.
   */
  val hasCommitted = RegInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val donotNeedWalk = recovery.prefixSkip
  val allCommitted = Wire(Bool())

  when(allCommitted) {
    hasCommitted := 0.U.asTypeOf(hasCommitted)
  }.elsewhen(io.commits.isCommit){
    for (i <- 0 until CommitWidth){
      hasCommitted(i) := commitValidThisLine(i) || hasCommitted(i)
    }
  }
  allCommitted := io.commits.isCommit && commitValidThisLine.last
  val walkPtrHead = Wire(new RobPtr)
  /**
   * At the last recovery line, steer the shared bank reader back to the commit head.
   */
  val changeBankAddrToDeqPtr = walkPtrVec.head.addEntries(CommitWidth.U).isAfterSlot(lastWalkPtr)
  when(io.redirect.valid){
    robBanksRaddrNextLine := UIntToOH(walkPtrHead.value(walkPtrHead.value.getWidth-1, bankAddrWidth))
  }.elsewhen(allCommitted || io.commits.isWalk && !changeBankAddrToDeqPtr){
    robBanksRaddrNextLine := Mux(robBanksRaddrThisLine.head(1) === 1.U, 1.U, robBanksRaddrThisLine << 1)
  }.elsewhen(io.commits.isWalk && changeBankAddrToDeqPtr){
    robBanksRaddrNextLine := UIntToOH(deqPtr.value(deqPtr.value.getWidth-1, bankAddrWidth))
  }.otherwise(
    robBanksRaddrNextLine := robBanksRaddrThisLine
  )
  /**
   * [ROB-10]
   * Registered commit/walk metadata window. needUpdateCommitW is computed from
   * the same next packed completion state used by the entry array, so this
   * window sees current-cycle writebacks without an additional completion bubble.
   * When the current row finishes, capture the prefetched next row instead.
   */
  val robDeqGroup = Reg(Vec(bankNum, new RobCommitEntryBundle))
  val rawInfo = VecInit((0 until CommitWidth).map(i => robDeqGroup(deqPtrVec(i).value(bankAddrWidth-1, 0)))).toSeq
  val commitInfo = VecInit((0 until CommitWidth).map(i => robDeqGroup(deqPtrVec(i).value(bankAddrWidth-1,0)))).toSeq
  val walkInfo = VecInit((0 until CommitWidth).map(i => robDeqGroup(walkPtrVec(i).value(bankAddrWidth-1, 0)))).toSeq
  for (i <- 0 until CommitWidth) {
    connectCommitEntry(robDeqGroup(i), robBanksRdataThisLineUpdate(i), needUpdateCommitW(i))
    when(allCommitted){
      connectCommitEntry(robDeqGroup(i), robBanksRdataNextLineUpdate(i), needUpdateCommitW(i + CommitWidth))
    }
  }

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
  /**
   * [ROB-11]
   * Recover total and per-slot architectural instruction counts from trace
   * codes. NORMAL can still represent a run of simple instructions; it does
   * not imply exactly one retired instruction. iretireCommit is in halfwords.
   */
  val iretireCommit = Wire(Vec(CommitWidth, UInt(IretireWidthCommited.W)))
  val instrSizeCommit = Wire(Vec(CommitWidth, UInt(traceInstrCountWidth.W)))
  val formerInstrCntCommit = Wire(Vec(CommitWidth, UInt(traceInstrCountWidth.W)))
  val latterInstrCntCommit = Wire(Vec(CommitWidth, UInt(traceInstrCountWidth.W)))
  val formerHalfWordsCommit = Wire(Vec(CommitWidth, UInt(IretireWidthCommited.W)))
  val formerLenCommit = Wire(Vec(CommitWidth, UInt(log2Ceil(RenameWidth * 4 + 1).W)))

  for (i <- 0 until CommitWidth) {
    iretireCommit(i) := decodeTraceHalfWords(rawInfo(i).traceBlockInPipe.iretire)
    instrSizeCommit(i) := decodeTraceInstrCount(rawInfo(i).traceBlockInPipe.iretire)
    formerInstrCntCommit(i) := decodeTraceInstrCount(rawInfo(i).formerTraceIretire)
    formerHalfWordsCommit(i) := decodeTraceHalfWords(rawInfo(i).formerTraceIretire)
    latterInstrCntCommit(i) := Mux(
      CompressType.isNotNORMAL(rawInfo(i).entryPairType),
      instrSizeCommit(i) - formerInstrCntCommit(i),
      0.U
    )
    formerLenCommit(i) := traceHalfWordsToBytes(formerHalfWordsCommit(i))
    when(rawInfo(i).commit_v && CompressType.isNotNORMAL(rawInfo(i).entryPairType)) {
      assert(instrSizeCommit(i) >= formerInstrCntCommit(i))
    }
    when(rawInfo(i).commit_v && CompressType.isNORMAL(rawInfo(i).entryPairType)) {
      assert(latterInstrCntCommit(i) === 0.U)
    }
  }
  val commitInstrCntByEntry = instrSizeCommit.toSeq
  /**
   * Frontend commit bookkeeping uses the preceding FTQ block. rawInfo keeps the
   * original FTQ coordinates for trace and flush PC reconstruction; do not merge
   * these two views just because they select the same registered entry.
   */
  for (i <- 0 until CommitWidth) {
    commitInfo(i).ftqOffset := 0.U
    commitInfo(i).ftqIdx := rawInfo(i).ftqIdx - 1.U
  }

  // data for debug
  // Warn: debug_* prefix should not exist in generated verilog.
  /**
   * [ROB-12]
   * Per-entry, per-slot payload retains the first instruction metadata and the
   * latest execution observation for each slot. Some fields are also consumed
   * by current functional paths (LSQ counts and exception payload selection),
   * so a debug_ name alone does not establish that a register can be removed.
   */
  val debug_microOp = Reg(Vec(RobSize, Vec(2, new EnqRobUop)))
  val debug_lqIdx = Reg(Vec(RobSize, Vec(2, new LqPtr)))
  val debug_sqIdx = Reg(Vec(RobSize, Vec(2, new SqPtr)))
  val debug_exuData = Reg(Vec(RobSize, Vec(2, UInt(XLEN.W))))
  val debug_exuDebug = Reg(Vec(RobSize, Vec(2, new DebugBundle)))
  val debug_lsInfo = RegInit(VecInit(Seq.fill(RobSize)(DebugLsInfo.init)))
  val debug_lsTopdownInfo = RegInit(VecInit(Seq.fill(RobSize)(LsTopdownInfo.init)))
  val debug_lqIdxValid = RegInit(VecInit.fill(RobSize)(VecInit(Seq.fill(2)(false.B))))
  val debug_lsIssued = RegInit(VecInit.fill(RobSize)(false.B))
  val enqHalfWritten = RegInit(VecInit(Seq.fill(RobSize)(VecInit(Seq.fill(2)(false.B)))))

  /**
   * Former maps to array index zero; latter maps to index one.
   */
  private def slotIndex(ptr: RobPtr): UInt = (!ptr.isFormer).asUInt
  private def debugMeta(uop: EnqRobUop): RenameOutUopDebug =
    uop.debug.getOrElse(0.U.asTypeOf(new RenameOutUopDebug))

  /**
   * [ROB-13]
   * Emptiness and occupancy count physical entries. Snapshots capture the exact
   * starting ROB pointer plus consecutive physical pointers; walk aligns only
   * the reader, while preserving the exact lower bound separately.
   */
  val isEmpty = enqPtr.isSameEntry(deqPtr)
  val snptEnq = io.enq.canAccept && io.enq.req.map(x => x.valid && x.bits.snapshot).reduce(_ || _)
  val snapshotPtrVec = Wire(Vec(CommitWidth, new RobPtr))
  snapshotPtrVec(0) := io.enq.req(0).bits.robIdx
  for (i <- 1 until CommitWidth) {
    snapshotPtrVec(i) := snapshotPtrVec(0).addEntries(i.U).asFormer
  }
  val snapshots = SnapshotGenerator(snapshotPtrVec, snptEnq, io.snpt.snptDeq, io.redirect.valid, io.snpt.flushVec)
  val debug_lsIssue = WireDefault(debug_lsIssued)
  debug_lsIssue(deqPtr.value) := io.debugHeadLsIssue

  /**
   * states of Rob
   */
  /**
   * [ROB-14]
   * Two-state ROB control: normal retirement or replay of surviving rename state.
   * The walk is forward from head/snapshot toward the redirect boundary; it is
   * not a reverse scan that frees squashed entries. RAB/VTB must also finish.
   */
  val s_idle = recovery.idle
  val s_walk = recovery.walk
  val state = recovery.state
  val state_next = recovery.nextState

  /**
   * [ROB-15]
   * TIP database classification is observational: drained, recovering, retiring,
   * or stalled. It does not add ROB control states or alter retirement gating.
   */
  val tip_computing :: tip_stalled :: tip_walk :: tip_drained :: Nil = Enum(4)
  val tip_state = WireInit(0.U(4.W))
  when(!isEmpty) {  // One or more inst in ROB
    when(state === s_walk || io.redirect.valid) {
      tip_state := tip_walk
    }.elsewhen(io.commits.isCommit && PopCount(io.commits.commitValid) =/= 0.U) {
      tip_state := tip_computing
    }.otherwise {
      tip_state := tip_stalled
    }
  }.otherwise {
    tip_state := tip_drained
  }
  class TipEntry()(implicit p: Parameters) extends XSBundle {
    val state = UInt(4.W)
    val commits = new RobCommitIO()      // info of commit
    val redirect = Valid(new Redirect)   // info of redirect
    val redirect_pc = UInt(VAddrBits.W)  // PC of the redirect uop
    val debugLsInfo = new DebugLsInfo()
  }
  val tip_table = ChiselDB.createTable("Tip_" + p(XSCoreParamsKey).HartId.toString, new TipEntry)
  val tip_data = Wire(new TipEntry())
  tip_data.state := tip_state
  tip_data.commits := io.commits
  tip_data.redirect := io.redirect
  tip_data.redirect_pc := debugMeta(debug_microOp(io.redirect.bits.robIdx.value)(slotIndex(io.redirect.bits.robIdx))).pc
  tip_data.debugLsInfo := debug_lsInfo(io.commits.robIdx(0).value)
  tip_table.log(tip_data, true.B, "", clock, reset)

  /**
   * ExceptionGen retains the oldest exceptional slot and merges later reports for that slot.
   */
  val exceptionGen = Module(new ExceptionGen(params))
  val exceptionDataRead = exceptionGen.io.state
  io.robDeqPtr := deqPtr

  /**
   * connection of [[rab]]
   */
  /**
   * [ROB-16]
   * RAB receives every admitted uop, then filters needEnqRab internally. Commit
   * and walk quotas count destination mappings, not physical entries. Prefix
   * sums exclude positions already consumed from the cached ROB row.
   */
  rab.io.redirect.valid := io.redirect.valid

  rab.io.req.zip(io.enq.req).map { case (dest, src) =>
    dest.bits := src.bits
    dest.valid := src.valid && io.enq.canAccept
  }

  // TODO:
  val walkDestSizeDeqGroup = RegInit(VecInit(Seq.fill(CommitWidth)(0.U(log2Up(MaxUopSize + 1).W))))
  val realDestSizeSeq = VecInit(robDeqGroup.zip(hasCommitted).map{case (r, h) => Mux(h, 0.U, r.realDestSize)})
  val walkDestSizeSeq = VecInit(robDeqGroup.zip(donotNeedWalk).map{case (r, d) => Mux(d, 0.U, r.realDestSize)})
  val commitSizeSumSeq = VecInit((0 until CommitWidth).map(i => realDestSizeSeq.take(i + 1).reduce(_ +& _)))
  val walkSizeSumSeq   = VecInit((0 until CommitWidth).map(i => walkDestSizeSeq.take(i + 1).reduce(_ +& _)))
  val commitSizeSumCond = VecInit(commitValidThisLine.zip(hasCommitted).map{case (c,h) => (c || h) && io.commits.isCommit})
  val walkSizeSumCond   = VecInit(io.commits.walkValid.zip(donotNeedWalk).map{case (w,d) => (w || d) && io.commits.isWalk})
  val commitSizeSum = PriorityMuxDefault(commitSizeSumCond.reverse.zip(commitSizeSumSeq.reverse), 0.U)
  val walkSizeSum   = PriorityMuxDefault(walkSizeSumCond.reverse.zip(walkSizeSumSeq.reverse), 0.U)

  /**
   * Vector memory exception recovery can issue a one-shot destination quota outside ordinary retirement.
   */
  val deqVlsExceptionNeedCommit = RegInit(false.B)
  val deqVlsExceptionCommitSize = RegInit(0.U(log2Up(MaxUopSize + 1).W))
  val deqVlsCanCommit= RegInit(false.B)
  rab.io.fromRob.commitSize := Mux(deqVlsExceptionNeedCommit, deqVlsExceptionCommitSize, commitSizeSum)
  rab.io.fromRob.walkSize := walkSizeSum
  rab.io.fromRob.vecLoadExcp.valid := RegNext(exceptionDataRead.valid && exceptionDataRead.bits.isVecLoad)
  rab.io.fromRob.vecLoadExcp.bits.isStrided := RegEnable(exceptionDataRead.bits.isStrided, exceptionDataRead.valid)
  rab.io.fromRob.vecLoadExcp.bits.isVlm := RegEnable(exceptionDataRead.bits.isVlm, exceptionDataRead.valid)
  rab.io.snpt := io.snpt
  rab.io.snpt.snptEnq := snptEnq

  // pipe rab commits for better timing and area
  io.rabCommits := RegNext(rab.io.commits)
  io.vlCommits := RegNext(vtypeBuffer.io.commits)

  /**
   * connection of [[vtypeBuffer]]
   */

  /**
   * [ROB-17]
   * VTypeBuffer stores VL/vtype changes separately. ROB supplies the number of
   * vset-bearing entries retired or replayed, while the buffer expands that
   * quota into its own mapping stream and recovers decode's speculative vtype.
   */
  vtypeBuffer.io.redirect.valid := io.redirect.valid

  vtypeBuffer.io.req.zip(io.enq.req).map { case (sink, source) =>
    sink.valid := source.valid && io.enq.canAccept
    sink.bits := source.bits
  }

  private val commitIsVTypeVec = VecInit(io.commits.commitValid.zip(io.commits.info).map { case (valid, info) => io.commits.isCommit && valid && info.needVTB })
  private val walkIsVTypeVec = VecInit(io.commits.walkValid.zip(walkInfo).map { case (valid, info) => io.commits.isWalk && valid && info.needVTB })
  vtypeBuffer.io.fromRob.commitSize := PopCount(commitIsVTypeVec)
  vtypeBuffer.io.fromRob.walkSize := PopCount(walkIsVTypeVec)
  vtypeBuffer.io.snpt := io.snpt
  vtypeBuffer.io.snpt.snptEnq := snptEnq
  io.toDecode.walkToArchVType := vtypeBuffer.io.toDecode.walkToArchVType
  io.toDecode.commitVType := vtypeBuffer.io.toDecode.commitVType
  io.toDecode.walkVType := vtypeBuffer.io.toDecode.walkVType

  // When blockBackward instruction leaves Rob (commit or walk), hasBlockBackward should be set to false.B
  // To reduce registers usage, for hasBlockBackward cases, we allow enqueue after ROB is empty.
  // TODO: How about a lot of fence instr and XSTrap instr
  /**
   * [ROB-18]
   * Serialization flags have deliberately different lifetimes: blockBackward
   * stays set until ROB empties; waitForward clears on any commit/walk event.
   * The following enqueue assignments can set them again in the same cycle.
   */
  val hasWB = VecInit(exuWBs.map(wb => wb.valid)).asUInt.orR
  //  when(isEmpty || (hasBlockBackward && hasWB)) {
  when(isEmpty) {
    hasBlockBackward := false.B
  }
  // When any instruction commits, hasNoSpecExec should be set to false.B
  when(io.commits.hasWalkInstr || io.commits.hasCommitInstr) {
    hasWaitForward := false.B
  }

  io.debugWaitForward.foreach(_ := hasWaitForward)
  io.debugBlockBackward.foreach(_ := hasBlockBackward)

  // The wait-for-interrupt (WFI) instruction waits in the ROB until an interrupt might need servicing.
  // io.csr.wfiEvent will be asserted if the WFI can resume execution, and we change the state to s_wfi_idle.
  // It does not affect how interrupts are serviced. Note that WFI is noSpecExec and it does not trigger interrupts.
  /**
   * [ROB-19]
   * An admitted WFI blocks retirement until a delayed wake event, flush, timeout,
   * or CSR disable releases it. cpu_wfi also requires memory/frontend quiescence.
   * When enabled at elaboration, the timeout counter advances while WFI waits.
   */
  private val wfiControl = new RobWfiControl(
    admittedWfi = io.enq.req.map(req =>
      io.enq.canAccept && req.valid && req.bits.isWFI &&
      !req.bits.exceptionVec.orR && !TriggerAction.isDmode(req.bits.trigger)),
    wakeEvent = io.csr.wfiEvent,
    flush = io.flushOut.valid,
    enabled = io.wfi_enable,
    safeFromMem = io.wfi.safeFromMem,
    safeFromFrontend = io.wfi.safeFromFrontend
  )
  val hasWFI = wfiControl.waiting
  io.wfi.wfiReq := wfiControl.request
  io.cpu_wfi := wfiControl.safeToIdle

  /**
   * [ROB-20]
   * Capture slot-head debug metadata once even when split uops arrive over
   * several cycles. Earlier lanes in this cycle participate in the duplicate
   * check because enqHalfWritten still contains the previous cycle's state.
   * The first physical-entry uop resets both slot-written bits before reuse.
   */
  for (i <- 0 until RenameWidth) {
    // we don't check whether io.redirect is valid here since redirect has higher priority
    val hasEarlierSameHalf =
      if (i == 0) {
        false.B
      } else {
        VecInit((0 until i).map { j =>
          io.enq.canAccept &&
          io.enq.req(j).valid &&
          io.enq.req(j).bits.robIdx.isSameSlot(io.enq.req(i).bits.robIdx)
        }).asUInt.orR
      }
    val hasEarlierEntryReset =
      if (i == 0) {
        false.B
      } else {
        VecInit((0 until i).map { j =>
          io.enq.canAccept &&
          io.enq.req(j).valid &&
          io.enq.req(j).bits.firstUop &&
          io.enq.req(j).bits.robIdx.isSameEntry(io.enq.req(i).bits.robIdx)
        }).asUInt.orR
      }
    when(io.enq.canAccept && io.enq.req(i).valid) {
      val enqUop = io.enq.req(i).bits
      val enqIndex = io.enq.req(i).bits.robIdx.value
      val subIndex = !io.enq.req(i).bits.robIdx.isFormer
      val slotWritten = Mux(enqUop.firstUop || hasEarlierEntryReset, false.B, enqHalfWritten(enqIndex)(subIndex))
      when(enqUop.firstUop && !hasEarlierEntryReset) {
        enqHalfWritten(enqIndex)(0) := false.B
        enqHalfWritten(enqIndex)(1) := false.B
      }
      when(!hasEarlierSameHalf && !slotWritten) {
        // Keep the first instruction metadata in each rob-half for difftest/debug.
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
        when(enqUop.robIdx.isFormer) {
          robEntries(enqIndex).perfDebugInfo.foreach(_.dispatchTime := timer)
          robEntries(enqIndex).perfDebugInfo.foreach(_.enqRsTime := timer)
          robEntries(enqIndex).perfDebugInfo.foreach(_.selectTime := timer)
          robEntries(enqIndex).perfDebugInfo.foreach(_.issueTime := timer)
          robEntries(enqIndex).perfDebugInfo.foreach(_.writebackTime := timer)
          robEntries(enqIndex).perfDebugInfo.foreach(_.tlbFirstReqTime := timer)
          robEntries(enqIndex).perfDebugInfo.foreach(_.tlbRespTime := timer)
        }
        debug_lqIdxValid(enqIndex)(subIndex) := false.B
        enqHalfWritten(enqIndex)(subIndex) := true.B
      }
      when(enqUop.firstUop && !hasEarlierEntryReset) {
        debug_lsInfo(enqIndex) := DebugLsInfo.init
        debug_lsTopdownInfo(enqIndex) := LsTopdownInfo.init
        debug_lsIssued(enqIndex) := false.B
      }
      when(enqUop.waitForward) {
        hasWaitForward := true.B
      }
    }
  }

  for (i <- 0 until RenameWidth) {
    val enqUop = io.enq.req(i)
    when(enqUop.valid && enqUop.bits.blockBackward && io.enq.canAccept) {
      hasBlockBackward := true.B
    }
  }

  /**
   * Capacity reservations count physical entry heads; isEmpty is a delayed frontend-facing indication.
   */
  val dispatchNum = Mux(io.enq.canAccept, PopCount(io.enq.req.map(req => req.valid && req.bits.firstUop)), 0.U)
  io.enq.isEmpty := RegNext(isEmpty && !VecInit(io.enq.req.map(_.valid)).asUInt.orR)

  // sel vsetvl's flush position
  /**
   * [ROB-21]
   * Legacy vset flush-position search. It records the first later vector uop,
   * but needModifyFtqIdxOffset below is hardwired false, so these candidates do
   * not currently choose the actual redirect location. Preserve that distinction
   * when reading the state names or planning dead-code cleanup.
   */
  val vs_idle :: vs_waitVinstr :: vs_waitFlush :: Nil = Enum(3)
  val vsetvlState = RegInit(vs_idle)

  val firstVInstrFtqPtr = RegInit(0.U.asTypeOf(new FtqPtr))
  val firstVInstrFtqOffset = RegInit(0.U.asTypeOf(UInt(FetchBlockInstOffsetWidth.W)))
  val firstVInstrRobIdx = RegInit(RobPtr(false.B, 0.U))

  val enq0 = io.enq.req(0)
  val enq0IsVset = enq0.bits.isVset && enq0.bits.lastUop && canEnqueue(0)
  val enq0IsVsetFlush = enq0IsVset && enq0.bits.flushPipe
  val enqIsVInstrVec = io.enq.req.zip(canEnqueue).map { case (req, fire) => FuType.isVArith(req.bits.fuType) && fire }
  // for vs_idle
  val firstVInstrIdle = PriorityMux(enqIsVInstrVec.zip(io.enq.req).drop(1) :+ (true.B, 0.U.asTypeOf(io.enq.req(0).cloneType)))
  // for vs_waitVinstr
  val enqIsVInstrOrVset = (enqIsVInstrVec(0) || enq0IsVset) +: enqIsVInstrVec.drop(1)
  val firstVInstrWait = PriorityMux(enqIsVInstrOrVset, io.enq.req)
  when(vsetvlState === vs_idle) {
    firstVInstrFtqPtr := firstVInstrIdle.bits.ftqPtr
    firstVInstrFtqOffset := firstVInstrIdle.bits.ftqOffset
    firstVInstrRobIdx := firstVInstrIdle.bits.robIdx
  }.elsewhen(vsetvlState === vs_waitVinstr) {
    when(Cat(enqIsVInstrOrVset).orR) {
      firstVInstrFtqPtr := firstVInstrWait.bits.ftqPtr
      firstVInstrFtqOffset := firstVInstrWait.bits.ftqOffset
      firstVInstrRobIdx := firstVInstrWait.bits.robIdx
    }
  }

  val hasVInstrAfterI = Cat(enqIsVInstrVec(0)).orR
  when(vsetvlState === vs_idle && !io.redirect.valid) {
    when(enq0IsVsetFlush) {
      vsetvlState := Mux(hasVInstrAfterI, vs_waitFlush, vs_waitVinstr)
    }
  }.elsewhen(vsetvlState === vs_waitVinstr) {
    when(io.redirect.valid) {
      vsetvlState := vs_idle
    }.elsewhen(Cat(enqIsVInstrOrVset).orR) {
      vsetvlState := vs_waitFlush
    }
  }.elsewhen(vsetvlState === vs_waitFlush) {
    when(io.redirect.valid) {
      vsetvlState := vs_idle
    }
  }

  // lqEnq
  /**
   * [ROB-22]
   * Record per-slot load queue allocation before execution; only the former is mirrored into entry debug fields.
   */
  io.debugEnqLsq.needAlloc.map(_(0)).zip(io.debugEnqLsq.req).foreach { case (alloc, req) =>
    when(io.debugEnqLsq.canAccept && alloc && req.valid) {
      val ptr = req.bits.uop.robIdx
      debug_lqIdx(ptr.value)(slotIndex(ptr)) := req.bits.uop.lqIdx
      debug_lqIdxValid(ptr.value)(slotIndex(ptr)) := true.B
      when(ptr.isFormer) {
        robEntries(ptr.value).debug_lqIdx.foreach(_ := req.bits.uop.lqIdx)
      }
    }
  }

  // lsIssue
  when(io.debugHeadLsIssue) {
    debug_lsIssued(deqPtr.value) := true.B
  }

  /**
   * Writeback (from execution units)
   */
  /**
   * [ROB-23]
   * Collect execution payload and timing by exact slot. These writes intentionally
   * use the observation stream; architectural completion counters are updated
   * later from the redirect-filtered io.writeback stream.
   */
  for (wb <- exuWBs) {
    val wbIdx = wb.bits.robIdx.value
    val subIdx = slotIndex(wb.bits.robIdx)
    val debug_Uop = debug_microOp(wbIdx)(subIdx)
    val debug = debugMeta(debug_Uop)
    when(wb.valid) {
      debug_exuData(wbIdx)(subIdx) := wb.bits.data
      debug_exuDebug(wbIdx)(subIdx) := wb.bits.debug
      wb.bits.perfDebugInfo.foreach { x =>
        debug_microOp(wbIdx)(subIdx).debug.foreach { slotDebug =>
          slotDebug.perfDebugInfo.enqRsTime := x.enqRsTime
          slotDebug.perfDebugInfo.selectTime := x.selectTime
          slotDebug.perfDebugInfo.issueTime := x.issueTime
          slotDebug.perfDebugInfo.writebackTime := x.writebackTime
          slotDebug.perfDebugInfo.tlbFirstReqTime := x.tlbFirstReqTime
          slotDebug.perfDebugInfo.tlbRespTime := x.tlbRespTime
        }
        when(wb.bits.robIdx.isFormer) {
          robEntries(wbIdx).perfDebugInfo.foreach(_.enqRsTime := x.enqRsTime)
          robEntries(wbIdx).perfDebugInfo.foreach(_.selectTime := x.selectTime)
          robEntries(wbIdx).perfDebugInfo.foreach(_.issueTime := x.issueTime)
          robEntries(wbIdx).perfDebugInfo.foreach(_.writebackTime := x.writebackTime)
          robEntries(wbIdx).perfDebugInfo.foreach(_.tlbFirstReqTime := x.tlbFirstReqTime)
          robEntries(wbIdx).perfDebugInfo.foreach(_.tlbRespTime := x.tlbRespTime)
        }
      }
      debug_lqIdx(wbIdx)(subIdx) := wb.bits.lqIdx.getOrElse(0.U.asTypeOf(new LqPtr))
      debug_sqIdx(wbIdx)(subIdx) := wb.bits.sqIdx.getOrElse(0.U.asTypeOf(new SqPtr))
      when(wb.bits.robIdx.isFormer) {
        robEntries(wbIdx).debug_lqIdx.foreach(_ := wb.bits.lqIdx.getOrElse(0.U.asTypeOf(new LqPtr)))
        robEntries(wbIdx).debug_sqIdx.foreach(_ := wb.bits.sqIdx.getOrElse(0.U.asTypeOf(new SqPtr)))
      }
    }
    XSInfo(wb.valid,
      p"writebacked pc 0x${Hexadecimal(debug.pc)} wen ${debug_Uop.rfWen} " +
        p"data 0x${Hexadecimal(wb.bits.data)} ldst ${debug_Uop.ldest} pdst ${debug_Uop.pdest} " +
        p"skip ${wb.bits.debug.isSkipDiff} robIdx: ${wb.bits.robIdx}\n"
    )
  }

  // The entry-level trace block describes the youngest instruction in the
  // surviving compressed entry, while debug/difftest keep both slots exact.
  /**
   * Keep branch outcome in both exact slot history and the separately saved former trace.
   */
  for (wb <- branchWBs) {
    val wbIdx = wb.bits.robIdx.value
    val subIdx = slotIndex(wb.bits.robIdx)
    when(wb.valid && wb.bits.redirect.get.bits.taken &&
      Itype.isNonTaken(debug_microOp(wbIdx)(subIdx).traceBlockInPipe.itype)) {
      debug_microOp(wbIdx)(subIdx).traceBlockInPipe.itype := Itype.Taken
    }
    when(wb.valid && wb.bits.robIdx.isFormer && wb.bits.redirect.get.bits.taken &&
      Itype.isNonTaken(robEntries(wbIdx).formerTraceBlockInPipe.itype)) {
      robEntries(wbIdx).formerTraceBlockInPipe.itype := Itype.Taken
    }
  }

  val writebackNum = PopCount(exuWBs.map(_.valid))
  XSInfo(writebackNum =/= 0.U, "writebacked %d insts\n", writebackNum)


  /**
   * RedirectOut: Interrupt and Exceptions
   */
  val debug_deqUop = debug_microOp(deqPtr.value).head

  /**
   * [ROB-24]
   * Head exception selection first chooses the oldest slot marked need-flush,
   * then waits for ExceptionGen state to identify that exact slot. Interrupts
   * select the former boundary. commit_w means completion is resolved; it does
   * not by itself permit retirement when exception metadata is still pending.
   */
  val deqPtrEntry = rawInfo(0)
  val deqPtrEntryValid = deqPtrEntry.commit_v
  val deqHasFlushed = RegInit(false.B)
  val intrBitSetReg = RegNext(io.csr.intrBitSet)
  val intrEnable = intrBitSetReg && !hasWaitForward && deqPtrEntry.interruptSafe && !deqHasFlushed
  val deqPtrFormer = Wire(new RobPtr)
  val deqPtrLatter = Wire(new RobPtr)
  deqPtrFormer := deqPtr
  deqPtrLatter := deqPtr
  deqPtrFormer.isFormer := true.B
  deqPtrLatter.isFormer := false.B
  val deqExceptionIsFormer = intrEnable || deqPtrEntry.slotNeedFlushMask(0)
  val deqExceptionUop = Mux(deqExceptionIsFormer, debug_microOp(deqPtr.value)(0), debug_microOp(deqPtr.value)(1))
  // Select the exact slot (former/latter) that should provide exception state for the head entry.
  val deqExceptionRobIdx = Mux(deqPtrEntry.slotNeedFlushMask(0), deqPtrFormer, deqPtrLatter)
  val formerNotFlush = !deqPtrFormer.needFlush(io.redirect)
  val latterNotFlush = !deqPtrLatter.needFlush(io.redirect)
  val deqNeedFlush = (deqPtrEntry.slotNeedFlushMask(0) && formerNotFlush || deqPtrEntry.slotNeedFlushMask(1) && CompressType.isNotNORMAL(deqPtrEntry.entryPairType) && latterNotFlush) && deqPtrEntry.commit_v && deqPtrEntry.commit_w
  val deqHitExceptionGenState = exceptionDataRead.valid && exceptionDataRead.bits.robIdx.isSameSlot(deqExceptionRobIdx)
  val deqNeedFlushAndHitExceptionGenState = deqNeedFlush && deqHitExceptionGenState
  val exceptionGenStateIsException = exceptionDataRead.bits.exceptionVec.orR || exceptionDataRead.bits.singleStep || TriggerAction.isDmode(exceptionDataRead.bits.trigger)
  /**
   * The two delayed commit_w samples are an explicit metadata-settling guard in the current implementation.
   */
  val deqHasException = deqNeedFlushAndHitExceptionGenState && exceptionGenStateIsException && RegNext(RegNext(deqPtrEntry.commit_w))
  val deqHasFlushPipe = deqNeedFlushAndHitExceptionGenState && exceptionDataRead.bits.flushPipe && !deqHasException && RegNext(RegNext(deqPtrEntry.commit_w))
  val deqHasReplayInst = deqNeedFlushAndHitExceptionGenState && exceptionDataRead.bits.replayInst
  val deqIsVlsException = deqHasException && isVectorMemory(deqExceptionUop) && !exceptionDataRead.bits.isEnqExcp
  // delay 2 cycle wait exceptionGen out
  // vls exception can be committed only when RAB commit all its reg pairs
  deqVlsCanCommit := RegNext(RegNext(deqIsVlsException && deqPtrEntry.commit_w)) && rab.io.status.commitEnd

  // lock at assertion of deqVlsExceptionNeedCommit until condition not assert
  /**
   * [ROB-25]
   * Vector LS exceptions may need partial destination-map recovery. Wait for
   * RAB's earlier commit work, issue a single quota, and lock until the head
   * pointer advances. This path is separate from ordinary physical retirement.
   */
  val deqVlsExcpLock = RegInit(false.B)
  val handleVlsExcp = deqIsVlsException && deqVlsCanCommit && !deqVlsExcpLock && state === s_idle
  when(handleVlsExcp) {
    deqVlsExcpLock := true.B
  }.elsewhen(deqPtrVec.head =/= deqPtrVec_next.head) {
    deqVlsExcpLock := false.B
  }

  // Only assert once when deqVlsExcp occurs until condition not assert to avoid multi message passed to RAB
  when (deqVlsExceptionNeedCommit) {
    deqVlsExceptionNeedCommit := false.B
  }.elsewhen(handleVlsExcp){
    deqVlsExceptionCommitSize := deqPtrEntry.realDestSize
    deqVlsExceptionNeedCommit := true.B
  }

  val hasCommit = io.commits.isCommit && io.commits.commitValid.asUInt.orR
  /**
   * [ROB-26]
   * Diff RAT snapshots are slot-addressed: a compressed retirement selects its
   * latter endpoint, while NORMAL (including a downgraded former) selects former.
   * The vector exception quota uses the former snapshot without ordinary commit.
   */
  val retiredSlotPtrs = io.commits.robIdx.zip(io.commits.info).map { case (ptr, info) =>
    val retired = WireDefault(ptr)
    retired.isFormer := CompressType.isNORMAL(info.entryPairType)
    retired
  }
  val newestCommit = PriorityMuxDefault(
    io.commits.commitValid.zip(retiredSlotPtrs).reverse,
    deqPtr
  )
  io.diffRatCommitRobIdx.foreach { commitRobIdx =>
    commitRobIdx.valid := hasCommit || deqVlsExceptionNeedCommit
    commitRobIdx.bits := Mux(deqVlsExceptionNeedCommit, deqPtr.asFormer, newestCommit)
  }
  io.diffRatCommitRobIdxVec.foreach { commitRobIdxVec =>
    assert(!(deqVlsExceptionNeedCommit && hasCommit), "VLS and normal commits overlap")
    for (i <- 0 until CommitWidth) {
      commitRobIdxVec(i).valid := io.commits.isCommit && io.commits.commitValid(i) &&
        !deqVlsExceptionNeedCommit
      commitRobIdxVec(i).bits := retiredSlotPtrs(i)
    }
    when(deqVlsExceptionNeedCommit) {
      commitRobIdxVec.head.valid := true.B
      commitRobIdxVec.head.bits := deqPtr.asFormer
    }
  }

  XSDebug(deqHasException && exceptionDataRead.bits.singleStep, "Debug Mode: Deq has singlestep exception\n")
  XSDebug(deqHasException && TriggerAction.isDmode(exceptionDataRead.bits.trigger), "Debug Mode: Deq has trigger entry debug Mode\n")

  /**
   * [ROB-27]
   * Plain flushPipe retires the causing instruction after recovery; replay and
   * traps include the causing slot in the squash. deqHasFlushed prevents a
   * flushAfter head from repeatedly requesting the same recovery.
   */
  val isFlushPipe = deqPtrEntry.commit_w && (deqHasFlushPipe || deqHasReplayInst)

  // vsetvl instruction need another one cycle to write to vtype gen
  val isVsetFlushPipe = deqPtrEntry.commit_w && deqHasFlushed && exceptionDataRead.bits.isVset
  val isVsetFlushPipeReg = RegNext(isVsetFlushPipe)
  //  val needModifyFtqIdxOffset = isVsetFlushPipe && (vsetvlState === vs_waitFlush)
  /**
   * The recorded vset search candidate is currently disabled as a redirect source.
   */
  val needModifyFtqIdxOffset = false.B
  io.isVsetFlushPipe := isVsetFlushPipe
  io.toDecode.isResumeVType := vtypeBuffer.io.toDecode.isResumeVType || isVsetFlushPipeReg
  // io.flushOut will trigger redirect at the next cycle.
  // Block any redirect or commit at the next cycle.
  val lastCycleFlush = RegNext(io.flushOut.valid)

  /**
   * [ROB-28]
   * Request recovery only from idle with a valid head, and suppress a second
   * request during its feedback cycle. This valid is not gated by blockCommit.
   * PC reconstruction uses the former FTQ base plus formerLen for an untaken
   * latter path; a predicted-taken path uses the next FTQ block instead.
   */
  io.flushOut.valid := (state === s_idle) && deqPtrEntryValid && (intrEnable || deqHasException && (!deqIsVlsException || deqVlsCanCommit) || isFlushPipe) && !lastCycleFlush
  io.flushOut.bits := DontCare
  val flushIsFormer = intrEnable || deqPtrEntry.slotNeedFlushMask(0)
  val flushBaseIsRVC = Mux(flushIsFormer, deqPtrEntry.slotHeadRvcMask(0), Mux(deqPtrEntry.predTaken, deqPtrEntry.slotHeadRvcMask(1), deqPtrEntry.slotHeadRvcMask(0)))
  io.flushOut.bits.isRVC := flushBaseIsRVC
  io.flushOut.bits.robIdx := Mux(needModifyFtqIdxOffset, firstVInstrRobIdx, deqPtr)
  io.flushOut.bits.robIdx.isFormer := Mux(needModifyFtqIdxOffset, true.B, flushIsFormer)
  io.flushOut.bits.ftqIdx :=
    Mux(needModifyFtqIdxOffset,
      firstVInstrFtqPtr,
      Mux(flushIsFormer,
        deqPtrEntry.ftqIdx,
        Mux(deqPtrEntry.predTaken,
          deqPtrEntry.ftqIdx + 1.U,
          deqPtrEntry.ftqIdx
        )
      )
    )
  io.flushOut.bits.ftqOffset :=
    Mux(needModifyFtqIdxOffset,
      firstVInstrFtqOffset,
      Mux(flushIsFormer,
        deqPtrEntry.ftqOffset,
        Mux(deqPtrEntry.predTaken,
          Mux(deqPtrEntry.slotHeadRvcMask(1),
            0.U.asTypeOf(deqPtrEntry.ftqOffset),
            1.U.asTypeOf(deqPtrEntry.ftqOffset)
          ),
          deqPtrEntry.ftqOffset
        )
      )
    )
  io.flushPcInfo.formerLen := Mux(
    needModifyFtqIdxOffset || flushIsFormer || deqPtrEntry.predTaken,
    0.U.asTypeOf(io.flushPcInfo.formerLen),
    formerLenCommit(0)
  )
  io.flushPcInfo.flushIsRVC := Mux(
    needModifyFtqIdxOffset,
    io.flushOut.bits.isRVC,
    Mux(flushIsFormer, deqPtrEntry.slotHeadRvcMask(0), deqPtrEntry.slotHeadRvcMask(1))
  )
  // TODO: it could use the former instr's ftqIdx and ftqOffset to calculate the latter instr's PC
  // rather than calculate the latter instr's ftqIdx and ftqOffset which could be wrong!!!

  io.flushOut.bits.level := Mux(deqHasReplayInst || intrEnable || deqHasException || needModifyFtqIdxOffset, RedirectLevel.flush, RedirectLevel.flushAfter) // TODO use this to implement "exception next"
  io.flushOut.bits.interrupt := !isFlushPipe
  io.flushOut.bits.satpFlush := isFlushPipe && exceptionDataRead.bits.satpFlush
  XSPerfAccumulate("flush_num", io.flushOut.valid)
  XSPerfAccumulate("interrupt_num", io.flushOut.valid && intrEnable)
  XSPerfAccumulate("exception_num", io.flushOut.valid && deqHasException)
  XSPerfAccumulate("flush_pipe_num", io.flushOut.valid && isFlushPipe)
  XSPerfAccumulate("replay_inst_num", io.flushOut.valid && isFlushPipe && deqHasReplayInst)

  /**
   * [ROB-29]
   * Trap event is a subset of flushOut reasons. Register its exact-slot payload
   * for CSR and start the synchronous GPA lookup in the same cycle. Plain
   * flushPipe and replay request recovery without asserting io.exception.valid.
   */
  val exceptionHappen = (state === s_idle) && deqPtrEntryValid && (intrEnable || deqHasException && (!deqIsVlsException || deqVlsCanCommit)) && !lastCycleFlush
  val exceptionIsFormer = deqExceptionIsFormer
  val exceptionUop = deqExceptionUop
  val exceptionDebug = debugMeta(exceptionUop)
  io.exception.valid := RegNext(exceptionHappen)
  io.exception.bits.pc := RegEnable(exceptionDebug.pc, exceptionHappen)
  io.exception.bits.gpaddr := io.readGPAMemData.gpaddr
  io.exception.bits.isForVSnonLeafPTE := io.readGPAMemData.isForVSnonLeafPTE
  io.exception.bits.instr := RegEnable(exceptionDebug.instr, exceptionHappen)
  io.exception.bits.commitType := RegEnable(exceptionUop.commitType, exceptionHappen)
  io.exception.bits.isFormer := RegEnable(exceptionIsFormer, exceptionHappen)
  io.exception.bits.isStore := RegEnable(FuType.isStore(exceptionUop.fuType), exceptionHappen)
  io.exception.bits.exceptionVec extendFrom RegEnable(exceptionDataRead.bits.exceptionVec, exceptionHappen)
  io.exception.bits.satpFlushFirstFetchFault := RegEnable(exceptionDataRead.bits.satpFlushFirstFetchFault && deqHasException, exceptionHappen)
  // fetch trigger fire or execute ebreak
  io.exception.bits.isPcBkpt := RegEnable(
    exceptionDataRead.bits.exceptionVec(ExceptionNO.EX_BP) && (
      exceptionDataRead.bits.isEnqExcp ||
      exceptionDataRead.bits.trigger === TriggerAction.None
    ),
    exceptionHappen,
  )
  io.exception.bits.isFetchMalAddr := RegEnable(exceptionDataRead.bits.isFetchMalAddr && deqHasException, exceptionHappen)
  io.exception.bits.singleStep := RegEnable(exceptionDataRead.bits.singleStep, exceptionHappen)
  io.exception.bits.crossPageIPFFix := RegEnable(exceptionDataRead.bits.crossPageIPFFix, exceptionHappen)
  io.exception.bits.isInterrupt := RegEnable(intrEnable, exceptionHappen)
  io.exception.bits.isHls := RegEnable(exceptionUop.isHls, exceptionHappen)
  io.exception.bits.vls := RegEnable(isVectorMemory(exceptionUop), exceptionHappen)
  io.exception.bits.trigger := RegEnable(exceptionDataRead.bits.trigger, exceptionHappen)

  // A latter exception flushes itself but preserves its older former slot.
  // Hold only the difftest observation until that slot really retires after walk.
  /**
   * [ROB-30]
   * A latter trap leaves an older former slot to retire after walk. CSR handles
   * the hardware trap immediately; only difftest observation waits. The pending
   * bit also serializes the eventual former retirement to align the two events.
   */
  val diffTrapPending = WireDefault(false.B)
  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    val pending = RegInit(false.B)
    val pendingPtr = Reg(new RobPtr)
    val release = pending && io.commits.isCommit && io.commits.commitValid.head &&
      deqPtr.isSameEntry(pendingPtr)
    when(exceptionHappen && !exceptionIsFormer) {
      assert(!pending, "A latter exception observation is already pending")
      assert(!isVectorMemory(exceptionUop), "Vector memory instructions must not be compressed")
      pending := true.B
      pendingPtr := deqPtr
    }.elsewhen(release) {
      pending := false.B
    }
    when(release) {
      assert(CompressType.isNORMAL(io.commits.info.head.entryPairType))
      assert(PopCount(io.commits.commitValid) === 1.U)
      assert(io.diffArchEvent.get.valid && io.diffArchEvent.get.bits,
        "The surviving former and its latter exception must be observed together")
    }
    diffTrapPending := pending
    io.diffCommitForTrap.get := release
  }

  // data will be one cycle after valid
  io.readGPAMemAddr.valid := exceptionHappen
  io.readGPAMemAddr.bits.ftqPtr := exceptionDataRead.bits.ftqPtr
  io.readGPAMemAddr.bits.ftqOffset := exceptionDataRead.bits.ftqOffset
  io.readGPAMemAddr.bits.isRVC := exceptionDataRead.bits.isRVC

  XSDebug(io.flushOut.valid,
    p"generate redirect: pc 0x${Hexadecimal(io.exception.bits.pc)} intr $intrEnable " +
      p"excp $deqHasException flushPipe $isFlushPipe " +
      p"Trap_target 0x${Hexadecimal(io.csr.trapTarget.pc)} exceptionVec ${Binary(exceptionDataRead.bits.exceptionVec.asUInt)}\n")


  /**
   * Commits (and walk)
   * They share the same width.
   */
  // T redirect.valid, T+1 use walkPtrVec read robEntries, T+2 start walk, shouldWalkVec used in T+2
  /**
   * [ROB-31]
   * Recovery reader pipeline, with T denoting the cycle redirect is asserted:
   * T installs the starting pointers; T+1 captures bank data and masks the
   * already-consumed prefix; T+2 emits the first walkValid group. A new redirect
   * restarts this pipeline and suppresses both setup cycles again.
   */
  val shouldWalkVec = recovery.valid
  val walkingPtrVec = recovery.scannedPointers
  val walkFinished = recovery.scanDone
  rab.io.fromRob.walkEnd := state === s_walk && walkFinished
  vtypeBuffer.io.fromRob.walkEnd := state === s_walk && walkFinished

  require(RenameWidth <= CommitWidth)

  // wiring to csr
  // update when enq
  /**
   * [ROB-32]
   * Sticky CSR dirty bits track only the oldest in-flight setter per flag.
   * Tags remain slot-aware so a squashed latter setter cannot dirty a surviving
   * former. Actual commit reports are physical-entry based; the tracker resolves
   * redirect cancellation before producing the entry's set mask.
   */
  val enqFlagTracker = Module(new RobSetFlagTracker(
    2,
    io.enq.req.size,
    CommitWidth,
    orderedUpdatePorts = true
  ))
  enqFlagTracker.io.update.zip(io.enq.req).zip(uopCanEnqueue).foreach {
    case ((update, req), canEnqueue) =>
      update.valid := canEnqueue
      update.bits.robIdx := req.bits.robIdx
      update.bits.setMask := Cat(req.bits.dirtyVs, req.bits.dirtyFs || req.bits.fflagsWen)
  }
  enqFlagTracker.io.redirect := io.redirect
  enqFlagTracker.io.commit.zip(io.commits.commitValid).zip(io.commits.robIdx).foreach {
    case ((commit, valid), robIdx) =>
      commit.valid := io.commits.isCommit && valid
      commit.bits := robIdx
  }
  val dirty_fs = enqFlagTracker.io.commitSetMask(0)
  val dirty_vs = enqFlagTracker.io.commitSetMask(1)

  val deqPtrCmp = Wire(new RobPtr)
  val enqPtrCmp = Wire(new RobPtr)
  deqPtrCmp := deqPtr
  enqPtrCmp := enqPtr
  deqPtrCmp.isFormer := true.B
  enqPtrCmp.isFormer := true.B
  /**
   * Reject a removed latter slot; the independent ROB-window check also excludes stale generations.
   */
  private def wbSlotStillValid(robIdx: RobPtr): Bool = {
    val wbEntry = robEntries(robIdx.value) // TODO: this may cause timing issue
    wbEntry.valid && (robIdx.isFormer || CompressType.isNotNORMAL(wbEntry.entryPairType))
  }

  // update when writeback
  val fflagsWidth = 5

  /**
   * [ROB-33]
   * Candidate port lists are Scala-time structure. Each runtime update carries
   * fflags[4:0] plus vxsat, and is admitted only inside the live ROB slot window.
   * The tracker cancels speculative setters and releases flags at retirement.
   */
  private val writebackFflagsCandidates = (0 until io.writeback.size).filter { index =>
    io.writeback(index).bits.fflags.nonEmpty
  }
  private val writebackVxsatCandidates = (0 until io.writeback.size).filter { index =>
    io.writeback(index).bits.vxsat.nonEmpty
  }
  private val writebackFlagCandidates =
    Seq.fill(fflagsWidth)(writebackFflagsCandidates) :+ writebackVxsatCandidates
  val writebackFlagTracker = Module(new RobSetFlagTracker(
    fflagsWidth + 1,
    io.writeback.size,
    CommitWidth,
    writebackFlagCandidates
  ))
  writebackFlagTracker.io.update.zip(io.writeback).foreach { case (update, wb) =>
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
  writebackFlagTracker.io.redirect := io.redirect
  writebackFlagTracker.io.commit.zip(io.commits.commitValid).zip(io.commits.robIdx).foreach {
    case ((commit, valid), robIdx) =>
      commit.valid := io.commits.isCommit && valid
      commit.bits := robIdx
  }

  /**
   * [ROB-34]
   * Retired vector work can clear architectural vstart; a real exception has
   * priority and can install its reported restart index instead. Vector recovery
   * format is registered alongside the exception and sent with the RAB maps.
   */
  val resetVstart = dirty_vs && !io.vstartIsZero

  vecExcpInfo.valid := exceptionHappen && !intrEnable && exceptionDataRead.bits.vstartEn && exceptionDataRead.bits.isVecLoad && !exceptionDataRead.bits.isEnqExcp
  when (exceptionHappen) {
    vecExcpInfo.bits.nf := exceptionDataRead.bits.nf
    vecExcpInfo.bits.vsew := exceptionDataRead.bits.vsew
    vecExcpInfo.bits.veew := exceptionDataRead.bits.veew
    vecExcpInfo.bits.vlmul := exceptionDataRead.bits.vlmul
    vecExcpInfo.bits.isStride := exceptionDataRead.bits.isStrided
    vecExcpInfo.bits.isIndexed := exceptionDataRead.bits.isIndexed
    vecExcpInfo.bits.isWhole := exceptionDataRead.bits.isWhole
    vecExcpInfo.bits.isVlm := exceptionDataRead.bits.isVlm
    vecExcpInfo.bits.vstart := exceptionDataRead.bits.vstart
  }

  io.csr.vstart.valid := RegNext(Mux(exceptionHappen && deqHasException, exceptionDataRead.bits.vstartEn, resetVstart))
  io.csr.vstart.bits := RegNext(Mux(exceptionHappen && deqHasException, exceptionDataRead.bits.vstart, 0.U))

  // when mispredict branches writeback, stop commit in the next 2 cycles
  // TODO: don't check all exu write back
  /**
   * [ROB-35]
   * Commit protection windows let delayed redirects/exception metadata catch up
   * with completion. Loading 111 into a three-bit shift register blocks the next
   * three cycles; the historical comment above says two, so use this logic when
   * deriving timing. Existing uninitialized Reg declarations are unchanged.
   */
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
  val deqHasCommitted = io.commits.isCommit && io.commits.commitValid(0)
  // TODO *** WARNING ***
  // Blocking commit. Don't change this before we fully understand the logic.
  val deqHitRedirectReg = RegNext(io.redirect.valid && io.redirect.bits.robIdx.isSameEntry(deqPtr)) ||
    RegNext(RegNext(io.redirect.valid && io.redirect.bits.robIdx.isSameEntry(deqPtr)))
  val criticalErrorState = io.csr.criticalErrorState
  when(deqNeedFlush && deqHitRedirectReg){
    deqFlushBlockCounter := "b111".U
  }.otherwise{
    deqFlushBlockCounter := deqFlushBlockCounter >> 1.U
  }
  when(deqHasCommitted){
    deqHasFlushed := false.B
  }.elsewhen(deqNeedFlush && io.flushOut.valid && !io.flushOut.bits.flushItself()){
    deqHasFlushed := true.B
  }
  val traceBlock = io.trace.blockCommit
  /**
   * Global gate is separate from candidate masks: consumers must qualify commitValid with isCommit.
   */
  val blockCommit = misPredBlock || lastCycleFlush || hasWFI || io.redirect.valid ||
    (deqNeedFlush && !deqHasFlushed) || deqFlushBlock || criticalErrorState || traceBlock

  io.commits.isWalk := state === s_walk
  io.commits.isCommit := state === s_idle && !blockCommit

  /**
   * [ROB-36]
   * Retirement candidates are formed in physical bank order, excluding entries
   * already retired from this row and any younger entries behind a blocker.
   * Shift the candidate mask to head-relative output lanes only at the interface.
   * The pointer wrapper must consume the same predicates and row bookkeeping.
   */
  val walk_v = VecInit(walkingPtrVec.map(ptr => robEntries(ptr.value).valid))
  val commit_vDeqGroup = VecInit(robDeqGroup.map(_.commit_v))
  val commit_wDeqGroup = VecInit(robDeqGroup.map(_.commit_w))
  val realCommitLast = deqPtrVec(0).lineHeadPtr.addEntries(Fill(bankAddrWidth, 1.U)).asFormer
  private val commitPolicy = new RobCommitPolicy(
    robDeqGroup, hasCommitted, intrBitSetReg, intrEnable,
    deqNeedFlush, deqHasFlushed, diffTrapPending
  )
  val allowOnlyOneCommit = commitPolicy.allowOnlyOne
  commitValidThisLine := commitPolicy.rowCandidates
  io.commits.commitValid := commitPolicy.headCandidates

  for (i <- 0 until CommitWidth) {
    // defaults: state === s_idle and instructions commit
    // when intrBitSetReg, allow only one instruction to commit at each clock cycle
    io.commits.info(i) := commitInfo(i)
    io.commits.robIdx(i) := deqPtrVec(i)
    val deqDebugInst = debug_microOp(deqPtrVec(i).value).head
    deqDebugInst.debug.foreach { debug =>
      PerfCCT.commitInstMeta(i.U, debug.debug_seqNum.seqNum, commitInstrCntByEntry(i), io.commits.isCommit && io.commits.commitValid(i), clock, reset)
    }

    io.commits.walkValid(i) := shouldWalkVec(i)
    XSError(
      state === s_walk &&
      io.commits.isWalk && state === s_walk && shouldWalkVec(i) &&
      !walk_v(i),
      s"The walking entry($i) should be valid\n")

    XSInfo(io.commits.isCommit && io.commits.commitValid(i),
      "retired pc %x wen %d ldest %d pdest %x data %x\n",
      debugMeta(debug_microOp(deqPtrVec(i).value).head).pc,
      io.commits.info(i).rfWen,
      io.commits.info(i).basicDebug.map(_.ldest).getOrElse(0.U),
      io.commits.info(i).basicDebug.map(_.pdest).getOrElse(0.U),
      debug_exuData(deqPtrVec(i).value)(0)
    )
    XSInfo(state === s_walk && io.commits.walkValid(i), "walked pc %x wen %d ldst %d data %x\n",
      debugMeta(debug_microOp(walkPtrVec(i).value).head).pc,
      io.commits.info(i).rfWen,
      io.commits.info(i).basicDebug.map(_.ldest).getOrElse(0.U),
      debug_exuData(walkPtrVec(i).value)(0)
    )
  }

  /**
   * [ROB-37]
   * Diagnostic counter only: print the exact head counters after 14999 cycles without a head retirement.
   */
  val noCommitCycleCnt = RegInit(0.U(20.W))
  when(io.commits.isCommit && io.commits.commitValid(0)) {
    noCommitCycleCnt := 0.U
  }.otherwise {
    noCommitCycleCnt := noCommitCycleCnt + 1.U
  }
  // Read the architectural ROB entry explicitly for the debug-only counters.
  // `robDeqGroup` is a `RobCommitEntryBundle`, which intentionally no longer
  // carries the shadow uop state used to derive these values.
  val deqPtrRobEntry: RobEntryBundle = robEntries(deqPtr.value)
  val deqPtrFormerUopNum = RobBundles.decodeFormerUopNum(
    deqPtrRobEntry.entryPairType,
    deqPtrRobEntry.uopState
  )
  val deqPtrLatterUopNum = RobBundles.decodeLatterUopNum(
    deqPtrRobEntry.entryPairType,
    deqPtrRobEntry.uopState
  )
  when(noCommitCycleCnt === 14999.U) {
    printf(p"[CROB-STUCK] deq=${Hexadecimal(deqPtr.value)} valid=${deqPtrEntry.commit_v} " +
      p"realDest=${deqPtrEntry.realDestSize} " +
      p"ctype=${Binary(deqPtrEntry.entryPairType)} slotNeedFlush=${Binary(deqPtrEntry.slotNeedFlushMask)} " +
      p"formerCnt=${formerInstrCntCommit(0)} latterCnt=${latterInstrCntCommit(0)} " +
      p"formerUopNum=${deqPtrFormerUopNum} latterUopNum=${deqPtrLatterUopNum} " +
      p"headPC=${Hexadecimal(debugMeta(debug_microOp(deqPtr.value).head).pc)} " +
      p"tailPC=${Hexadecimal(debugMeta(debug_microOp(deqPtr.value)(1)).pc)}\n")
  }

  // sync fflags/dirty_fs/vxsat to csr
  /**
   * CSR sticky flags are reported one cycle after the matching physical retirement.
   */
  for(i <- 0 until fflagsWidth) {
    io.csr.fflags(i) := RegNext(writebackFlagTracker.io.commitSetMask(i), false.B)
  }
  io.csr.dirty_fs := GatedValidRegNext(dirty_fs)
  io.csr.dirty_vs := GatedValidRegNext(dirty_vs)
  io.csr.vxsat    := RegNext(writebackFlagTracker.io.commitSetMask(fflagsWidth), false.B)

  // commit load/store to lsq
  /**
   * [ROB-38]
   * LSQ load retirement counts both surviving slots, up to 2*CommitWidth.
   * Store notification uses entryHasStore and the special CC partial-completion
   * condition below rather than simply PopCount of STORE commitType entries.
   * All count outputs here are registered once.
   */
  private val lsqControl = new RobLsqControl(
    commits = io.commits,
    deqPtr = deqPtr,
    nextDeqPtr = deqPtrVec_next.head,
    pendingEntry = robEntries(deqPtr.value),
    redirect = io.redirect,
    slotIsLoad = (0 until CommitWidth).map(i => (0 until 2).map(j =>
      debug_microOp(deqPtrVec(i).value)(j).commitType === CommitType.LOAD)),
    entryIsVls = (0 until CommitWidth).map(i => robEntries(deqPtrVec(i).value).vls)
  )
  io.lsq.lcommit := lsqControl.lcommit
  io.lsq.scommit := lsqControl.scommit
  io.lsq.commit := lsqControl.commit
  io.lsq.pendingPtr := lsqControl.pendingPtr
  io.lsq.pendingPtrNext := lsqControl.pendingPtrNext

  /**
   * state changes
   * (1) redirect: switch to s_walk
   * (2) walk: when walking comes to the end, switch to s_idle
   */
  /**
   * [ROB-40]
   * Both redirect setup cycles force walk state. Returning to idle requires the
   * ROB scan to finish and both independently paced RAB/VTB replay queues to drain.
   */
  XSPerfAccumulate("s_idle_to_idle", state === s_idle && state_next === s_idle)
  XSPerfAccumulate("s_idle_to_walk", state === s_idle && state_next === s_walk)
  XSPerfAccumulate("s_walk_to_idle", state === s_walk && state_next === s_idle)
  XSPerfAccumulate("s_walk_to_walk", state === s_walk && state_next === s_walk)

  /**
   * pointers and counters
   */
  /**
   * [ROB-41]
   * Pointer generators own the actual allocation and retirement pointer registers.
   * Capacity uses conservative reserved headroom and does not subtract this
   * cycle's retirements. Recovery rewinds allocation directly to its slot-aware
   * boundary while keeping dequeue at the oldest unretired physical entry.
   */
  val deqPtrGenModule = Module(new NewRobDeqPtrWrapper)
  deqPtrGenModule.io.state := state
  deqPtrGenModule.io.deq_v := commit_vDeqGroup
  deqPtrGenModule.io.deq_w := commit_wDeqGroup
  deqPtrGenModule.io.exception_state := exceptionDataRead
  deqPtrGenModule.io.intrBitSetReg := intrBitSetReg
  deqPtrGenModule.io.hasNoSpecExec := hasWaitForward
  deqPtrGenModule.io.allowOnlyOneCommit := allowOnlyOneCommit
  deqPtrGenModule.io.interruptSafe := robDeqGroup(deqPtr.value(bankAddrWidth-1,0)).interruptSafe
  deqPtrGenModule.io.blockCommit := blockCommit
  deqPtrGenModule.io.hasCommitted := hasCommitted
  deqPtrGenModule.io.allCommitted := allCommitted
  deqPtrVec := deqPtrGenModule.io.out
  deqPtrVec_next := deqPtrGenModule.io.next_out

  val enqPtrGenModule = Module(new RobEnqPtrWrapper)
  enqPtrGenModule.io.redirect := io.redirect
  enqPtrGenModule.io.allowEnqueue := allowEnqueue && rab.io.canEnq && !io.fromVecExcpMod.busy
  enqPtrGenModule.io.hasBlockBackward := hasBlockBackward
  enqPtrGenModule.io.enq := VecInit(io.enq.req.map(req => req.valid && req.bits.firstUop))
  enqPtrVec := enqPtrGenModule.io.out

  // next walkPtrVec:
  // (1) redirect occurs: update according to state
  // (2) walk: move forwards
  /**
   * [ROB-42]
   * Bank reads start at the containing line, while walkPtrTrue keeps the exact
   * snapshot/head lower bound. lastWalkPtr is inclusive; former+flushItself moves
   * it to the preceding physical entry because no slot survives in the cut entry.
   */
  recoverySnapshotPointer := snapshots(io.snpt.snptSelect)(0)
  val deqPtrVecForWalk = recovery.dequeuePointers
  val snapPtrReadBank = recovery.snapshotBank
  val snapPtrVecForWalk = recovery.snapshotPointers
  val walkPtrVec_next = recovery.nextPointers
  val walkPtrTrue_next = recovery.nextExactPointer
  walkPtrHead := walkPtrVec_next.head
  walkDestSizeDeqGroup.zip(walkPtrVec_next).map {
    case (reg, ptrNext) => reg := deqPtrEntry.realDestSize
  }
  val numValidEntries = distanceBetween(enqPtr, deqPtr)
  val commitCnt = PopCount(io.commits.commitValid)

  allowEnqueue := numValidEntries + dispatchNum <= (RobSize - RenameWidth).U
  allowEnqueueForDispatch := numValidEntries + dispatchNum <= (RobSize - 2 * RenameWidth).U

  val redirectWalkDistance = distanceBetween(io.redirect.bits.robIdx, deqPtrVec_next(0))



  /**
   * States
   * We put all the stage bits changes here.
   *
   * All events: (1) enqueue (dispatch); (2) writeback; (3) cancel; (4) dequeue (commit);
   * All states: (1) valid; (2) writebacked; (3) flagBkup
   */

  val deqPtrGroup = Wire(Vec(2 * CommitWidth, new RobPtr))
  deqPtrGroup.zipWithIndex.map { case (deq, i) => deq := deqPtrVec(0).addEntries(i.U).asFormer }
  val commitReadAddr = Mux(state === s_idle, VecInit(deqPtrVec.map(_.value)), VecInit(walkPtrVec.map(_.value)))

  /**
   * [ROB-43]
   * Array validity cancellation is delayed one cycle and uses the open physical
   * index interval (redirectBegin, redirectEnd), with explicit wrap/full handling.
   * Slot-only cuts retain their physical entry; downgrade logic repairs its data.
   */
  val redirectValidReg = RegNext(io.redirect.valid)
  val redirectBegin = Reg(UInt(log2Up(RobSize).W))
  val redirectEnd = Reg(UInt(log2Up(RobSize).W))
  val redirectAll = RegInit(false.B)
  when(io.redirect.valid){
    redirectBegin := Mux(io.redirect.bits.robIdx.isFormer && io.redirect.bits.flushItself(), io.redirect.bits.robIdx.value - 1.U, io.redirect.bits.robIdx.value)
    redirectEnd := enqPtr.value
    // Only former+flushItself removes the redirect entry itself.  A latter-slot
    // redirect must retain the former slot even when the physical range wraps.
    redirectAll := io.redirect.bits.robIdx.isFormer && io.redirect.bits.flushItself() &&
      (io.redirect.bits.robIdx.value === enqPtr.value) && (io.redirect.bits.robIdx.flag ^ enqPtr.flag)
  }

  // update robEntries valid
  /**
   * One-hot entry-head decode is shared by validity and completion updates.
   * Validity assignment priority here is commit, allocation, delayed cancellation.
   * Do not infer that all ROB fields use this same priority: each field has its
   * own connections below, including later overrides of connectEnq defaults.
   */
  val enqOHByEntry = (0 until RobSize).map { i =>
    VecInit(canEnqueue.zip(allocatePtrVec.map(_.value === i.U)).map {
      case (valid, ptrMatch) => valid && ptrMatch
    })
  }
  for (i <- 0 until RobSize) {
    val enqOH = enqOHByEntry(i)
    val commitCond = io.commits.isCommit && io.commits.commitValid.zip(deqPtrVec.map(_.value === i.U)).map(x => x._1 && x._2).reduce(_ || _)
    assert(PopCount(enqOH) < 2.U, s"robEntries$i enqOH is not one hot")
    // TODO: fix the needFlush logic for CROB
    // redirectBegin is still valid, but redirectBegin+1 is invalid
    val needFlush = redirectValidReg && (Mux(
      redirectEnd > redirectBegin,
      (i.U > redirectBegin) && (i.U < redirectEnd),
      (i.U > redirectBegin) || (i.U < redirectEnd)
    ) || redirectAll)
    when(commitCond) {
      robEntries(i).valid := false.B
    }.elsewhen(enqOH.asUInt.orR && !io.redirect.valid) {
      robEntries(i).valid := true.B
    }.elsewhen(needFlush){
      robEntries(i).valid := false.B
    }
  }

  // debug_inst update
  /**
   * Merge memory pipeline history into per-entry observation records.
   */
  for (i <- 0 until (LduCnt + StaCnt)) {
    debug_lsInfo(io.debug_ls.debugLsInfo(i).s1_robIdx).s1SignalEnable(io.debug_ls.debugLsInfo(i))
    debug_lsInfo(io.debug_ls.debugLsInfo(i).s2_robIdx).s2SignalEnable(io.debug_ls.debugLsInfo(i))
    debug_lsInfo(io.debug_ls.debugLsInfo(i).s3_robIdx).s3SignalEnable(io.debug_ls.debugLsInfo(i))
  }
  for (i <- 0 until LduCnt) {
    debug_lsTopdownInfo(io.lsTopdownInfo(i).s1.robIdx).s1SignalEnable(io.lsTopdownInfo(i))
    debug_lsTopdownInfo(io.lsTopdownInfo(i).s2.robIdx).s2SignalEnable(io.lsTopdownInfo(i))
  }

  // status field: writebacked
  // enqueue logic set 6 writebacked to false

  // writeback logic set numWbPorts writebacked to true

  // if the first uop of an instruction is valid , write writebackedCounter
  /**
   * [ROB-44]
   * Predecode admitted uops and completion metadata for both the full entry array
   * and the two-line read bypass. needEnqRab counts mappings; formerNumWB and
   * latterNumWB count outstanding completion reports, including store sub-work.
   */
  val uopEnqValidSeq = io.enq.req.map(req => io.enq.canAccept && req.valid)
  val enqNeedWriteRFSeq = io.enq.req.map(_.bits.needEnqRab)
  val enqHasExcpSeq = io.enq.req.map(_.bits.hasException)
  val enqRobIdxSeq = io.enq.req.map(req => req.bits.robIdx.value)
  val enqWBNumVec = VecInit(io.enq.req.map(req => req.bits.numWB))
  private val enqWriteStdVec = VecInit(io.enq.req.map(req => req.bits.stdwriteNeed))
  val enqFormerWBNumVec = VecInit(io.enq.req.map(req => req.bits.formerNumWB))
  val enqLatterWBNumVec = VecInit(io.enq.req.map(req => req.bits.latterNumWB))

  /**
   * Clamp simultaneous writeback decrements at zero so unsigned subtraction cannot wrap.
   */
  private val entryState = new RobEntryState
  import entryState.calcNextPackedUopState

  /**
   * [ROB-46]
   * Per-entry state updates aggregate all matching admitted uops/writeback ports.
   * realDestSize can grow across split-uop enqueue cycles. A slot-only redirect
   * retains only the former destination quota, according to CC/CS/SC layout.
   * Completion matching here uses physical value plus slot; upstream redirect
   * filtering and entry-lifetime rules are therefore part of the input contract.
   */
  for (i <- 0 until RobSize) {

    val enqOH = enqOHByEntry(i)
    val robIdxMatchSeq = io.enq.req.map(_.bits.robIdx.value === i.U)
    val uopCanEnqSeq = uopEnqValidSeq.zip(robIdxMatchSeq).map { case (valid, isMatch) => valid && isMatch }
    val instCanEnqFlag = enqOH.asUInt.orR
    val isFirstEnq = !robEntries(i).valid && instCanEnqFlag
    val realDestEnqNum = PopCount(enqNeedWriteRFSeq.zip(uopCanEnqSeq).map { case (writeFlag, valid) => writeFlag && valid })
    val entryPairType = robEntries(i).entryPairType
    val redirectIsFormer = io.redirect.bits.robIdx.isFormer
    val redirectFlushItSelf = io.redirect.bits.flushItself()
    when(isFirstEnq){
      robEntries(i).realDestSize := realDestEnqNum
    }.elsewhen(robEntries(i).valid && Cat(uopCanEnqSeq).orR){
      robEntries(i).realDestSize := robEntries(i).realDestSize + realDestEnqNum
    }.elsewhen(io.redirect.valid && io.redirect.bits.robIdx.value === i.U){
      val clearsLatter = CompressType.isNotNORMAL(entryPairType) && (
        redirectIsFormer && !redirectFlushItSelf ||
        !redirectIsFormer && redirectFlushItSelf
      )
      when(clearsLatter && (CompressType.isCC(entryPairType) || CompressType.isCS(entryPairType))){
        robEntries(i).realDestSize := robEntries(i).complexSlotHasDest
      }.elsewhen(clearsLatter && CompressType.isSC(entryPairType)){
        robEntries(i).realDestSize := robEntries(i).realDestSize - robEntries(i).complexSlotHasDest
      }
    }
    val enqFormerWBNum = Mux1H(enqOH, enqFormerWBNumVec)
    val enqLatterWBNum = Mux1H(enqOH, enqLatterWBNumVec)
    /**
     * Each valid matching completion port contributes one decrement, independently for each slot.
     */
    val writebackEvents = RobWritebackEvents.forEntry(
      i.U, io.writeback.toSeq, exceptionWBs, io.writebackNeedFlush.toSeq
    )
    val formerWbCnt = writebackEvents.formerCount
    val latterWbCnt = writebackEvents.latterCount
    val slotNeedFlushMask = robEntries(i).slotNeedFlushMask
    val needFlushWriteBack = writebackEvents.anyNeedsFlush
    val needFlushWriteBackFormer = writebackEvents.formerNeedsFlush
    val needFlushWriteBackLater = writebackEvents.latterNeedsFlush
    when(robEntries(i).valid && needFlushWriteBack){
      slotNeedFlushMask := slotNeedFlushMask | Cat(needFlushWriteBackLater, needFlushWriteBackFormer)
    }

    val redirectHitThisEntry = robEntries(i).valid &&
      io.redirect.valid &&
      io.redirect.bits.robIdx.value === i.U
    val redirectClearsLatter = redirectHitThisEntry &&
      CompressType.isNotNORMAL(entryPairType) && (
        io.redirect.bits.robIdx.isFormer && !io.redirect.bits.flushItself() ||
        !io.redirect.bits.robIdx.isFormer && io.redirect.bits.flushItself()
      )
    val enqEntryPairType = Mux1H(enqOH, io.enq.req.map(_.bits.entryPairType))
    val nextPackedUopState = calcNextPackedUopState(
      entryValid = robEntries(i).valid,
      entryUopState = robEntries(i).uopState,
      entryCompressType = robEntries(i).entryPairType,
      redirectClearsLatter = redirectClearsLatter,
      needFlushWriteBackFormer = needFlushWriteBackFormer,
      needFlushWriteBackLater = needFlushWriteBackLater,
      instCanEnqFlag = instCanEnqFlag,
      enqEntryPairType = enqEntryPairType,
      enqFormerWBNum = enqFormerWBNum,
      enqLatterWBNum = enqLatterWBNum,
      formerWbCntRaw = formerWbCnt,
      latterWbCntRaw = latterWbCnt
    )
    robEntries(i).uopState := nextPackedUopState

    // trace
    val youngestSlotIsFormer = CompressType.isNORMAL(robEntries(i).entryPairType) || redirectClearsLatter
    val taken = branchWBs.map(writeback =>
      writeback.valid &&
      writeback.bits.robIdx.value === i.U &&
      (writeback.bits.robIdx.isFormer === youngestSlotIsFormer) &&
      writeback.bits.redirect.get.bits.taken
    ).reduce(_ || _)
    when(robEntries(i).valid && Itype.isNonTaken(robEntries(i).traceBlockInPipe.itype) && taken){
      // BranchType code(notaken itype = 4) must be correctly replaced!
      // TODO: In the future, there are two itype in trace
      robEntries(i).traceBlockInPipe.itype := Itype.Taken
    }
  }

  // begin update robBanksRdata
  val robBanksRdata = VecInit(robBanksRdataThisLine ++ robBanksRdataNextLine)
  /**
   * [ROB-47]
   * Two-line combinational bypass mirrors completion/flush-mask/trace updates
   * before RobCommitEntryBundle is registered. Keep this logic synchronized with
   * the full-array transition; the two loops are not completely identical (for
   * example their destination-size enqueue guards differ). Shared helpers must
   * preserve those existing differences until separately validated.
   */
  val needUpdate = Wire(Vec(2 * CommitWidth, new RobEntryBundle))
  needUpdate := VecInit(robBanksRdataThisLine ++ robBanksRdataNextLine)
  val needUpdateRobIdx = robIdxThisLine ++ robIdxNextLine
  for (i <- 0 until 2 * CommitWidth) {
    val enqHeadOH = VecInit(canEnqueue.zip(allocatePtrVec.map(_.value === needUpdateRobIdx(i))).map {
      case (valid, ptrMatch) => valid && ptrMatch
    })
    val robIdxMatchSeq = io.enq.req.map(_.bits.robIdx.value === needUpdateRobIdx(i))
    val uopCanEnqSeq = uopEnqValidSeq.zip(robIdxMatchSeq).map { case (valid, isMatch) => valid && isMatch }
    val instCanEnqFlag = enqHeadOH.asUInt.orR
    val realDestEnqNum = PopCount(enqNeedWriteRFSeq.zip(uopCanEnqSeq).map { case (writeFlag, valid) => writeFlag && valid })
    when(!needUpdate(i).valid && instCanEnqFlag) {
      needUpdate(i).realDestSize := realDestEnqNum
    }.elsewhen(needUpdate(i).valid && instCanEnqFlag) {
      needUpdate(i).realDestSize := robBanksRdata(i).realDestSize + realDestEnqNum
    }
    val enqFormerWBNum = Mux1H(enqHeadOH, enqFormerWBNumVec)
    val enqLatterWBNum = Mux1H(enqHeadOH, enqLatterWBNumVec)
    val writebackEvents = RobWritebackEvents.forEntry(
      needUpdateRobIdx(i), io.writeback.toSeq, exceptionWBs, io.writebackNeedFlush.toSeq
    )
    val formerWbCnt = writebackEvents.formerCount
    val latterWbCnt = writebackEvents.latterCount
    val slotNeedFlushMask = robBanksRdata(i).slotNeedFlushMask
    val needFlushWriteBack = writebackEvents.anyNeedsFlush
    val needFlushWriteBackFormer = writebackEvents.formerNeedsFlush
    val needFlushWriteBackLater = writebackEvents.latterNeedsFlush
    when(needUpdate(i).valid && needFlushWriteBack) {
      needUpdate(i).slotNeedFlushMask := slotNeedFlushMask | Cat(needFlushWriteBackLater, needFlushWriteBackFormer)
    }

    val redirectHitThisEntry = needUpdate(i).valid &&
      io.redirect.valid &&
      io.redirect.bits.robIdx.value === needUpdateRobIdx(i)
    val entryPairType = robBanksRdata(i).entryPairType
    val redirectClearsLatter = redirectHitThisEntry &&
      CompressType.isNotNORMAL(entryPairType) && (
        io.redirect.bits.robIdx.isFormer && !io.redirect.bits.flushItself() ||
        !io.redirect.bits.robIdx.isFormer && io.redirect.bits.flushItself()
      )
    val enqEntryPairType = Mux1H(enqHeadOH, io.enq.req.map(_.bits.entryPairType))
    val nextPackedUopState = calcNextPackedUopState(
      entryValid = needUpdate(i).valid,
      entryUopState = robBanksRdata(i).uopState,
      entryCompressType = robBanksRdata(i).entryPairType,
      redirectClearsLatter = redirectClearsLatter,
      needFlushWriteBackFormer = needFlushWriteBackFormer,
      needFlushWriteBackLater = needFlushWriteBackLater,
      instCanEnqFlag = instCanEnqFlag,
      enqEntryPairType = enqEntryPairType,
      enqFormerWBNum = enqFormerWBNum,
      enqLatterWBNum = enqLatterWBNum,
      formerWbCntRaw = formerWbCnt,
      latterWbCntRaw = latterWbCnt
    )
    needUpdate(i).uopState := nextPackedUopState
    needUpdateCommitW(i) := !nextPackedUopState.orR

    // trace
    val youngestSlotIsFormer = CompressType.isNORMAL(robBanksRdata(i).entryPairType) || redirectClearsLatter
    val taken = branchWBs.map(writeback =>
      writeback.valid &&
      writeback.bits.robIdx.value === needUpdateRobIdx(i) &&
      (writeback.bits.robIdx.isFormer === youngestSlotIsFormer) &&
      writeback.bits.redirect.get.bits.taken
    ).reduce(_ || _)
    when(robBanksRdata(i).valid && Itype.isNonTaken(robBanksRdata(i).traceBlockInPipe.itype) && taken){
      // BranchType code(notaken itype = 4) must be correctly replaced!
      needUpdate(i).traceBlockInPipe.itype := Itype.Taken
    }
  }
  robBanksRdataThisLineUpdate := VecInit(needUpdate.take(bankNum))
  robBanksRdataNextLineUpdate := VecInit(needUpdate.drop(bankNum))
  // end update robBanksRdata

  // interruptSafe
//  for (i <- 0 until RenameWidth) {
//    when(canEnqueue(i)) {
//      // For now, we allow non-load-store instructions to trigger interrupts
//      // For MMIO instructions, they should not trigger interrupts since they may
//      // be sent to lower level before it writes back.
//      // However, we cannot determine whether a load/store instruction is MMIO.
//      // Thus, we don't allow load/store instructions to trigger an interrupt.
//      // TODO: support non-MMIO load-store instructions to trigger interrupts
//      val allow_interrupts = !CommitType.isLoadStore(io.enq.req(i).bits.commitType) && !FuType.isFence(io.enq.req(i).bits.fuType) && !FuType.isCsr(io.enq.req(i).bits.fuType) && !FuType.isVset(io.enq.req(i).bits.fuType)
//      robEntries(allocatePtrVec(i).value).interruptSafe := allow_interrupts
//    }
//  }

  /**
   * read and write of data modules
   */
  val commitReadAddr_next = Mux(state_next === s_idle,
    VecInit(deqPtrVec_next.map(_.value)),
    VecInit(walkPtrVec_next.map(_.value))
  )

  /**
   * [ROB-48]
   * Normalize decode/enqueue and heterogeneous execution exceptions into
   * ExceptionGen inputs. Optional writeback fields get neutral defaults. The
   * current execution adapter forces vector-specific exception fields to zero;
   * their presence in the interface alone does not prove an active producer.
   */
  exceptionGen.io.redirect <> io.redirect
  exceptionGen.io.flush := io.flushOut.valid

  val canEnqueueEG = uopCanEnqueue
  for (i <- 0 until RenameWidth) {
    exceptionGen.io.enq(i).valid := canEnqueueEG(i)
    exceptionGen.io.enq(i).bits.robIdx := io.enq.req(i).bits.robIdx
    exceptionGen.io.enq(i).bits.ftqPtr := io.enq.req(i).bits.ftqPtr
    exceptionGen.io.enq(i).bits.ftqOffset := io.enq.req(i).bits.ftqOffset
    exceptionGen.io.enq(i).bits.isRVC := io.enq.req(i).bits.isRVC
    exceptionGen.io.enq(i).bits.exceptionVec := io.enq.req(i).bits.exceptionVec
    exceptionGen.io.enq(i).bits.satpFlushFirstFetchFault := io.enq.req(i).bits.satpFlushFirstFetchFault
    exceptionGen.io.enq(i).bits.hasException := io.enq.req(i).bits.hasException
    exceptionGen.io.enq(i).bits.isEnqExcp := io.enq.req(i).bits.hasException
    exceptionGen.io.enq(i).bits.isFetchMalAddr := io.enq.req(i).bits.isFetchMalAddr
    exceptionGen.io.enq(i).bits.flushPipe := io.enq.req(i).bits.flushPipe
    exceptionGen.io.enq(i).bits.satpFlush := false.B
    exceptionGen.io.enq(i).bits.isVset := io.enq.req(i).bits.isVset
    exceptionGen.io.enq(i).bits.replayInst := false.B
    XSError(canEnqueue(i) && io.enq.req(i).bits.replayInst, "enq should not set replayInst")
    exceptionGen.io.enq(i).bits.singleStep := io.enq.req(i).bits.singleStep
    exceptionGen.io.enq(i).bits.crossPageIPFFix := io.enq.req(i).bits.crossPageIPFFix
    exceptionGen.io.enq(i).bits.trigger := io.enq.req(i).bits.trigger
    exceptionGen.io.enq(i).bits.vstartEn := false.B //DontCare
    exceptionGen.io.enq(i).bits.vstart := 0.U //DontCare
    exceptionGen.io.enq(i).bits.vuopIdx := 0.U
    exceptionGen.io.enq(i).bits.isVecLoad := false.B
    exceptionGen.io.enq(i).bits.isVlm := false.B
    exceptionGen.io.enq(i).bits.isStrided := false.B
    exceptionGen.io.enq(i).bits.isIndexed := false.B
    exceptionGen.io.enq(i).bits.isWhole := false.B
    exceptionGen.io.enq(i).bits.nf := 0.U
    exceptionGen.io.enq(i).bits.vsew := 0.U
    exceptionGen.io.enq(i).bits.veew := 0.U
    exceptionGen.io.enq(i).bits.vlmul := 0.U
  }

  println(s"ExceptionGen:")
  println(s"num of exceptions: ${params.numException}")
  require(exceptionWBs.length == exceptionGen.io.wb.length,
    f"exceptionWBs.length: ${exceptionWBs.length}, " +
      f"exceptionGen.io.wb.length: ${exceptionGen.io.wb.length}")
  for (((wb, exc_wb), i) <- exceptionWBs.zip(exceptionGen.io.wb).zipWithIndex) {
    exc_wb.valid       := wb.valid
    exc_wb.bits.robIdx := wb.bits.robIdx
    // only enq inst use ftqPtr to read gpa
    exc_wb.bits.ftqPtr          := 0.U.asTypeOf(exc_wb.bits.ftqPtr)
    exc_wb.bits.ftqOffset       := 0.U.asTypeOf(exc_wb.bits.ftqOffset)
    exc_wb.bits.isRVC           := false.B
    exc_wb.bits.exceptionVec    := wb.bits.exceptionVec
    exc_wb.bits.satpFlushFirstFetchFault := false.B
    exc_wb.bits.hasException    := wb.bits.exceptionVec.orR // Todo: use io.writebackNeedFlush(i) instead
    exc_wb.bits.isEnqExcp       := false.B
    exc_wb.bits.isFetchMalAddr  := false.B
    exc_wb.bits.flushPipe       := wb.bits.flushPipe.getOrElse(false.B)
    exc_wb.bits.satpFlush       := wb.bits.satpFlush.getOrElse(false.B)
    exc_wb.bits.isVset          := false.B
    exc_wb.bits.replayInst      := wb.bits.replay.getOrElse(false.B)
    exc_wb.bits.singleStep      := false.B
    exc_wb.bits.crossPageIPFFix := false.B
    val trigger = wb.bits.trigger.getOrElse(TriggerAction.None).asTypeOf(exc_wb.bits.trigger)
    exc_wb.bits.trigger := trigger
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

  /**
   * [ROB-49]
   * Architectural retired-instruction count comes from decoded trace counts,
   * not the number of retired entries or slots. Capture counts on isCommit,
   * then update the 64-bit total and CSR retiredInstr one cycle later.
   */
  val isCommit = io.commits.isCommit
  val isCommitReg = GatedValidRegNext(io.commits.isCommit)
  val instrCntReg = RegInit(0.U(64.W))
  val fusionCommitCountNow = io.commits.commitValid.zip(io.commits.info).zip(deqPtrVec).map {
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
  val trueCommitCnt = RegEnable(io.commits.commitValid.zip(commitInstrCntByEntry).map { case (v, instrCnt) => Mux(v, instrCnt, 0.U) }.reduce(_ +& _), isCommit)
  val retireCounter = Mux(isCommitReg, trueCommitCnt, 0.U)
  val instrCnt = instrCntReg + retireCounter
  when(isCommitReg){
    instrCntReg := instrCnt
  }
  io.csr.perfinfo.retiredInstr := retireCounter
  io.robFull := !allowEnqueue

  io.toVecExcpMod.logicPhyRegMap := rab.io.toVecExcpMod.logicPhyRegMap
  io.toVecExcpMod.excpInfo := vecExcpInfo

  /**
   * trace
   */

  // trace output
  /**
   * [ROB-50]
   * Trace retains original FTQ coordinates and the youngest surviving branch
   * type. Its iretire payload is decoded halfwords. A registered CSR exception
   * overrides block zero with an exception/interrupt marker and zero halfwords.
   */
  val traceValids = io.trace.traceCommitInfo.blocks.map(_.valid)
  val traceBlocks = io.trace.traceCommitInfo.blocks
  val traceBlockInPipe = io.trace.traceCommitInfo.blocks.map(_.bits.tracePipe)

  for (i <- 0 until CommitWidth) {
    traceBlocks(i).bits.ftqIdx.foreach(_ := rawInfo(i).ftqIdx)
    traceBlocks(i).bits.ftqOffset.foreach(_ := rawInfo(i).ftqOffset)
    traceBlockInPipe(i).itype := rawInfo(i).traceBlockInPipe.itype
    traceBlockInPipe(i).iretire := iretireCommit(i)
    traceBlockInPipe(i).ilastsize := rawInfo(i).traceBlockInPipe.ilastsize
    traceValids(i) := io.commits.isCommit && io.commits.commitValid(i)
    // exception only occur in block(0).
    if(i == 0) {
      when(io.exception.valid){ // trace exception
        traceBlocks(i).bits.tracePipe.itype := Mux(io.exception.bits.isInterrupt,
          Itype.Interrupt,
          Itype.Exception
        )
        traceValids(i) := true.B
        traceBlockInPipe(i).iretire := 0.U
      }
    }
  }

  /**
   * debug info
   */
  /**
   * [ROB-51]
   * Read-only debug dumps and pointer-order consistency checks.
   */
  XSDebug(p"enqPtr ${enqPtr} deqPtr ${deqPtr}\n")
  XSDebug("")
  XSError(enqPtr.isBeforeSlot(deqPtr) && !isFull(enqPtr, deqPtr), "\ndeqPtr is older than enqPtr!\n")
  for (i <- 0 until RobSize) {
    XSDebug(false, !robEntries(i).valid, "-")
    XSDebug(false, robEntries(i).valid && robEntries(i).isWritebacked, "w")
    XSDebug(false, robEntries(i).valid && !robEntries(i).isWritebacked, "v")
  }
  XSDebug(false, true.B, "\n")

  for (i <- 0 until RobSize) {
    if (i % 4 == 0) XSDebug("")
    XSDebug(false, true.B, "%x ", debugMeta(debug_microOp(i).head).pc)
    XSDebug(false, !robEntries(i).valid, "- ")
    XSDebug(false, robEntries(i).valid && robEntries(i).isWritebacked, "w ")
    XSDebug(false, robEntries(i).valid && !robEntries(i).isWritebacked, "v ")
    if (i % 4 == 3) XSDebug(false, true.B, "\n")
  }

  def ifCommit(counter: UInt): UInt = Mux(isCommit, counter, 0.U)

  def ifCommitReg(counter: UInt): UInt = Mux(isCommitReg, counter, 0.U)

  /**
   * [ROB-52]
   * Flatten observation lanes as entry0.former, entry0.latter, entry1.former, ... .
   * Suppress latter lanes after downgrade to NORMAL. Event names such as
   * commitUop historically count physical entries; commitInstr counts decoded
   * architectural instructions and can exceed both entry and slot counts.
   */
  val commitDebugUop = deqPtrVec.map(_.value).map(debug_microOp(_)).flatten
  val commitSlotValid = io.commits.commitValid.zip(io.commits.info).flatMap { case (valid, info) =>
    Seq(valid, valid && !CompressType.isNORMAL(info.entryPairType))
  }
  XSPerfAccumulate("clock_cycle", 1.U, XSPerfLevel.CRITICAL)
  QueuePerf(RobSize, numValidEntries, numValidEntries === RobSize.U)
  XSPerfAccumulate("commitUop", ifCommit(commitCnt))
  XSPerfAccumulate("commitInstr", ifCommitReg(trueCommitCnt), XSPerfLevel.CRITICAL)
  XSPerfRolling("ipc", ifCommitReg(trueCommitCnt), 1000, clock, reset)
  XSPerfRolling("cpi", perfCnt = 1.U /*Cycle*/ , eventTrigger = ifCommitReg(trueCommitCnt), granularity = 1000, clock, reset)
  XSPerfAccumulate("commitInstrFused", ifCommitReg(fuseCommitCnt))
  val commitIsLoad = deqPtrVec.map(ptr => debug_microOp(ptr.value)(0).commitType === CommitType.LOAD)
  val commitLoadValid = commitSlotValid.zip(commitDebugUop).map { case (v, uop) => v && uop.commitType === CommitType.LOAD }
  XSPerfAccumulate("commitInstrLoad", ifCommit(PopCount(commitLoadValid)))
  val commitIsBranch = deqPtrVec.map(ptr => FuType.isBrh(debug_microOp(ptr.value)(0).fuType))
  val commitBranchValid = commitSlotValid.zip(commitDebugUop).map { case (v, uop) => v && FuType.isBrh(uop.fuType) }
  XSPerfAccumulate("commitInstrBranch", ifCommit(PopCount(commitBranchValid)))
  val commitIsStore = deqPtrVec.map(ptr => debug_microOp(ptr.value)(0).commitType === CommitType.STORE)
  val commitStoreValid = commitSlotValid.zip(commitDebugUop).map { case (v, uop) => v && uop.commitType === CommitType.STORE }
  XSPerfAccumulate("commitInstrStore", ifCommit(PopCount(commitStoreValid)))
  XSPerfAccumulate("writeback", PopCount((0 until RobSize).map(i => robEntries(i).valid && robEntries(i).isWritebacked)))
  // XSPerfAccumulate("enqInstr", PopCount(io.dp1Req.map(_.fire)))
  // XSPerfAccumulate("d2rVnR", PopCount(io.dp1Req.map(p => p.valid && !p.ready)))
  XSPerfAccumulate("walkInstr", Mux(io.commits.isWalk, PopCount(io.commits.walkValid), 0.U))
  XSPerfAccumulate("walkCycleTotal", state === s_walk)
  XSPerfAccumulate("waitRabWalkEnd", state === s_walk && walkFinished && !rab.io.status.walkEnd)
  private val walkCycle = RegInit(0.U(8.W))
  private val waitRabWalkCycle = RegInit(0.U(8.W))
  walkCycle := Mux(io.redirect.valid, 0.U, Mux(state === s_walk, walkCycle + 1.U, 0.U))
  waitRabWalkCycle := Mux(state === s_walk && walkFinished, 0.U, Mux(state === s_walk, walkCycle + 1.U, 0.U))

  XSPerfHistogram("walkRobCycleHist", walkCycle, state === s_walk && walkFinished, 0, 32)
  XSPerfHistogram("walkRabExtraCycleHist", waitRabWalkCycle, state === s_walk && walkFinished && rab.io.status.walkEnd, 0, 32)
  XSPerfHistogram("walkTotalCycleHist", walkCycle, state === s_walk && state_next === s_idle, 0, 32)

  /**
   * Head wait counters classify the former metadata; they do not independently classify a stalled latter slot.
   */
  private val deqNotWritebacked = robEntries(deqPtr.value).valid && !robEntries(deqPtr.value).isWritebacked
  private val deqUopNotWritebacked = robEntries(deqPtr.value).valid && !robEntries(deqPtr.value).isUopWritebacked
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
    RegEnable(io.commits.info(0).debug_pc.getOrElse(0.U),
              0.U,
              io.commits.isCommit && io.commits.commitValid(0)))
  XSPerfAccumulate("commitCompressCntAll", PopCount(io.commits.commitValid.zip(commitInstrCntByEntry).map { case (valid, instrCnt) => io.commits.isCommit && valid && instrCnt > 1.U }))
  (1 to RenameWidth).foreach(i =>
    XSPerfAccumulate(s"commitCompressCnt${i}", PopCount(io.commits.commitValid.zip(commitInstrCntByEntry).map { case (valid, instrCnt) => io.commits.isCommit && valid && instrCnt === i.U }))
  )
  XSPerfAccumulate("compressSize", io.commits.commitValid.zip(commitInstrCntByEntry).map { case (valid, instrCnt) => Mux(io.commits.isCommit && valid && instrCnt > 1.U, instrCnt, 0.U) }.reduce(_ +& _))
  val compressTypeIsNORMAL = io.commits.commitValid.zip(io.commits.info).map(x => x._1 && CompressType.isNORMAL(x._2.entryPairType))
  val compressTypeIsCC = io.commits.commitValid.zip(io.commits.info).map(x => x._1 && CompressType.isCC(x._2.entryPairType))
  val compressTypeIsCS = io.commits.commitValid.zip(io.commits.info).map(x => x._1 && CompressType.isCS(x._2.entryPairType))
  val compressTypeIsSC = io.commits.commitValid.zip(io.commits.info).map(x => x._1 && CompressType.isSC(x._2.entryPairType))
  XSPerfAccumulate("compressType_NORMAL", ifCommit(PopCount(compressTypeIsNORMAL)))
  XSPerfAccumulate("compressType_CC", ifCommit(PopCount(compressTypeIsCC)))
  XSPerfAccumulate("compressType_CS", ifCommit(PopCount(compressTypeIsCS)))
  XSPerfAccumulate("compressType_SC", ifCommit(PopCount(compressTypeIsSC)))
  XSPerfAccumulate("RobEntriesCommited", ifCommit(PopCount(io.commits.commitValid)))

  // By CommitType
//  val commitIsLoad   = io.commits.info.map(x => x.commitType === CommitType.LOAD)
//  val commitIsStore  = io.commits.info.map(x => x.commitType === CommitType.STORE)
//  val commitIsBranch = io.commits.info.map(x => x.commitType === CommitType.BRANCH)
  val commitIsNormal = io.commits.info.map(x => x.commitType === CommitType.NORMAL)

  XSPerfAccumulate("NoCompressIsLoad", ifCommit(PopCount(compressTypeIsNORMAL.zip(commitIsLoad).map(x => x._1 && x._2))))
  XSPerfAccumulate("NoCompressIsStore", ifCommit(PopCount(compressTypeIsNORMAL.zip(commitIsStore).map(x => x._1 && x._2))))
  XSPerfAccumulate("NoCompressIsBranch", ifCommit(PopCount(compressTypeIsNORMAL.zip(commitIsBranch).map(x => x._1 && x._2))))
  XSPerfAccumulate("NoCompressIsNORMAL", ifCommit(PopCount(compressTypeIsNORMAL.zip(commitIsNormal).map(x => x._1 && x._2))))

  // By FuType
  for (fuType <- FuType.functionNameMap.keys) {
    val fuName = FuType.functionNameMap(fuType)
    val commitIsFuType = compressTypeIsNORMAL.zip(deqPtrVec.map(_.value).map(debug_microOp(_).head)).map {
      case (commit, uop) => commit && uop.fuType === fuType.U
    }
    XSPerfAccumulate(s"NoCompress_$fuName", ifCommit(PopCount(commitIsFuType)))
    val NoCompress_Source = commitIsFuType.zip(io.commits.info).map {
      case (valid, info) =>
        val noEnoughInstr  = valid && info.noCompressReason === NoCompressReason.noEnoughInstr
        val flushedHalf    = valid && info.noCompressReason === NoCompressReason.flushedHalf
        val cannotCompress = valid && info.noCompressReason === NoCompressReason.cannotCompress
        (noEnoughInstr, flushedHalf, cannotCompress)
    }
    XSPerfAccumulate(s"NoCompress_${fuName}_by_noEnoughInstr", ifCommit(PopCount(NoCompress_Source.map(_._1))))
    XSPerfAccumulate(s"NoCompress_${fuName}_by_flushedHalf", ifCommit(PopCount(NoCompress_Source.map(_._2))))
    XSPerfAccumulate(s"NoCompress_${fuName}_by_cannotCompress", ifCommit(PopCount(NoCompress_Source.map(_._3))))
  }

  /**
   * [ROB-53]
   * Per-slot committed-instruction latency statistics use stored timestamps; they do not gate retirement.
   */
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
  XSPerfAccumulate(s"redirect_use_snapshot", io.redirect.valid && io.snpt.useSnpt)

  // top-down info
  /**
   * Forward stored head memory addresses and delayed architectural retirement counts to TopDown consumers.
   */
  io.debugTopDown.toCore.robHeadVaddr.valid := debug_lsTopdownInfo(deqPtr.value).s1.vaddr_valid
  io.debugTopDown.toCore.robHeadVaddr.bits := debug_lsTopdownInfo(deqPtr.value).s1.vaddr_bits
  io.debugTopDown.toCore.robHeadPaddr.valid := debug_lsTopdownInfo(deqPtr.value).s2.paddr_valid
  io.debugTopDown.toCore.robHeadPaddr.bits := debug_lsTopdownInfo(deqPtr.value).s2.paddr_bits
  io.debugTopDown.toDispatch.robTrueCommit := ifCommitReg(trueCommitCnt)
  io.debugTopDown.toDispatch.robHeadLsIssue := debug_lsIssue(deqPtr.value)
  io.debugTopDown.robHeadLqIdx.valid := debug_lqIdxValid(deqPtr.value)(0)
  io.debugTopDown.robHeadLqIdx.bits := debug_lqIdx(deqPtr.value)(0)

  // rolling
  io.debugRolling.robTrueCommit := ifCommitReg(trueCommitCnt)

  // debug trigger for simulation
  if (backendParams.debugEn) {
    val debug_sim_trig = dontTouch(Wire(Bool()))
    debug_sim_trig := io.commits.isCommit && (commitSlotValid zip commitDebugUop map {
      case (valid, uop) => valid && debugMeta(uop).debug_sim_trig
    }).reduce(_ || _)
  }

  /**
   * DataBase info:
   * log trigger is at writeback valid
   * */
  /**
   * [ROB-54]
   * Writeback-time database logging includes execution and memory history; it is distinct from retirement logging.
   */
  if (!env.FPGAPlatform) {
    val instTableName = "InstTable" + p(XSCoreParamsKey).HartId.toString
    val instSiteName = "Rob" + p(XSCoreParamsKey).HartId.toString
    val debug_instTable = ChiselDB.createTable(instTableName, new InstInfoEntry, basicDB = true)
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
  /**
   * Load database logging is retirement-qualified and expands both surviving slots.
   */
  val load_debug_table = ChiselDB.createTable("LoadDebugTable" + p(XSCoreParamsKey).HartId.toString, new LoadInfoEntry, basicDB = true)
  for (i <- 0 until CommitWidth) {
    val commit_index = io.commits.robIdx(i).value
    for (j <- 0 until 2) {
      val slot = 2 * i + j
      val uop = commitDebugUop(slot)
      val log_enable = io.commits.isCommit && commitSlotValid(slot) && uop.commitType === CommitType.LOAD
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
   * Current local vector destination shadow is reset to zero; no update producer exists in this file.
   */
  val debug_VecOtherPdest = RegInit(VecInit.fill(RobSize)(VecInit.fill(8)(0.U(PhyRegIdxWidth.W))))

  // topdown
  /**
   * [ROB-55]
   * Load stall classification uses memory hierarchy matches in priority order:
   * memory, L3, L2, not issued, TLB, MSHR replay, violation replay, then L1.
   * These are observational reasons; they are not LSQ scheduling predicates.
   */
  val notIssue = !debug_lsIssue(deqPtr.value)
  val tlbReplay = io.debugTopDown.fromCore.fromMem.robHeadTlbReplay
  val tlbMiss = io.debugTopDown.fromCore.fromMem.robHeadTlbMiss
  val vioReplay = io.debugTopDown.fromCore.fromMem.robHeadLoadVio
  val mshrReplay = io.debugTopDown.fromCore.fromMem.robHeadLoadMSHR
  val l1Miss = io.debugTopDown.fromCore.fromMem.robHeadMissInDCache
  val l2Miss = io.debugTopDown.fromCore.l2MissMatch
  val l3Miss = io.debugTopDown.fromCore.l3MissMatch
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


  /**
   * [ROB-56]
   * TopDown collector reduces IQ observations per physical entry and accumulates
   * issued/cancel/head/ideal-issue durations until completion. These durations
   * are counters, despite several field names ending in Time. Slot distinction
   * is lost at this collector boundary, unlike difftest's per-slot records.
   */
  if (backendParams.debugEn) {
    val topdownRobInfoCollect = Module(new TopdownRobInfoCollect(backendParams.iqEntryNum, RobSize))
    topdownRobInfoCollect.io.in.zip(io.topdownIQInfoVec.get).foreach{ case (sink, source) =>
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
        val hasWriteBack = robEntries(i).isWritebacked
        val isRobHead = i.U === deqPtr.value

        val topdownIQInfoCandidate = Mux(topdownRobCandidate(i).valid, topdownRobCandidate(i),
          0.U.asTypeOf(new TopdownRobInfo))
        val topdownIQInfoValid = topdownIQInfoCandidate.valid
        val topdownIQIssued = topdownIQInfoCandidate.issued
        val topdownIQIdealIssue = topdownIQInfoCandidate.idealIssueTime

        val topdownIQCancelSource = topdownIQInfoCandidate.cancelSource

        val topdownIssuedUpdate = topdownIQIssued || robEntries(i).topdownIssued.get
        val topdownCanceled= !topdownIQIssued && robEntries(i).topdownIssued.get && topdownIQInfoValid &&
          topdownIQCancelSource =/= IQCancelSource.none
        val topdownCanceledUpdate = topdownCanceled || robEntries(i).topdownCanceled.get
        val topdownRobHeadUpdate = isRobHead || robEntries(i).topdownRobHead.get
        val topdownIdealIssueUpdate = topdownIQIdealIssue || robEntries(i).topdownIdealIssue.get
        val topdownCancelSrcUpdate = Mux(topdownCanceled, topdownIQCancelSource,
          robEntries(i).topdownCancelSource.get)
        val topdownCancelTimeUpdate = topdownCanceled
        robEntries(i).topdownCanceled.foreach(_ := topdownCanceledUpdate)
        robEntries(i).topdownIssued.foreach(_ := topdownIssuedUpdate)
        robEntries(i).topdownRobHead.foreach(_ := topdownRobHeadUpdate)
        robEntries(i).topdownIdealIssue.foreach(_ := topdownIdealIssueUpdate)

        robEntries(i).topdownIssueTime.foreach(_ := Mux(hasWriteBack , 0.U,
          robEntries(i).topdownIssueTime.get +& topdownIssuedUpdate))
        robEntries(i).topdownLastIssueTime.foreach(_ := Mux(hasWriteBack , 0.U,
          Mux(topdownCanceled, 0.U,
            robEntries(i).topdownLastIssueTime.get +& topdownIssuedUpdate)
        ))
        robEntries(i).topdownRobHeadTime.foreach(_ := Mux(hasWriteBack , 0.U,
          robEntries(i).topdownRobHeadTime.get +& topdownRobHeadUpdate))
        // only caculate rising edge of ideal issue
        robEntries(i).topdownIdealIssueTime.foreach(_ := Mux(hasWriteBack , 0.U,
          Mux(topdownIQIdealIssue && !robEntries(i).topdownIdealIssue.get , 1.U,
            robEntries(i).topdownIdealIssueTime.get +& robEntries(i).topdownIdealIssue.get)
        ))
        robEntries(i).topdownCancelSource.foreach(_ := topdownCancelSrcUpdate)
        robEntries(i).topdownCancelTimeVec.foreach(_.zipWithIndex.foreach{ case (cancelTime, index) =>
          val cancelMatch = topdownCancelSrcUpdate === index.asUInt
          cancelTime := Mux(hasWriteBack, 0.U,
            Mux(topdownCanceled && cancelMatch, robEntries(i).topdownCancelTimeVec.get(index) + robEntries(i).topdownLastIssueTime.get,
              robEntries(i).topdownCancelTimeVec.get(index) +& (cancelMatch & topdownCancelTimeUpdate)))
        })
      }
    }
    io.debugRobHeadStall.foreach{ case stall =>
      val deqEntry = robEntries(deqPtr.value)
      val deqEntryNormalLatency = Mux1H(deqEntry.debug_fuType.get, sortedFulatency) +& bypassLatency +& writeBackLatency
      val issueTime = deqEntry.topdownIssueTime.get
      val lastIssueTime = deqEntry.topdownLastIssueTime.get
      val idealIssueTime = deqEntry.topdownIdealIssueTime.get
      val robHeadTime = deqEntry.topdownRobHeadTime.get

      val topdownIQInfoCandidate = topdownRobCandidate(deqPtr.value)
      val issued = topdownIQInfoCandidate.issued && topdownIQInfoCandidate.valid

      when(deqEntry.valid){
        assert(issueTime >= lastIssueTime)
        assert(issueTime <= idealIssueTime)
      }
      val waitTime = idealIssueTime - robHeadTime
      val idealWaitTime = idealIssueTime - issueTime
      val robHeadFutype = deqEntry.debug_fuType.get
      val robHeadExecStall = deqEntry.valid && (lastIssueTime > deqEntryNormalLatency) && (lastIssueTime > waitTime)
      val robHeadNotIssued = deqEntry.valid && (!deqEntry.topdownIssued.get && !issued)
      val robHeadIssueCancel = deqEntry.valid && deqEntry.topdownCanceled.get && (issueTime > deqEntryNormalLatency)
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

      val robHeadCancelSource = deqEntry.topdownCancelSource.get

      val cancelTimeVec = deqEntry.topdownCancelTimeVec.get
      val cancelTimeFixVec = deqEntry.topdownCancelTimeFixVec.get
      /**
       * Subtract already-attributed cycles, saturate each cancel-source remainder,
       * then charge one cycle to the highest-index source among equal maxima.
       */
      val fixedCancelTimeVec = cancelTimeVec.zip(cancelTimeFixVec).map{ case (time, fix) =>
        Mux(time < fix, 0.U, time - fix)
      }
      val maxCancelTime = fixedCancelTimeVec.reduce{ (a,b) => Mux(a > b, a, b)}
      val maxCancelTimeVec = VecInit(fixedCancelTimeVec.map(_ === maxCancelTime)).asUInt
      val lastMaxCancelTimeOH = Reverse(PriorityEncoderOH(Reverse(maxCancelTimeVec)))
      val lastMaxIdx = OHToUInt(lastMaxCancelTimeOH)
      val cancelTimeFixUpdateVec = VecInit(cancelTimeFixVec.zip(lastMaxCancelTimeOH.asBools).map{
        case (time, hit) => time +& hit
      })

      if (backendParams.debugEn){
        dontTouch(VecInit(lastMaxCancelTimeOH.asBools))
      }


      val cancelStallReason = MuxCase(IssueCancelStallOther.id.U, Seq(
        IQCancelSource.isog0(lastMaxIdx) -> IssueCancelStallOg0.id.U  ,
        IQCancelSource.isog1(lastMaxIdx) -> IssueCancelStallOg1.id.U  ,
        IQCancelSource.isld(lastMaxIdx)  -> IssueCancelStallLd.id.U   ,
        IQCancelSource.isst(lastMaxIdx)  -> IssueCancelStallSt.id.U   ,
      ))
      deqEntry.topdownCancelTimeFixVec.foreach(_ := cancelTimeFixUpdateVec)


      stall.valid := robHeadExecStall || robHeadNotIssued || robHeadIssueCancel || robHeadIssueDelay
      stall.bits := MuxCase(BackendOtherCoreStall.id.U, Seq(
        robHeadNotIssued                    -> RobHeadNotIssued.id.U     ,
        robHeadExecStall                    -> robHeadExecStallReason    ,
        robHeadIssueDelay                   -> IssueDelayStall.id.U      ,
        robHeadIssueCancel                  -> cancelStallReason         ,
      ))
    }
  }

  //difftest signals
  val firstValidCommit = deqPtr.addEntries(PriorityMux(io.commits.commitValid, VecInit(List.tabulate(CommitWidth)(_.U(log2Up(CommitWidth).W))))).value

  /**
   * [ROB-57]
   * Prepare exact-slot result/PC vectors for instruction and XSTrap difftest events.
   */
  val wdata = Wire(Vec(2 * CommitWidth, UInt(XLEN.W)))
  val wpc = Wire(Vec(2 * CommitWidth, UInt(XLEN.W)))

  for (i <- 0 until CommitWidth; j <- 0 until 2) {
    val idx = deqPtrVec(i).value
    wdata(2 * i + j) := debug_exuData(idx)(j)
    wpc(2 * i + j) := SignExt(debugMeta(commitDebugUop(2 * i + j)).pc, XLEN)
  }

  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    // Keep the mainline debug payload, independently sampled at each slot head.
    /**
     * [ROB-58]
     * Sample basic destination/debug metadata once per slot head independently of
     * the first-uop-only architectural entry fields. A later split uop must not
     * overwrite the original instruction identity used by DiffInstrCommit.
     */
    val fullBasicDiff = env.EnableDifftest || env.FullBasicDiff
    val dt_basicDebug = Reg(Vec(RobSize, Vec(2, new BasicDebugInfo)))
    val dt_pcTransType = Option.when(fullBasicDiff)(Reg(Vec(RobSize, Vec(2, new AddrTransType))))
    val dt_exuDebug = Reg(Vec(RobSize, Vec(2, new DebugBundle)))

    for (i <- 0 until RenameWidth) {
      val hasEarlierSameHalf = (0 until i).map { j =>
        io.enq.canAccept && io.enq.req(j).valid &&
          io.enq.req(j).bits.robIdx.isSameSlot(io.enq.req(i).bits.robIdx)
      }.foldLeft(false.B)(_ || _)
      val hasEarlierEntryReset = (0 until i).map { j =>
        io.enq.canAccept && io.enq.req(j).valid && io.enq.req(j).bits.firstUop &&
          io.enq.req(j).bits.robIdx.isSameEntry(io.enq.req(i).bits.robIdx)
      }.foldLeft(false.B)(_ || _)
      when(io.enq.canAccept && io.enq.req(i).valid) {
        val uop = io.enq.req(i).bits
        val ptr = uop.robIdx.value
        val slot = slotIndex(uop.robIdx)
        val slotWritten = Mux(uop.firstUop || hasEarlierEntryReset, false.B, enqHalfWritten(ptr)(slot))
        when(!hasEarlierSameHalf && !slotWritten) {
          connectBasicDebug(dt_basicDebug(ptr)(slot), uop)
          dt_pcTransType.foreach(_(ptr)(slot) := io.debugInstrAddrTransType)
        }
      }
    }
    for (wb <- exuWBs) {
      when(wb.valid) {
        dt_exuDebug(wb.bits.robIdx.value)(slotIndex(wb.bits.robIdx)) := wb.bits.debug
      }
    }

    /**
     * CSR observation must either coincide with surviving-former retirement or exclude ordinary retirement.
     */
    val event = io.diffArchEvent.get
    when(event.valid) {
      when(event.bits) {
        assert(io.diffCommitForTrap.get && io.commits.isCommit && io.commits.commitValid.head)
      }.otherwise {
        assert(!hasCommit, "An event at the former slot must not overlap ordinary retirement")
      }
    }
    for (i <- 0 until CommitWidth; j <- 0 until 2) {
      val index = 2 * i + j
      val ptr = deqPtrVec(i).value
      val uop = debug_microOp(ptr)(j)
      val uopDebug = debugMeta(uop)
      val exuOut = dt_exuDebug(ptr)(j)
      val basicDebug = dt_basicDebug(ptr)(j)
      /**
       * One physical retirement expands to up to two difftest records; each slot reports its own fused count.
       */
      val ordinaryValid = io.commits.isCommit && commitSlotValid(index)
      val slotInstrCount = if (j == 0) {
        Mux(CompressType.isNORMAL(io.commits.info(i).entryPairType), instrSizeCommit(i), formerInstrCntCommit(i))
      } else {
        latterInstrCntCommit(i)
      }
      val skip = !basicDebug.eliminatedMove && exuOut.isSkipDiff
      val isVLoad = isVectorMemory(uop) && FuType.isLoad(uop.fuType)
      val diffMaxPhyRegs = Seq(MaxPhyRegs, 2 * (V0PhyRegs + VfPhyRegs)).max
      val difftest = DifftestModule(new DiffInstrCommit(diffMaxPhyRegs), delay = 3, dontCare = true)
      difftest.coreid := io.hartId
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
        difftest.otherwpdest := debug_VecOtherPdest(ptr).zipWithIndex.flatMap { case (pdest, idx) =>
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
        difftestLoadEvent.coreid := io.hartId
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
    /**
     * [ROB-59]
     * XSTrap is a retiring simulation instruction, separate from the CSR exception/interrupt event path.
     */
    val trapVec = commitSlotValid.zip(commitDebugUop).map { case (valid, uop) =>
      io.commits.isCommit && valid && uop.isXSTrap
    }
    val hitTrap = trapVec.reduce(_ || _)
    val difftest = DifftestModule(new DiffTrapEvent, dontCare = true)
    difftest.coreid := io.hartId
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
  /**
   * Slot-selected store PC and total-entry instruction offset support store-event alignment.
   */
  io.storeDebugInfo := DontCare
  if (env.EnableDifftest || env.FullBasicDiff) {
    io.storeDebugInfo.map{port =>
      port.pc := debugMeta(debug_microOp(port.robidx.value)(slotIndex(port.robidx))).pc
      port.storeOffset.foreach(_ := decodeTraceInstrCount(robEntries(port.robidx.value).traceBlockInPipe.iretire) - 1.U)
      // Conservatively include stores from the current head entry, even when it retires this cycle.
      port.preCommit.foreach(_ := port.robidx.isSameEntry(deqPtr))
    }
  }

  /**
   * [ROB-60]
   * Expose branch/redirect and retirement/occupancy performance events without changing control decisions.
   */
  val misPred = io.redirect.valid && io.redirect.bits.isMisPred
  val brhJump = PopCount(isBrhOrJmpWBs.map(wb => wb.valid))

  XSPerfAccumulate("branch_jump", brhJump)
  XSPerfAccumulate("br_mis_pred", misPred)
  XSPerfAccumulate("total_flush", io.redirect.valid)

  val commitLoadVec = VecInit(commitLoadValid)
  val commitBranchVec = VecInit(commitBranchValid)
  val commitStoreVec = VecInit(commitStoreValid)
  val perfEvents = Seq(
    ("rob_interrupt_num      ", io.flushOut.valid && intrEnable),
    ("rob_exception_num      ", io.flushOut.valid && deqHasException),
    ("rob_flush_pipe_num     ", io.flushOut.valid && isFlushPipe),
    ("rob_replay_inst_num    ", io.flushOut.valid && isFlushPipe && deqHasReplayInst),
    ("rob_commitUop          ", ifCommit(commitCnt)),
    ("rob_commitInstr        ", ifCommitReg(trueCommitCnt)),
    ("rob_commitInstrFused   ", ifCommitReg(fuseCommitCnt)),
    ("rob_commitInstrLoad    ", ifCommitReg(PopCount(RegEnable(commitLoadVec, isCommit)))),
    ("rob_commitInstrBranch  ", ifCommitReg(PopCount(RegEnable(commitBranchVec, isCommit)))),
    ("rob_commitInstrStore   ", ifCommitReg(PopCount(RegEnable(commitStoreVec, isCommit)))),
    ("rob_walkInstr          ", Mux(io.commits.isWalk, PopCount(io.commits.walkValid), 0.U)),
    ("rob_walkCycle          ", (state === s_walk)),
    ("rob_1_4_valid          ", numValidEntries <= (RobSize / 4).U),
    ("rob_2_4_valid          ", numValidEntries > (RobSize / 4).U && numValidEntries <= (RobSize / 2).U),
    ("rob_3_4_valid          ", numValidEntries > (RobSize / 2).U && numValidEntries <= (RobSize * 3 / 4).U),
    ("rob_4_4_valid          ", numValidEntries > (RobSize * 3 / 4).U),
    ("BRANCH_JUMP            ", brhJump),
    ("BR_MIS_PRED            ", misPred),
    ("TOTAL_FLUSH            ", io.redirect.valid)
  )
  generatePerfEvent()

  // max commit-stuck cycle
  /**
   * [ROB-61]
   * Commit watchdog pauses counting while head MMIO is busy. WFI suppresses
   * overflow only when automatic WFI resume is disabled. This is a critical
   * error report path, separate from the shorter diagnostic print counter.
   */
  val mmioBusy = io.lsq.mmioBusy // lsq know uncache request is rob head
  val commitStuck = (!io.commits.commitValid.reduce(_ || _) || !io.commits.isCommit) && !mmioBusy
  val commitStuckCycle = RegInit(0.U(log2Up(maxCommitStuck).W))
  when(commitStuck) {
    commitStuckCycle := commitStuckCycle + 1.U
  }.elsewhen(!commitStuck && RegNext(commitStuck)) {
    commitStuckCycle := 0.U
  }
  // check if stuck > 2^maxCommitStuckCycle
  val commitStuck_overflow = commitStuckCycle.andR && (if (wfiResume) true.B else (!hasWFI))
  val criticalErrors = Seq(
    ("rob_commit_stuck  ", commitStuck_overflow),
  )
  generateCriticalErrors()


  // dontTouch for debug
  /**
   * [ROB-62]
   * Preserve selected internal nets for debug elaborations. These annotations
   * affect observability/optimization, not architectural state transitions.
   */
  if (backendParams.debugEn) {
    dontTouch(enqPtrVec)
    dontTouch(deqPtrVec)
    dontTouch(robEntries)
    dontTouch(robDeqGroup)
    dontTouch(robBanks)
    dontTouch(robBanksRaddrThisLine)
    dontTouch(robBanksRaddrNextLine)
    dontTouch(robBanksRdataThisLine)
    dontTouch(robBanksRdataNextLine)
    dontTouch(robBanksRdataThisLineUpdate)
    dontTouch(robBanksRdataNextLineUpdate)
    dontTouch(needUpdate)
    val exceptionWBsVec = MixedVecInit(exceptionWBs)
    dontTouch(exceptionWBsVec)
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
    dontTouch(io.commits)
    dontTouch(commitIsVTypeVec)
    dontTouch(walkIsVTypeVec)
    dontTouch(commitValidThisLine)
    dontTouch(commitReadAddr_next)
    dontTouch(donotNeedWalk)
    dontTouch(walkPtrVec_next)
    dontTouch(walkPtrVec)
    dontTouch(deqPtrVec_next)
    dontTouch(deqPtrVecForWalk)
    dontTouch(snapPtrReadBank)
    dontTouch(snapPtrVecForWalk)
    dontTouch(shouldWalkVec)
    dontTouch(walkFinished)
    dontTouch(changeBankAddrToDeqPtr)
  }
  if (env.EnableDifftest || env.FullBasicDiff) {
    io.commits.info.map(info => dontTouch(info.debug_pc.get))
  }
}
