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
***************************************************************************************/
package xiangshan.mem

import org.chipsalliance.cde.config._
import chisel3._
import chisel3.util._
import utility._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles.{DynInst, ExuOutput, MemExuOutput, IssueQueueLRQWakeUpBundle}
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.Bundles._
import xiangshan.cache._
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.cache.mmu._
import math._

object LoadReplayCauses {
  // these causes have priority, lower coding has higher priority.
  // when load replay happens, load unit will select highest priority
  // from replay causes vector

  /*
   * Warning:
   * ************************************************************
   * * Don't change the priority. If the priority is changed,   *
   * * deadlock may occur. If you really need to change or      *
   * * add priority, please ensure that no deadlock will occur. *
   * ************************************************************
   *
   */
  // uncache
  val C_UNCACHE = 0
  // storeQueue multi forward invalid
  val C_SMF = 1
  // st-ld violation re-execute check
  val C_MA  = 2
  // tlb miss check
  val C_TM  = 3
  // store-to-load-forwarding check
  val C_FF  = 4
  // dcache replay check
  val C_DR  = 5
  // dcache miss check
  val C_DM  = 6
  // wpu predict fail
  val C_WF  = 7
  // dcache bank conflict check / unalign tail split fail
  val C_BC  = 8
  // RAR queue accept check
  val C_RAR = 9
  // RAW queue accept check
  val C_RAW = 10
  // st-ld violation
  val C_NK  = 11
  // misalignBuffer Full
  val C_MF  = 12
  // total causes
  val allCauses = 13

  private val perfCauseNameMap = Seq(
    C_UNCACHE -> "uncache",
    C_SMF -> "storeQueue_multi_match",
    C_MA -> "mem_amb",
    C_TM -> "tlb_miss",
    C_FF -> "forward_fail",
    C_DR -> "dcache_replay",
    C_DM -> "dcache_miss",
    C_WF -> "wpu_fail",
    C_BC -> "bank_conflict",
    C_RAR -> "rar_nack",
    C_RAW -> "raw_nack",
    C_NK -> "nuke",
    C_MF -> "misalign_buffer_full"
  )
  require(perfCauseNameMap.map(_._1).distinct.size == allCauses)
  require(perfCauseNameMap.map(_._1).sorted == (0 until allCauses))
  require(perfCauseNameMap.map(_._2).distinct.size == allCauses)

  val perfCauseNames: Seq[String] = perfCauseNameMap.sortBy(_._1).map(_._2)
}

class VecReplayInfo(implicit p: Parameters) extends XSBundle with HasVLSUParameters {
  val isvec = Bool()
  val isLastElem = Bool()
  val is128bit = Bool()
  val uop_unit_stride_fof = Bool()
  val usSecondInv = Bool()
  val elemIdx = UInt(elemIdxBits.W)
  val alignedType = UInt(alignTypeBits.W)
  val mbIndex = UInt(max(vlmBindexBits, vsmBindexBits).W)
  val elemIdxInsideVd = UInt(elemIdxBits.W)
  val reg_offset = UInt(vOffsetBits.W)
  val vecActive = Bool()
  val is_first_ele = Bool()
  val mask = UInt((VLEN/8).W)
}

class AgeDetector(numEntries: Int, numEnq: Int, regOut: Boolean = true)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    // NOTE: deq and enq may come at the same cycle.
    val enq = Vec(numEnq, Input(UInt(numEntries.W)))
    val deq = Input(UInt(numEntries.W))
    val ready = Input(UInt(numEntries.W))
    val out = Output(UInt(numEntries.W))
  })

  // age(i)(j): entry i enters queue before entry j
  val age = Seq.fill(numEntries)(Seq.fill(numEntries)(RegInit(false.B)))
  val nextAge = Seq.fill(numEntries)(Seq.fill(numEntries)(Wire(Bool())))

  // to reduce reg usage, only use upper matrix
  def get_age(row: Int, col: Int): Bool = if (row <= col) age(row)(col) else !age(col)(row)
  def get_next_age(row: Int, col: Int): Bool = if (row <= col) nextAge(row)(col) else !nextAge(col)(row)
  def isFlushed(i: Int): Bool = io.deq(i)
  def isEnqueued(i: Int, numPorts: Int = -1): Bool = {
    val takePorts = if (numPorts == -1) io.enq.length else numPorts
    takePorts match {
      case 0 => false.B
      case 1 => io.enq.head(i) && !isFlushed(i)
      case n => VecInit(io.enq.take(n).map(_(i))).asUInt.orR && !isFlushed(i)
    }
  }

  for ((row, i) <- nextAge.zipWithIndex) {
    val thisValid = get_age(i, i) || isEnqueued(i)
    for ((elem, j) <- row.zipWithIndex) {
      when (isFlushed(i)) {
        // (1) when entry i is flushed or dequeues, set row(i) to false.B
        elem := false.B
      }.elsewhen (isFlushed(j)) {
        // (2) when entry j is flushed or dequeues, set column(j) to validVec
        elem := thisValid
      }.elsewhen (isEnqueued(i)) {
        // (3) when entry i enqueues from port k,
        // (3.1) if entry j enqueues from previous ports, set to false
        // (3.2) otherwise, set to true if and only of entry j is invalid
        // overall: !jEnqFromPreviousPorts && !jIsValid
        val sel = io.enq.map(_(i))
        val result = (0 until numEnq).map(k => isEnqueued(j, k))
        // why ParallelMux: sel must be one-hot since enq is one-hot
        elem := !get_age(j, j) && !ParallelMux(sel, result)
      }.otherwise {
        // default: unchanged
        elem := get_age(i, j)
      }
      age(i)(j) := elem
    }
  }

  def getOldest(get: (Int, Int) => Bool): UInt = {
    VecInit((0 until numEntries).map(i => {
      io.ready(i) & VecInit((0 until numEntries).map(j => if (i != j) !io.ready(j) || get(i, j) else true.B)).asUInt.andR
    })).asUInt
  }
  val best = getOldest(get_age)
  val nextBest = getOldest(get_next_age)

  io.out := (if (regOut) best else nextBest)
}

object AgeDetector {
  def apply(numEntries: Int, enq: Vec[UInt], deq: UInt, ready: UInt)(implicit p: Parameters): Valid[UInt] = {
    val age = Module(new AgeDetector(numEntries, enq.length, regOut = true))
    age.io.enq := enq
    age.io.deq := deq
    age.io.ready:= ready
    val out = Wire(Valid(UInt(deq.getWidth.W)))
    out.valid := age.io.out.orR
    out.bits := age.io.out
    out
  }
}

object StoreWakeupShouldCancel {
  def apply(scoreboard: Seq[UInt], storeCancel: Seq[LRQWakeUpCancelBundle], isStd: Boolean): Bool = {
    require(scoreboard.head.getWidth >= (if (isStd) 4 else 3))
    require(scoreboard.length == storeCancel.length)
    if (isStd) {
      val og0Cancel = scoreboard.zip(storeCancel.map(_.og0Cancel)).map { case (sc, cancel) => sc(0) && cancel }.reduce(_ || _)
      val og1Cancel = scoreboard.zip(storeCancel.map(_.og1Cancel)).map { case (sc, cancel) => sc(1) && cancel }.reduce(_ || _)
      val s0Cancel = scoreboard.zip(storeCancel.map(_.s0Cancel)).map { case (sc, cancel) => sc(2) && cancel }.reduce(_ || _)
      val s1Cancel = scoreboard.zip(storeCancel.map(_.s1Cancel)).map { case (sc, cancel) => sc(3) && cancel }.reduce(_ || _)

      og0Cancel || og1Cancel || s0Cancel || s1Cancel
    } else {
      val og1Cancel = scoreboard.zip(storeCancel.map(_.og1Cancel)).map { case (sc, cancel) => sc(0) && cancel }.reduce(_ || _)
      val s0Cancel = scoreboard.zip(storeCancel.map(_.s0Cancel)).map { case (sc, cancel) => sc(1) && cancel }.reduce(_ || _)
      val s1Cancel = scoreboard.zip(storeCancel.map(_.s1Cancel)).map { case (sc, cancel) => sc(2) && cancel }.reduce(_ || _)

      og1Cancel || s0Cancel || s1Cancel
    }
  }
}


class LoadQueueReplay(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasMemBlockParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasTlbConst
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    // control
    val redirect = Flipped(ValidIO(new Redirect))
    val robHeadPtr = Input(new RobPtr)

    // from load unit s3
    val enq = Vec(LoadPipelineWidth, Flipped(Decoupled(new LqWriteBundle)))

    // from sta og1
    val storeAddrWakeup = Vec(StorePipelineWidth, Flipped(ValidIO(new IssueQueueLRQWakeUpBundle)))
    val storeAddrWakeupCancel = Vec(StorePipelineWidth, Input(new LRQWakeUpCancelBundle))

    // from std og1
    val storeDataWakeup = Vec(StorePipelineWidth, Flipped(ValidIO(new IssueQueueLRQWakeUpBundle)))
    val storeDataWakeupCancel = Vec(StorePipelineWidth, Input(new LRQWakeUpCancelBundle))

    // actual store data writes accepted by StoreQueue
    val storeDataWrite = Vec(StorePipelineWidth, Flipped(ValidIO(new SqPtr)))

    // queue-based replay
    val replay = Vec(LoadPipelineWidth, Decoupled(new LoadReplayIO))

    val loadWakeup = Flipped(Vec(cfg.numMemChannels, ValidIO(new DCacheLoadWakeup())))

    // from StoreQueue
    val stAddrReadySqPtr = Input(new SqPtr)
    val stAddrReadyVec   = Input(Vec(StoreQueuePhysicalSize, Bool()))
    val stDataReadySqPtr = Input(new SqPtr)
    val stDataReadyVec   = Input(Vec(StoreQueuePhysicalSize, Bool()))
    val sqDeqPtr         = Input(new SqPtr)
    val physicalUpperSqIdx = Input(new SqPtr)

    // from LoadQueueUncache
    val mmioWakeup = Flipped(ValidIO(new LqPtr()))
    val ncWakeup = Flipped(ValidIO(new LqPtr()))
    //
    val sqEmpty = Input(Bool())
    val lqFull  = Output(Bool())
    val ldWbPtr = Input(new LqPtr)
    val rarFull = Input(Bool())
    val rawFull = Input(Bool())
    val l2_hint  = Input(Vec(cfg.numMemChannels, Valid(new L2ToL1Hint())))
    val tlb_hint = Flipped(new TlbHintIO)
    val fast_tlb_hint = Flipped(ValidIO(new TLBHintResp))
    val tlbReplayDelayCycleCtrl = Vec(4, Input(UInt(ReSelectLen.W)))

    val debugTopDown = new LoadQueueTopDownIO
    val replayAllocate = Output(Bool())
  })

  println("LoadQueueReplay size: " + LoadQueueReplaySize)
  //  LoadQueueReplay field:
  //  +-----------+---------+-------+-------------+--------+
  //  | Allocated | MicroOp | VAddr |    Cause    |  Flags |
  //  +-----------+---------+-------+-------------+--------+
  //  Allocated   : entry has been allocated already
  //  MicroOp     : inst's microOp
  //  VAddr       : virtual address
  //  Cause       : replay cause
  //  Flags       : rar/raw queue allocate flags
  val allocated = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B))) // The control signals need to explicitly indicate the initial value
  val scheduled = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  val uop = Reg(Vec(LoadQueueReplaySize, new DynInst))
  val isNC = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  val vecReplay = Reg(Vec(LoadQueueReplaySize, new VecReplayInfo))
  val vaddrModule = Module(new LqVAddrModule(
    gen = UInt(VAddrBits.W),
    numEntries = LoadQueueReplaySize,
    numRead = LoadPipelineWidth,
    numWrite = LoadPipelineWidth,
    numWBank = LoadQueueNWriteBanks,
    numWDelay = 1,
    numCamPort = 0))
  vaddrModule.io := DontCare
  val debug_vaddr = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(VAddrBits.W))))
  val cause = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(LoadReplayCauses.allCauses.W))))
  val blocking = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  val strict = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  // Saturating residence timer for each LRQ entry. It covers the whole lifetime from the
  // first allocation to normal release; re-replay and cause updates do not restart it.
  private val replayResidenceWidth = 16
  val replayResidenceCycles = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(replayResidenceWidth.W))))

  // freeliset: store valid entries index.
  // +---+---+--------------+-----+-----+
  // | 0 | 1 |      ......  | n-2 | n-1 |
  // +---+---+--------------+-----+-----+
  val freeList = Module(new FreeList(
    size = LoadQueueReplaySize,
    allocWidth = LoadPipelineWidth,
    freeWidth = 4,
    enablePreAlloc = true,
    moduleName = "LoadQueueReplay freelist"
  ))
  freeList.io := DontCare
  /**
   * used for re-select control
   */
  val blockSqIdx = Reg(Vec(LoadQueueReplaySize, new SqPtr))
  // DCache miss block
  val missMSHRId = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U((log2Up(cfg.nMissEntries+1).W)))))
  val tlbHintId = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U((log2Up(loadfiltersize+1).W)))))
  // Has this load already updated dcache replacement?
  val replacementUpdated = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  val missDbUpdated = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  val trueCacheMissReplay = WireInit(VecInit(cause.map(_(LoadReplayCauses.C_DM))))
  val replayCarryReg = RegInit(VecInit(List.fill(LoadQueueReplaySize)(ReplayCarry(nWays, 0.U, false.B))))
  val dataInLastBeatReg = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  //  LoadQueueReplay deallocate
  val freeMaskVec = Wire(Vec(LoadQueueReplaySize, Bool()))
  // LoadQueueReplaySize * StorePipelineWidth
  val storeIssueScoreBoard = RegInit(VecInit(List.fill(LoadQueueReplaySize)(VecInit(List.fill(StorePipelineWidth)(0.U(LoadDependenceScoreBoardWidth.W))))))

  // Replay now reaches the load pipeline one cycle earlier. Delay pulse wakeups locally so
  // their producer data keeps the same alignment with the replayed load.
  def delayWakeup[T <: Data](source: ValidIO[T]): ValidIO[T] = {
    val delayed = Wire(Valid(chiselTypeOf(source.bits)))
    delayed.valid := RegNext(source.valid, false.B)
    delayed.bits := RegEnable(source.bits, source.valid)
    delayed
  }

  val storeAddrWakeup = VecInit(io.storeAddrWakeup.map(delayWakeup(_)))
  val storeDataWakeup = VecInit(io.storeDataWakeup.map(delayWakeup(_)))
  val storeAddrWakeupCancel = VecInit(io.storeAddrWakeupCancel.map(cancel => RegNext(cancel, 0.U.asTypeOf(cancel))))
  val storeDataWakeupCancel = VecInit(io.storeDataWakeupCancel.map(cancel => RegNext(cancel, 0.U.asTypeOf(cancel))))
  val loadWakeup = VecInit(io.loadWakeup.map(delayWakeup(_)))
  val loadWakeupPrev = VecInit(loadWakeup.map(delayWakeup(_)))
  // Fast hint selects C_TM replays directly. The delayed original hint remains the fallback.
  val fastTlbHintResp = io.fast_tlb_hint
  val tlbHintResp = delayWakeup(io.tlb_hint.resp)
  val l2Hint = VecInit(io.l2_hint.map(delayWakeup(_)))
  val mmioWakeup = delayWakeup(io.mmioWakeup)
  val ncWakeup = delayWakeup(io.ncWakeup)

  /**
   * Enqueue
   */
  val canEnqueue = io.enq.map(_.valid)
  val cancelEnq = io.enq.map(enq => enq.bits.uop.robIdx.needFlush(io.redirect))
  // Use the producer-side need_rep directly so replay admission does not
  // need to re-derive "has any replay cause" from rep_info.cause on this path.
  val needReplay = io.enq.map(enq => enq.bits.rep_info.need_rep)
  val loadReplay = io.enq.map(enq => enq.bits.isLoadReplay)
  val needEnqueue = VecInit((0 until LoadPipelineWidth).map(w => {
    canEnqueue(w) && !cancelEnq(w) && needReplay(w)
  }))
  val newEnqueue = Wire(Vec(LoadPipelineWidth, Bool()))
  val canFreeVec = VecInit((0 until LoadPipelineWidth).map(w => {
    canEnqueue(w) && loadReplay(w) && !needReplay(w)
  }))

  // select LoadPipelineWidth valid index.
  val lqFull = freeList.io.empty
  val lqFreeNums = freeList.io.validCount

  // replay logic
  // release logic generation
  val storeAddrInSameCycleVec = Wire(Vec(LoadQueueReplaySize, Bool()))
  val storeDataInSameCycleVec = Wire(Vec(LoadQueueReplaySize, Bool()))
  val addrNotBlockVec = Wire(Vec(LoadQueueReplaySize, Bool()))
  val dataNotBlockVec = Wire(Vec(LoadQueueReplaySize, Bool()))
  val storeAddrValidVec = addrNotBlockVec.asUInt | storeAddrInSameCycleVec.asUInt
  val storeDataValidVec = dataNotBlockVec.asUInt | storeDataInSameCycleVec.asUInt
  val storeAddrWakeupVec = WireInit(VecInit(Seq.fill(LoadQueueReplaySize)(VecInit(Seq.fill(StorePipelineWidth)(false.B)))))
  val storeDataWakeupVec = WireInit(VecInit(Seq.fill(LoadQueueReplaySize)(VecInit(Seq.fill(StorePipelineWidth)(false.B)))))

  // store data valid check
  val stAddrReadyVec = io.stAddrReadyVec
  val stDataReadyVec = io.stDataReadyVec

  for (i <- 0 until LoadQueueReplaySize) {
    // dequeue
    //  FIXME: store*Ptr is not accurate
    dataNotBlockVec(i) := io.stDataReadySqPtr.isAfter(blockSqIdx(i)) || stDataReadyVec(blockSqIdx(i).value) || io.sqEmpty // for better timing
    addrNotBlockVec(i) := io.stAddrReadySqPtr.isAfter(blockSqIdx(i)) ||
    !strict(i) && stAddrReadyVec(blockSqIdx(i).value) && blockSqIdx(i) < io.physicalUpperSqIdx || io.sqEmpty // for better timing
    // store address execute
    (0 until StorePipelineWidth).map(w => {
      storeAddrWakeupVec(i)(w) := storeAddrWakeup(w).valid &&
        storeAddrWakeup(w).bits.sqIdx.withInPhysicalQueue(io.sqDeqPtr) &&
        blockSqIdx(i) === storeAddrWakeup(w).bits.sqIdx
    })
    storeAddrInSameCycleVec(i) := storeAddrWakeupVec(i).asUInt.orR // for better timing

    // store data execute
    (0 until StorePipelineWidth).map(w => {
      storeDataWakeupVec(i)(w) := storeDataWakeup(w).valid &&
        blockSqIdx(i) === storeDataWakeup(w).bits.sqIdx
    })
    storeDataInSameCycleVec(i) := storeDataWakeupVec(i).asUInt.orR // for better timing

  }

  // store addr issue check
  val stAddrDeqVec = Wire(Vec(LoadQueueReplaySize, Bool()))
  (0 until LoadQueueReplaySize).map(i => {
    stAddrDeqVec(i) := allocated(i) && storeAddrValidVec(i)
  })

  // store data issue check
  val stDataDeqVec = Wire(Vec(LoadQueueReplaySize, Bool()))
  (0 until LoadQueueReplaySize).map(i => {
    stDataDeqVec(i) := allocated(i) && storeDataValidVec(i)
  })

  // mmio/nc issue check
  val lqIdxMatchMmio = VecInit((0 until LoadQueueReplaySize).map { i =>
    mmioWakeup.valid && mmioWakeup.bits === uop(i).lqIdx
  })

  val lqIdxMatchNc = VecInit((0 until LoadQueueReplaySize).map { i =>
    ncWakeup.valid && ncWakeup.bits === uop(i).lqIdx
  })
  private def tlDChannelHit(mshrId: UInt): Bool = {
    VecInit(loadWakeup.map(ch => ch.valid && ch.bits.mshrId === mshrId)).asUInt.orR
  }

  private def tlDChannelHitPrev(mshrId: UInt): Bool = {
    VecInit(loadWakeupPrev.map(ch => ch.valid && ch.bits.mshrId === mshrId)).asUInt.orR
  }

  private def l2HintMatchVec(mshrId: UInt): Seq[Bool] = {
    l2Hint.map(hint => hint.valid && hint.bits.sourceId === mshrId)
  }

  private def l2HintHit(mshrId: UInt): Bool = {
    VecInit(l2HintMatchVec(mshrId)).asUInt.orR
  }

  private def l2HintIsKeyword(mshrId: UInt): Bool = {
    val matchVec = l2HintMatchVec(mshrId)
    assert(PopCount(VecInit(matchVec)) <= 1.U, "multiple l2_hint hits for one replay entry")
    Mux(VecInit(matchVec).asUInt.orR, Mux1H(matchVec.zip(l2Hint.map(_.bits.isKeyword))), false.B)
  }

  val storeAddrWakeupCancelVec = VecInit((0 until LoadQueueReplaySize).map(i =>
    allocated(i) && cause(i)(LoadReplayCauses.C_MA) &&
      StoreWakeupShouldCancel(storeIssueScoreBoard(i), storeAddrWakeupCancel, isStd = false)
  ))
  val storeDataWakeupCancelVec = VecInit((0 until LoadQueueReplaySize).map(i =>
    allocated(i) && cause(i)(LoadReplayCauses.C_FF) &&
      StoreWakeupShouldCancel(storeIssueScoreBoard(i), storeDataWakeupCancel, isStd = true)
  ))
  val storeAddrWakeupCount = PopCount((0 until LoadQueueReplaySize).map(i => storeAddrWakeupVec(i).asUInt.orR && allocated(i)))
  val storeDataWakeupCount = PopCount((0 until LoadQueueReplaySize).map(i => storeDataWakeupVec(i).asUInt.orR && allocated(i)))

  // update blocking condition
  (0 until LoadQueueReplaySize).map(i => {
    // case C_MA
    when (cause(i)(LoadReplayCauses.C_MA)) {
      blocking(i) := Mux(stAddrDeqVec(i), false.B, blocking(i))
    }
    // case C_TM
    when (cause(i)(LoadReplayCauses.C_TM)) {
      blocking(i) := Mux(tlbHintResp.valid &&
                     (tlbHintResp.bits.replay_all ||
                     tlbHintResp.bits.id === tlbHintId(i)), false.B, blocking(i))
    }
    // case C_FF
    when (cause(i)(LoadReplayCauses.C_FF)) {
      blocking(i) := Mux(stDataDeqVec(i), false.B, blocking(i))
    }
    // case C_DM
    when (cause(i)(LoadReplayCauses.C_DM)) {
      blocking(i) := Mux(tlDChannelHit(missMSHRId(i)), false.B, blocking(i))
    }
    // case C_RAR
    when (cause(i)(LoadReplayCauses.C_RAR)) {
      blocking(i) := Mux((!io.rarFull || !isAfter(uop(i).lqIdx, io.ldWbPtr)), false.B, blocking(i))
    }
    // case C_RAW
    when (cause(i)(LoadReplayCauses.C_RAW)) {
      blocking(i) := Mux((!io.rawFull || blockSqIdx(i).isNotAfter(io.stAddrReadySqPtr)), false.B, blocking(i))
    }
    // case C_MF
    when (cause(i)(LoadReplayCauses.C_MF)) {
      assert(false.B) // TODO: remove C_MF
      blocking(i) := false.B
    }
    // case C_UNCACHE
    when (cause(i)(LoadReplayCauses.C_UNCACHE)) {
      blocking(i) := Mux(lqIdxMatchMmio(i) || lqIdxMatchNc(i), false.B, blocking(i))
    }
    // casue C_SMF
    when (cause(i)(LoadReplayCauses.C_SMF)) {
      blocking(i) := Mux(blockSqIdx(i).isNotAfter(io.sqDeqPtr), false.B, blocking(i))
    }
  })

  (0 until LoadQueueReplaySize).foreach { case i =>
    when(cause(i)(LoadReplayCauses.C_MA) || cause(i)(LoadReplayCauses.C_FF)) {
      when(storeAddrWakeupVec(i).reduce(_ || _) || storeDataWakeupVec(i).reduce(_ || _)) {
        when(cause(i)(LoadReplayCauses.C_MA)) {
          storeIssueScoreBoard(i).zip(storeAddrWakeupVec(i)).foreach{ case (sink, source) =>
            sink := Cat(0.U((LoadDependenceScoreBoardWidth - 1).W), source && allocated(i))
          }
        }
        when(cause(i)(LoadReplayCauses.C_FF)) {
          storeIssueScoreBoard(i).zip(storeDataWakeupVec(i)).foreach{ case (sink, source) =>
            sink := Cat(0.U((LoadDependenceScoreBoardWidth - 1).W), source && allocated(i))
          }
        }
      }.otherwise {
        storeIssueScoreBoard(i).foreach { case x =>
          x := x << 1.U
        }
      }
    }.otherwise {
      storeIssueScoreBoard(i).foreach { case x =>
        x := 0.U
      }
    }

      XSError(allocated(i) && cause(i)(LoadReplayCauses.C_MA) && PopCount(storeAddrWakeupVec(i)) > 1.U, s"storeAddrWakeup source exceed 1! ${i}\n")
      XSError(allocated(i) && cause(i)(LoadReplayCauses.C_FF) && PopCount(storeDataWakeupVec(i)) > 1.U, s"storeDataWakeup source exceed 1! ${i}\n")
  }

  // Replay is split into selection and request stages.
  require((LoadQueueReplaySize % LoadPipelineWidth) == 0)
  def getRemBits(input: UInt)(rem: Int): UInt = {
    VecInit((0 until LoadQueueReplaySize / LoadPipelineWidth).map(i => { input(LoadPipelineWidth * i + rem) })).asUInt
  }

  def getRemSeq(input: Seq[Seq[Bool]])(rem: Int) = {
    (0 until LoadQueueReplaySize / LoadPipelineWidth).map(i => { input(LoadPipelineWidth * i + rem) })
  }

  // stage 0: select entries and start reading their vaddr
  val s0_oldestSel = Wire(Vec(LoadPipelineWidth, Valid(UInt(LoadQueueReplaySize.W))))
  val s1_can_go = Wire(Vec(LoadPipelineWidth, Bool()))
  val s1_oldestSel = Wire(Vec(LoadPipelineWidth, Valid(UInt(log2Up(LoadQueueReplaySize + 1).W))))

  // generate mask
  val needCancel = Wire(Vec(LoadQueueReplaySize, Bool()))
  // generate enq mask
  val enqIndexOH = Wire(Vec(LoadPipelineWidth, UInt(LoadQueueReplaySize.W)))
  val s0_loadEnqFireMask = newEnqueue.zip(enqIndexOH).map(x => Mux(x._1, x._2, 0.U))
  val s0_remLoadEnqFireVec = s0_loadEnqFireMask.map(x => VecInit((0 until LoadPipelineWidth).map(rem => getRemBits(x)(rem))))
  val s0_remEnqSelVec = Seq.tabulate(LoadPipelineWidth)(w => VecInit(s0_remLoadEnqFireVec.map(x => x(w))))

  // generate free mask
  val s0_loadFreeSelMask = GatedRegNext(freeMaskVec.asUInt)
  val s0_remFreeSelVec = VecInit(Seq.tabulate(LoadPipelineWidth)(rem => getRemBits(s0_loadFreeSelMask)(rem)))

  // l2 hint wakes up cache missed load
  // l2 will send GrantData in next 2/3 cycle, wake up the missed load early and sent them to load pipe, so them will hit the data in D channel or mshr in load S1
  val s0_loadHintWakeMask = VecInit((0 until LoadQueueReplaySize).map(i => {
    allocated(i) && !scheduled(i) && cause(i)(LoadReplayCauses.C_DM) && blocking(i) && l2HintHit(missMSHRId(i))
  })).asUInt
  val s0_tlbHintWakeMask = VecInit((0 until LoadQueueReplaySize).map(i => {
    allocated(i) && !scheduled(i) && cause(i)(LoadReplayCauses.C_TM) && blocking(i) &&
      fastTlbHintResp.valid && fastTlbHintResp.bits.id === tlbHintId(i)
  })).asUInt
  // l2 will send 2 beats data in 2 cycles, so if data needed by this load is in first beat, select it this cycle, otherwise next cycle
  // when isKeyword = 1, s0_loadHintSelMask need overturn
    val s0_loadHintSelMask = VecInit((0 until LoadQueueReplaySize).map(i => {
      s0_loadHintWakeMask(i) && Mux(
        l2HintIsKeyword(missMSHRId(i)),
        dataInLastBeatReg(i),
        !dataInLastBeatReg(i)
      )
    })).asUInt
  val s0_remLoadHintSelMask = VecInit((0 until LoadPipelineWidth).map(rem => getRemBits(s0_loadHintSelMask)(rem)))
  val s0_remHintSelValidVec = VecInit((0 until LoadPipelineWidth).map(rem => ParallelORR(s0_remLoadHintSelMask(rem))))
  val s0_hintSelValid = ParallelORR(s0_loadHintSelMask)

  // wake up cache missed load
  (0 until LoadQueueReplaySize).foreach(i => {
    when(s0_loadHintWakeMask(i)) {
      blocking(i) := false.B
    }
    when(s0_tlbHintWakeMask(i)) {
      blocking(i) := false.B
    }
  })

  // generate replay mask
  // replay select priority is given as follow
  // 1. hint wake up load
  // 2. higher priority load
  // 3. lower priority load
  val s0_loadHigherPriorityReplaySelMask = VecInit((0 until LoadQueueReplaySize).map(i => {
    val hasHigherPriority = cause(i)(LoadReplayCauses.C_DM) || cause(i)(LoadReplayCauses.C_FF) || cause(i)(LoadReplayCauses.C_UNCACHE)
    allocated(i) && !scheduled(i) && !blocking(i) && hasHigherPriority
  })).asUInt // use uint instead vec to reduce verilog lines
  val s0_remLoadHigherPriorityReplaySelMask = VecInit((0 until LoadPipelineWidth).map(rem => getRemBits(s0_loadHigherPriorityReplaySelMask)(rem)))
  val s0_loadLowerPriorityReplaySelMask = VecInit((0 until LoadQueueReplaySize).map(i => {
    val hasLowerPriority = !cause(i)(LoadReplayCauses.C_DM) && !cause(i)(LoadReplayCauses.C_FF)
    allocated(i) && !scheduled(i) && (!blocking(i) || s0_tlbHintWakeMask(i)) && hasLowerPriority
  })).asUInt // use uint instead vec to reduce verilog lines
  val s0_remLoadLowerPriorityReplaySelMask = VecInit((0 until LoadPipelineWidth).map(rem => getRemBits(s0_loadLowerPriorityReplaySelMask)(rem)))
  val s0_loadNormalReplaySelMask = s0_loadLowerPriorityReplaySelMask | s0_loadHigherPriorityReplaySelMask | s0_loadHintSelMask
  val s0_remPriorityReplaySelVec = VecInit((0 until LoadPipelineWidth).map(rem => {
        Mux(s0_remHintSelValidVec(rem), s0_remLoadHintSelMask(rem),
          Mux(ParallelORR(s0_remLoadHigherPriorityReplaySelMask(rem)), s0_remLoadHigherPriorityReplaySelMask(rem), s0_remLoadLowerPriorityReplaySelMask(rem)))
      }))
  /******************************************************************************************************
   * WARNING: Make sure that OldestSelectStride must less than or equal stages of load pipeline.        *
   ******************************************************************************************************
   */
  val OldestSelectStride = 4
  val oldestPtrExt = (0 until OldestSelectStride).map(i => io.ldWbPtr + i.U)
  val s0_oldestMatchMaskVec = (0 until LoadQueueReplaySize).map(i => (0 until OldestSelectStride).map(j => s0_loadNormalReplaySelMask(i) && uop(i).lqIdx === oldestPtrExt(j)))
  val s0_remOldestMatchMaskVec = (0 until LoadPipelineWidth).map(rem => getRemSeq(s0_oldestMatchMaskVec.map(_.take(1)))(rem))
  val s0_remOlderMatchMaskVec = (0 until LoadPipelineWidth).map(rem => getRemSeq(s0_oldestMatchMaskVec.map(_.drop(1)))(rem))
  val s0_remOldestMatchMask = (0 until LoadPipelineWidth).map(rem => VecInit(s0_remOldestMatchMaskVec(rem).map(_.head)).asUInt)
  val s0_remOlderMatchMask = (0 until LoadPipelineWidth).map(rem => VecInit(s0_remOlderMatchMaskVec(rem).map(VecInit(_).asUInt.orR)).asUInt)
  val s0_remOldestMatch = s0_remOldestMatchMask.map(ParallelORR(_))
  val s0_remOlderMatch = s0_remOlderMatchMask.map(ParallelORR(_))
  val s0_remOldestSelVec = (0 until LoadPipelineWidth).map(rem =>
    Mux(s0_remOldestMatch(rem), s0_remOldestMatchMask(rem), s0_remOlderMatchMask(rem))
  )
  val s0_remOldestHintSelVec_ = s0_remOldestMatchMask.zip(s0_remLoadHintSelMask).map { case (oldestVec, hintVec) =>
    oldestVec & hintVec
  }
  val s0_remOlderHintSelVec_ = s0_remOlderMatchMask.zip(s0_remLoadHintSelMask).map { case (olderVec, hintVec) =>
    olderVec & hintVec
  }
  val s0_remOldestHintSel = (0 until LoadPipelineWidth).map(rem =>
    Mux(s0_remOldestMatch(rem), ParallelORR(s0_remOldestHintSelVec_(rem)), ParallelORR(s0_remOlderHintSelVec_(rem)))
  )
  val s0_remOldestHintSelOH = (0 until LoadPipelineWidth).map(rem =>
    Mux(s0_remOldestMatch(rem), PriorityEncoderOH(s0_remOldestHintSelVec_(rem)), PriorityEncoderOH(s0_remOlderHintSelVec_(rem)))
  )
  val s0_remOldestSelOH = (0 until LoadPipelineWidth).map(rem =>
    Mux(s0_remOldestMatch(rem), PriorityEncoderOH(s0_remOldestMatchMask(rem)), PriorityEncoderOH(s0_remOlderMatchMask(rem)))
  )
  val s0_remOldestHintSelVec = s0_remOldestSelVec.zip(s0_remLoadHintSelMask).map {
    case(oldestVec, hintVec) => oldestVec & hintVec
  }

  // select oldest logic
  s0_oldestSel := VecInit((0 until LoadPipelineWidth).map(rport => {
    // select enqueue earlest inst
    val ageOldest = AgeDetector(LoadQueueReplaySize / LoadPipelineWidth, s0_remEnqSelVec(rport), s0_remFreeSelVec(rport), s0_remPriorityReplaySelVec(rport))
    assert(!(ageOldest.valid && PopCount(ageOldest.bits) > 1.U), "oldest index must be one-hot!")
    val ageOldestValid = ageOldest.valid
    val ageOldestIndexOH = ageOldest.bits

    // select program order oldest
    val l2HintFirst = ParallelORR(s0_remOldestHintSelVec(rport))
    val issOldestValid = l2HintFirst || ParallelORR(s0_remOldestSelVec(rport))
    val issOldestIndexOH = Mux(l2HintFirst, PriorityEncoderOH(s0_remOldestHintSelVec(rport)), PriorityEncoderOH(s0_remOldestSelVec(rport)))

    val oldest = Wire(Valid(UInt()))
    val oldestSel = Mux(issOldestValid, issOldestIndexOH, ageOldestIndexOH)
    val oldestBitsVec = Wire(Vec(LoadQueueReplaySize, Bool()))

    require((LoadQueueReplaySize % LoadPipelineWidth) == 0)
    oldestBitsVec.foreach(e => e := false.B)
    for (i <- 0 until LoadQueueReplaySize / LoadPipelineWidth) {
      oldestBitsVec(i * LoadPipelineWidth + rport) := oldestSel(i)
    }

    oldest.valid := ageOldestValid || issOldestValid
    oldest.bits := oldestBitsVec.asUInt
    oldest
  }))

  // stage 1: hold and send the replay request to the load unit
  // replay cold down
  val ColdDownCycles = 16
  val coldCounter = RegInit(VecInit(List.fill(LoadPipelineWidth)(0.U(log2Up(ColdDownCycles).W))))
  val ColdDownThreshold = Wire(UInt(log2Up(ColdDownCycles).W))
  ColdDownThreshold := Constantin.createRecord(s"ColdDownThreshold_${p(XSCoreParamsKey).HartId}", initValue = 12)
  assert(ColdDownCycles.U > ColdDownThreshold, "ColdDownCycles must great than ColdDownThreshold!")

  def replayCanFire(i: Int) = coldCounter(i) >= 0.U && coldCounter(i) < ColdDownThreshold
  def coldDownNow(i: Int) = coldCounter(i) >= ColdDownThreshold

  val replay_req = io.replay
  val s1_cancelReplay = Wire(Vec(LoadPipelineWidth, Bool()))

  for (i <- 0 until LoadPipelineWidth) {
    val s0_can_go = s1_can_go(i) ||
                    s1_cancelReplay(i) ||
                    uop(s1_oldestSel(i).bits).robIdx.needFlush(io.redirect) ||
                    uop(s1_oldestSel(i).bits).robIdx.needFlush(RegNext(io.redirect))
    val s0_oldestSelIndexOH = s0_oldestSel(i).bits // one-hot
    val s0_oldestSelV = s0_oldestSel(i).valid
    // A fired request must leave s1 even when cooldown prevents accepting another selection.
    s1_oldestSel(i).valid := RegEnable(
      Mux(s0_can_go, s0_oldestSelV, false.B),
      false.B,
      s0_can_go || replay_req(i).fire
    )
    s1_oldestSel(i).bits := RegEnable(OHToUInt(s0_oldestSel(i).bits), s0_can_go)
    vaddrModule.io.ren(i) := s0_can_go && s0_oldestSelV
    vaddrModule.io.raddr(i) := OHToUInt(s0_oldestSel(i).bits)

    for (j <- 0 until LoadQueueReplaySize) {
      when (s0_can_go && s0_oldestSelV && s0_oldestSelIndexOH(j)) {
        scheduled(j) := true.B
      }
    }
  }
  for (i <- 0 until LoadPipelineWidth) {
    val s1_replayIdx = s1_oldestSel(i).bits
    val s1_redirectCancel = uop(s1_replayIdx).robIdx.needFlush(io.redirect) ||
      uop(s1_replayIdx).robIdx.needFlush(RegNext(io.redirect))
    s1_cancelReplay(i) := s1_redirectCancel
    s1_can_go(i) := replayCanFire(i) && (!s1_oldestSel(i).valid || replay_req(i).fire) || s1_cancelReplay(i)

    val s1_replayUop = uop(s1_replayIdx)
    val s1_nc = isNC(s1_replayIdx)
    val s1_vecReplay = vecReplay(s1_replayIdx)
    val s1_replayMSHRId = missMSHRId(s1_replayIdx)
    val s1_missDbUpdated = missDbUpdated(s1_replayIdx)
    val s1_replayCauses = cause(s1_replayIdx)
    val replay_req_vaddr = vaddrModule.io.rdata(i)
    val replay_req_size = LSUOpType.size(s1_replayUop.fuOpType)
    replay_req(i).valid := s1_oldestSel(i).valid && !s1_cancelReplay(i)
    replay_req(i).bits.entrance := Mux(
      s1_replayCauses(LoadReplayCauses.C_DM) || s1_replayCauses(LoadReplayCauses.C_UNCACHE),
      LoadEntrance.replayHiPrio.U,
      LoadEntrance.replayLoPrio.U
    )
    replay_req(i).bits.accessType.instrType := Mux(s1_vecReplay.isvec, InstrType.vector.U, InstrType.scalar.U)
    replay_req(i).bits.accessType.pftType := DontCare
    replay_req(i).bits.accessType.pftCoh := DontCare
    replay_req(i).bits.uop := s1_replayUop
    replay_req(i).bits.uop.exceptionVec(loadAddrMisaligned) := false.B
    replay_req(i).bits.vaddr := replay_req_vaddr
    replay_req(i).bits.fullva := replay_req_vaddr
    replay_req(i).bits.size := Mux(s1_vecReplay.isvec, s1_vecReplay.alignedType, replay_req_size)
    replay_req(i).bits.mask := Mux(
      s1_vecReplay.isvec,
      s1_vecReplay.mask,
      genVWmask(replay_req_vaddr, replay_req_size)
    )
    replay_req(i).bits.occupySource := DontCare
    replay_req(i).bits.mshrId.get := s1_replayMSHRId
    replay_req(i).bits.replayQueueIdx.get := s1_replayIdx
    replay_req(i).bits.cause.get := s1_replayCauses.asTypeOf(replay_req(i).bits.cause.get)
    replay_req(i).bits.forwardDChannel.get := s1_replayCauses(LoadReplayCauses.C_DM)
    replay_req(i).bits.uncacheReplay.get := s1_replayCauses(LoadReplayCauses.C_UNCACHE)
    replay_req(i).bits.ncReplay.get := s1_replayCauses(LoadReplayCauses.C_UNCACHE) && s1_nc
    replay_req(i).bits.elemIdx.get := s1_vecReplay.elemIdx
    replay_req(i).bits.mbIndex.get := s1_vecReplay.mbIndex
    replay_req(i).bits.regOffset.get := s1_vecReplay.reg_offset
    replay_req(i).bits.elemIdxInsideVd.get := s1_vecReplay.elemIdxInsideVd
    replay_req(i).bits.vecBaseVaddr.get := DontCare
    replay_req(i).bits.vecVaddrOffset.get := DontCare
    replay_req(i).bits.vecTriggerMask.get := DontCare
    replay_req(i).bits.hasROBEntry := true.B
    replay_req(i).bits.missDbUpdated := s1_missDbUpdated

    XSError(replay_req(i).fire && !allocated(s1_replayIdx), p"LoadQueueReplay: why replay an invalid entry ${s1_replayIdx} ?")
  }

  // update cold counter
  val lastReplay = RegNext(VecInit(io.replay.map(_.fire)))
  for (i <- 0 until LoadPipelineWidth) {
    when (lastReplay(i) && io.replay(i).fire) {
      coldCounter(i) := coldCounter(i) + 1.U
    } .elsewhen (coldDownNow(i)) {
      coldCounter(i) := coldCounter(i) + 1.U
    } .otherwise {
      coldCounter(i) := 0.U
    }
  }

  // init
  freeMaskVec.map(e => e := false.B)

  // LoadQueueReplay can't backpressure.
  // We think LoadQueueReplay can always enter, as long as it is the same size as VirtualLoadQueue.
  XSError(!freeList.io.canAllocate.reduce(_ || _) && io.enq.map{ case port =>
    port.valid && !port.bits.isLoadReplay}.reduce(_ || _), s"LoadQueueReplay Overflow")

  // Allocate logic
  needEnqueue.zip(newEnqueue).zip(io.enq).map {
    case ((needEnq, newEnq), enq) =>
      newEnq := needEnq && !enq.bits.isLoadReplay
  }

  val ffEnqNonblocking = Wire(Vec(LoadPipelineWidth, Bool()))
  for ((enq, w) <- io.enq.zipWithIndex) {
    vaddrModule.io.wen(w) := false.B
    freeList.io.doAllocate(w) := false.B

    freeList.io.allocateReq(w) := true.B

    //  Allocated ready
    val offset = PopCount(newEnqueue.take(w))
    val enqIndex = Mux(enq.bits.isLoadReplay, enq.bits.schedIndex, freeList.io.allocateSlot(offset))
    enqIndexOH(w) := UIntToOH(enqIndex)
    enq.ready := true.B

    val debug_robIdx = enq.bits.uop.robIdx.asUInt
    XSError(
      needEnqueue(w) && enq.ready &&
      allocated(enqIndex) && !enq.bits.isLoadReplay,
      p"LoadQueueReplay: can not accept more load, check: ldu $w, robIdx $debug_robIdx!")

    val enqFireBase = enq.fire && !cancelEnq(w)
    val replayInfo = enq.bits.rep_info
    val isMA = replayInfo.cause(LoadReplayCauses.C_MA)
    val isFF = replayInfo.cause(LoadReplayCauses.C_FF)
    val isRAW = replayInfo.cause(LoadReplayCauses.C_RAW)
    val isSMF = replayInfo.cause(LoadReplayCauses.C_SMF)
    val ffWaitSqIdx = replayInfo.data_inv_sq_idx
    val ffWakeupMatch = VecInit(storeDataWakeup.map { wakeup =>
      wakeup.valid && wakeup.bits.sqIdx === ffWaitSqIdx
    }).asUInt.orR
    val ffStoreWriteMatch = VecInit(io.storeDataWrite.map { write =>
      write.valid && write.bits === ffWaitSqIdx
    }).asUInt.orR
    val ffProducerReadyNow = ffWakeupMatch || ffStoreWriteMatch
    val ffEnqAccepted = enqFireBase && replayInfo.need_rep && isFF
    ffEnqNonblocking(w) := ffEnqAccepted && ffProducerReadyNow
    val nextBlockSqIdx = Mux(isMA, replayInfo.addr_inv_sq_idx,
    Mux(isFF, replayInfo.data_inv_sq_idx, enq.bits.uop.sqIdx))

    // special case: st-ld violation
    when (enqFireBase && (isMA || isFF || isRAW || isSMF)) {
      blockSqIdx(enqIndex) := nextBlockSqIdx
    }

    // special case: data forward fail
    when (enqFireBase && isMA) {
      strict(enqIndex) := enq.bits.uop.loadWaitStrict
    }.otherwise{
      strict(enqIndex) := false.B
    }

    when (needEnqueue(w) && enq.ready) {
      freeList.io.doAllocate(w) := !enq.bits.isLoadReplay

      //  Allocate new entry
      allocated(enqIndex) := true.B
      scheduled(enqIndex) := false.B
      uop(enqIndex)       := enq.bits.uop
      uop(enqIndex).exceptionVec.zeroInit()
      isNC(enqIndex)      := enq.bits.nc && enq.bits.rep_info.cause(LoadReplayCauses.C_UNCACHE)
      vecReplay(enqIndex).isvec := enq.bits.isvec
      vecReplay(enqIndex).isLastElem := enq.bits.isLastElem
      vecReplay(enqIndex).is128bit := enq.bits.is128bit
      vecReplay(enqIndex).uop_unit_stride_fof := enq.bits.uop_unit_stride_fof
      vecReplay(enqIndex).usSecondInv := enq.bits.usSecondInv
      vecReplay(enqIndex).elemIdx := enq.bits.elemIdx
      vecReplay(enqIndex).alignedType:= enq.bits.alignedType
      vecReplay(enqIndex).mbIndex := enq.bits.mbIndex
      vecReplay(enqIndex).elemIdxInsideVd := enq.bits.elemIdxInsideVd
      vecReplay(enqIndex).reg_offset := enq.bits.reg_offset
      vecReplay(enqIndex).vecActive := enq.bits.vecActive
      vecReplay(enqIndex).is_first_ele := enq.bits.is_first_ele
      vecReplay(enqIndex).mask         := enq.bits.mask

      vaddrModule.io.wen(w)   := true.B
      vaddrModule.io.waddr(w) := enqIndex
      vaddrModule.io.wdata(w) := enq.bits.vaddr
      debug_vaddr(enqIndex)   := enq.bits.vaddr

      /**
       * used for feedback and replay
       */
      // set flags
      val replayInfo = enq.bits.rep_info
      val dataInLastBeat = replayInfo.last_beat
      cause(enqIndex) := replayInfo.cause.asUInt


      // init
      blocking(enqIndex)     := true.B

      // update blocking pointer
      when (replayInfo.cause(LoadReplayCauses.C_BC) ||
            replayInfo.cause(LoadReplayCauses.C_NK) ||
            replayInfo.cause(LoadReplayCauses.C_DR) ||
            replayInfo.cause(LoadReplayCauses.C_WF)) {
        // normal case: bank conflict or schedule error or dcache replay
        // can replay next cycle
        blocking(enqIndex) := false.B
      }

      // special case: tlb miss
      when (replayInfo.cause(LoadReplayCauses.C_TM)) {
        blocking(enqIndex) := !replayInfo.tlb_full &&
          !(tlbHintResp.valid && (tlbHintResp.bits.id === replayInfo.tlb_id || tlbHintResp.bits.replay_all))
        when (fastTlbHintResp.valid && fastTlbHintResp.bits.id === replayInfo.tlb_id) {
          blocking(enqIndex) := false.B
        }
        tlbHintId(enqIndex) := replayInfo.tlb_id
      }

      // special case: dcache miss
      when (replayInfo.cause(LoadReplayCauses.C_DM) && enq.bits.handledByMSHR) {
        val tlDHitThisCycle = tlDChannelHit(replayInfo.mshr_id)
        val tlDHitPrevCycle = tlDChannelHitPrev(replayInfo.mshr_id)
        blocking(enqIndex) := !replayInfo.full_fwd && //  dcache miss
                              !tlDHitThisCycle && // no refill in this cycle
                              !tlDHitPrevCycle // not refill in last cycle
      }

      when (isFF) {
        blocking(enqIndex) := !ffProducerReadyNow
      }

      // extra info
      replayCarryReg(enqIndex) := replayInfo.rep_carry
      replacementUpdated(enqIndex) := enq.bits.replacementUpdated
      missDbUpdated(enqIndex) := enq.bits.missDbUpdated
      // update mshr_id only when the load has already been handled by mshr
      when(enq.bits.handledByMSHR) {
        missMSHRId(enqIndex) := replayInfo.mshr_id
      }
      dataInLastBeatReg(enqIndex) := dataInLastBeat
      //dataInLastBeatReg(enqIndex) := Mux(io.l2_hint.bits.isKeyword, !dataInLastBeat, dataInLastBeat)
    }

    //
    val schedIndex = enq.bits.schedIndex
    when (enq.valid && enq.bits.isLoadReplay) {
      when (!needReplay(w)) {
        allocated(schedIndex) := false.B
        freeMaskVec(schedIndex) := true.B
      } .otherwise {
        scheduled(schedIndex) := false.B
      }
    }
  }

  // misprediction recovery / exception redirect
  for (i <- 0 until LoadQueueReplaySize) {
    needCancel(i) := uop(i).robIdx.needFlush(io.redirect) && allocated(i)
    when (needCancel(i)) {
      allocated(i) := false.B
      freeMaskVec(i) := true.B
    }
  }

  freeList.io.free := freeMaskVec.asUInt

  io.lqFull := lqFull

  // Topdown
  val robHeadVaddr = io.debugTopDown.robHeadVaddr

  val uop_wrapper = Wire(Vec(LoadQueueReplaySize, new XSBundleWithMicroOp))
  (uop_wrapper.zipWithIndex).foreach {
    case (u, i) => {
      u.uop := uop(i)
    }
  }
  val lq_match_vec = (debug_vaddr.zip(allocated)).map{case(va, alloc) => alloc && (va === robHeadVaddr.bits)}
  val rob_head_lq_match = ParallelOperation(lq_match_vec.zip(uop_wrapper), (a: Tuple2[Bool, XSBundleWithMicroOp], b: Tuple2[Bool, XSBundleWithMicroOp]) => {
    val (a_v, a_uop) = (a._1, a._2)
    val (b_v, b_uop) = (b._1, b._2)

    val res = Mux(a_v && b_v, Mux(isAfter(a_uop.uop.robIdx, b_uop.uop.robIdx), b_uop, a_uop),
                  Mux(a_v, a_uop,
                      Mux(b_v, b_uop,
                                a_uop)))
    (a_v || b_v, res)
  })

  val lq_match_bits = rob_head_lq_match._2.uop
  val lq_match      = rob_head_lq_match._1 && robHeadVaddr.valid
  val lq_match_idx  = lq_match_bits.lqIdx.value

  val rob_head_tlb_miss        = lq_match && cause(lq_match_idx)(LoadReplayCauses.C_TM)
  val rob_head_nuke            = lq_match && cause(lq_match_idx)(LoadReplayCauses.C_NK)
  val rob_head_mem_amb         = lq_match && cause(lq_match_idx)(LoadReplayCauses.C_MA)
  val rob_head_confilct_replay = lq_match && cause(lq_match_idx)(LoadReplayCauses.C_BC)
  val rob_head_forward_fail    = lq_match && cause(lq_match_idx)(LoadReplayCauses.C_FF)
  val rob_head_mshrfull_replay = lq_match && cause(lq_match_idx)(LoadReplayCauses.C_DR)
  val rob_head_rar_nack        = lq_match && cause(lq_match_idx)(LoadReplayCauses.C_RAR)
  val rob_head_raw_nack        = lq_match && cause(lq_match_idx)(LoadReplayCauses.C_RAW)
  val rob_head_other_replay    = lq_match && (rob_head_rar_nack || rob_head_raw_nack || rob_head_forward_fail)

  val rob_head_vio_replay = rob_head_nuke || rob_head_mem_amb

  val rob_head_miss_in_dtlb = io.debugTopDown.robHeadMissInDTlb
  io.debugTopDown.robHeadTlbReplay := rob_head_tlb_miss && !rob_head_miss_in_dtlb
  io.debugTopDown.robHeadTlbMiss := rob_head_tlb_miss && rob_head_miss_in_dtlb
  io.debugTopDown.robHeadLoadVio := rob_head_vio_replay
  io.debugTopDown.robHeadLoadMSHR := rob_head_mshrfull_replay
  io.debugTopDown.robHeadOtherReplay := rob_head_other_replay
  io.replayAllocate := allocated.asUInt.orR
  val perfValidCount = RegNext(PopCount(allocated))

  //  perf cnt
  val normalRelease = VecInit(io.enq.zipWithIndex.map { case (enq, w) =>
    val schedIndex = enq.bits.schedIndex
    enq.fire && !cancelEnq(w) && enq.bits.isLoadReplay && !needReplay(w) && allocated(schedIndex)
  })
  // Entry-level release mask. Besides aggregating releases across load pipelines, it keeps
  // the release cycle out of both the residence timer and the ROB-head occupancy count.
  val normalReleaseOH = VecInit((0 until LoadQueueReplaySize).map { entryIdx =>
    VecInit(io.enq.zip(normalRelease).map { case (enq, release) =>
      release && enq.bits.schedIndex === entryIdx.U
    }).asUInt.orR
  })
  val newAllocate = VecInit(newEnqueue.zip(io.enq).map { case (newEnq, enq) =>
    newEnq && enq.fire
  })
  val newAllocateOH = VecInit((0 until LoadQueueReplaySize).map { entryIdx =>
    VecInit(newAllocate.zip(enqIndexOH).map { case (allocate, indexOH) =>
      allocate && indexOH(entryIdx)
    }).asUInt.orR
  })

  val replayResidenceMax = ((1 << replayResidenceWidth) - 1).U(replayResidenceWidth.W)
  // Reset only for a newly allocated entry. Redirect/cancel stops the timer without creating
  // a latency sample, while a live entry saturates at replayResidenceMax instead of wrapping.
  replayResidenceCycles.zipWithIndex.foreach { case (timer, entryIdx) =>
    when (newAllocateOH(entryIdx)) {
      timer := 0.U
    }.elsewhen (allocated(entryIdx) && !normalReleaseOH(entryIdx) && !needCancel(entryIdx)) {
      when (timer =/= replayResidenceMax) {
        timer := timer + 1.U
      }
    }
  }

  val releaseTimers = VecInit(io.enq.map(enq => replayResidenceCycles(enq.bits.schedIndex)))
  // The timer contains completed cycles before the current cycle, so add the normal-release
  // cycle to obtain T_release - T_enq. If the timer already reached the maximum, the exact
  // latency is larger than the representable value and this release sample is an overflow.
  val releaseLatencies = VecInit(releaseTimers.map { timer =>
    Mux(timer === replayResidenceMax, replayResidenceMax, timer + 1.U)
  })
  // Attribute each normal release to the entry's final one-hot cause. The per-cause reductions
  // preserve multiple releases in the same cycle by summing across all load pipelines.
  val releaseCauseEvents = (0 until LoadReplayCauses.allCauses).map { causeIdx =>
    VecInit(io.enq.zipWithIndex.map { case (enq, w) =>
      normalRelease(w) && cause(enq.bits.schedIndex)(causeIdx)
    })
  }
  val releaseCauseCounts = releaseCauseEvents.map(PopCount(_))
  val releaseCauseLatencies = releaseCauseEvents.map { events =>
    events.zip(releaseLatencies).map { case (event, latency) =>
      Mux(event, latency, 0.U(replayResidenceWidth.W))
    }.reduce(_ +& _)
  }
  // Count released samples whose exact latency exceeded replayResidenceMax. This is a sample
  // count, not the number of cycles beyond the representable range.
  val releaseCauseOverflowCounts = releaseCauseEvents.map { events =>
    PopCount(events.zip(releaseTimers).map { case (event, timer) =>
      event && timer === replayResidenceMax
    })
  }

  val scalarRobHeadRelease = VecInit(io.enq.zipWithIndex.map { case (enq, w) =>
    val schedIndex = enq.bits.schedIndex
    normalRelease(w) && !vecReplay(schedIndex).isvec && uop(schedIndex).robIdx === io.robHeadPtr
  })
  val robHeadReleaseCauseEvents = (0 until LoadReplayCauses.allCauses).map { causeIdx =>
    VecInit(io.enq.zipWithIndex.map { case (enq, w) =>
      val schedIndex = enq.bits.schedIndex
      scalarRobHeadRelease(w) && cause(schedIndex)(causeIdx)
    })
  }
  val robHeadReleaseCauseCounts = robHeadReleaseCauseEvents.map(PopCount(_))
  // Count unreleased scalar ROB-head occupancy using the current cause. Cancel and normal-release
  // cycles are excluded. The OR reduction limits each cause to at most one increment per cycle.
  val scalarUnreleasedRobHeadEntries = VecInit((0 until LoadQueueReplaySize).map { entryIdx =>
    allocated(entryIdx) &&
      !vecReplay(entryIdx).isvec &&
      uop(entryIdx).robIdx === io.robHeadPtr &&
      !needCancel(entryIdx) &&
      !normalReleaseOH(entryIdx)
  })
  val scalarUnreleasedRobHeadByCause = (0 until LoadReplayCauses.allCauses).map { causeIdx =>
    VecInit((0 until LoadQueueReplaySize).map { entryIdx =>
      scalarUnreleasedRobHeadEntries(entryIdx) && cause(entryIdx)(causeIdx)
    }).asUInt.orR
  }

  io.enq.zipWithIndex.foreach { case (enq, w) =>
    val schedIndex = enq.bits.schedIndex
    when (normalRelease(w)) {
      assert(PopCount(cause(schedIndex)) === 1.U, "released replay entry cause must be one-hot")
    }
  }
  for (i <- 0 until LoadPipelineWidth; j <- i + 1 until LoadPipelineWidth) {
    assert(!(normalRelease(i) && normalRelease(j) &&
      io.enq(i).bits.schedIndex === io.enq(j).bits.schedIndex),
      "a replay entry must not be released by multiple ports")
  }
  assert(robHeadReleaseCauseCounts.reduce(_ +& _) === PopCount(scalarRobHeadRelease),
    "replay cause increments must equal scalar ROB-head releases")
  XSError(releaseCauseCounts.reduce(_ +& _) =/= PopCount(normalRelease),
    "release cause increments must equal normal replay releases\n")
  XSError(PopCount(normalReleaseOH) =/= PopCount(normalRelease),
    "normal release entry mask must preserve the release count\n")
  XSError(releaseCauseOverflowCounts.reduce(_ +& _) > PopCount(normalRelease),
    "overflow release samples must not exceed normal replay releases\n")

  // event counters
  LoadReplayCauses.perfCauseNames.zip(robHeadReleaseCauseCounts).foreach { case (causeName, count) =>
    XSPerfAccumulate(s"replay_${causeName}_scalar_release_rob_head", count)
  }

  // slot counters
  // Offline average residence latency is release_latency_cycles / release_count. Overflow count
  // reports how many released samples exceeded the representable latency.
  LoadReplayCauses.perfCauseNames.zipWithIndex.foreach { case (causeName, causeIdx) =>
    XSPerfAccumulate(s"replay_${causeName}_release_latency_cycles", releaseCauseLatencies(causeIdx))
    XSPerfAccumulate(s"replay_${causeName}_release_count", releaseCauseCounts(causeIdx))
    XSPerfAccumulate(s"replay_${causeName}_release_latency_overflow_count", releaseCauseOverflowCounts(causeIdx))
    XSPerfAccumulate(s"replay_${causeName}_scalar_rob_head_blocked_cycles", scalarUnreleasedRobHeadByCause(causeIdx))
  }

  val enqNumber               = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay))
  val deqNumber               = PopCount(io.replay.map(_.fire))
  val deqBlockCount           = PopCount(io.replay.map(r => r.valid && !r.ready))
  val replayTlbMissCount      = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.rep_info.cause(LoadReplayCauses.C_TM)))
  val replayMemAmbCount       = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.rep_info.cause(LoadReplayCauses.C_MA)))
  val replayNukeCount         = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.rep_info.cause(LoadReplayCauses.C_NK)))
  val replayRARRejectCount    = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.rep_info.cause(LoadReplayCauses.C_RAR)))
  val replayRAWRejectCount    = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.rep_info.cause(LoadReplayCauses.C_RAW)))
  val replayBankConflictCount = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.rep_info.cause(LoadReplayCauses.C_BC)))
  val replayDCacheReplayCount = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.rep_info.cause(LoadReplayCauses.C_DR)))
  val replayForwardFailCount  = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.rep_info.cause(LoadReplayCauses.C_FF)))
  val replayDCacheMissCount   = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.rep_info.cause(LoadReplayCauses.C_DM)))
  val replayMultiMatchCount   = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.rep_info.cause(LoadReplayCauses.C_SMF)))
  val replayForwardFailEnqNonblockingCount = PopCount(ffEnqNonblocking)
  val replayStoreAddrWakeupCancelCount = PopCount(storeAddrWakeupCancelVec)
  val replayStoreDataWakeupCancelCount = PopCount(storeDataWakeupCancelVec)
  def storeIssueScoreBoardDelay3(idx: UInt): Bool = {
    VecInit(storeIssueScoreBoard.map(scoreBoard =>
      VecInit(scoreBoard.map(_(2))).asUInt.orR
    ))(idx)
  }
  val replayStoreAddrWakeupDelay3FireCount = PopCount(io.replay.map(r =>
    r.fire && r.bits.cause.get(LoadReplayCauses.C_MA) &&
      storeIssueScoreBoardDelay3(r.bits.replayQueueIdx.get)
  ))
  val replayStoreDataWakeupDelay3FireCount = PopCount(io.replay.map(r =>
    r.fire && r.bits.cause.get(LoadReplayCauses.C_FF) &&
      storeIssueScoreBoardDelay3(r.bits.replayQueueIdx.get)
  ))
  XSPerfAccumulate("enq", enqNumber)
  XSPerfAccumulate("deq", deqNumber)
  XSPerfAccumulate("deq_block", deqBlockCount)
  XSPerfAccumulate("replay_full", io.lqFull)
  XSPerfAccumulate("replay_rar_nack", replayRARRejectCount)
  XSPerfAccumulate("replay_raw_nack", replayRAWRejectCount)
  XSPerfAccumulate("replay_nuke", replayNukeCount)
  XSPerfAccumulate("replay_mem_amb", replayMemAmbCount)
  XSPerfAccumulate("replay_tlb_miss", replayTlbMissCount)
  XSPerfAccumulate("replay_bank_conflict", replayBankConflictCount)
  XSPerfAccumulate("replay_dcache_replay", replayDCacheReplayCount)
  XSPerfAccumulate("replay_forward_fail", replayForwardFailCount)
  XSPerfAccumulate("replay_forward_fail_enq_nonblocking", replayForwardFailEnqNonblockingCount)
  XSPerfAccumulate("replay_dcache_miss", replayDCacheMissCount)
  XSPerfAccumulate("replay_hint_wakeup", s0_hintSelValid)
  XSPerfAccumulate("replay_hint_priority_beat1", PopCount(VecInit(l2Hint.map(hint => hint.valid && hint.bits.isKeyword))))
  XSPerfAccumulate("replay_storeQueue_multi_match", replayMultiMatchCount)
  XSPerfAccumulate("replay_store_addr_wakeup", storeAddrWakeupCount)
  XSPerfAccumulate("replay_store_data_wakeup", storeDataWakeupCount)
  XSPerfAccumulate("replay_store_addr_wakeup_cancel", replayStoreAddrWakeupCancelCount)
  XSPerfAccumulate("replay_store_data_wakeup_cancel", replayStoreDataWakeupCancelCount)
  XSPerfAccumulate("replay_store_addr_wakeup_delay3_fire", replayStoreAddrWakeupDelay3FireCount)
  XSPerfAccumulate("replay_store_data_wakeup_delay3_fire", replayStoreDataWakeupDelay3FireCount)
  XSPerfAccumulate("replay_allocate", io.replayAllocate)

  // replay counter
  val perfReplayCounter = RegInit(VecInit(Seq.fill(LoadQueueReplaySize)(0.U(8.W))))
  for((enq, i) <- io.enq.zipWithIndex){
    //  Allocated ready
    val offset = PopCount(newEnqueue.take(i))
    val enqIndex = freeList.io.allocateSlot(offset)

    when(newEnqueue(i) && enq.ready) { // first enqueue
      perfReplayCounter(enqIndex) := 1.U
    }

    val schedIndex = enq.bits.schedIndex
    when (enq.valid && enq.bits.isLoadReplay && needReplay(i)) { // re-relpay
       perfReplayCounter(schedIndex) := perfReplayCounter(schedIndex) + 1.U
    }

    val enable = enq.valid && enq.bits.isLoadReplay && !needReplay(i) && allocated(schedIndex)
    val replayCounter = LookupTree(schedIndex, perfReplayCounter.zipWithIndex.map{case (d, v) => (v.U, d)})
    XSPerfHistogram(s"load_replay_count_${i}", replayCounter, enable, 1, 16, 1)
  }

  // count the number of each cause replay over 4 times.
  val replayTlbMissOver4Count      = PopCount(io.enq.map(enq => enq.valid && enq.bits.isLoadReplay && !enq.bits.rep_info.need_rep && (perfReplayCounter(enq.bits.schedIndex) > 4.U) && cause(enq.bits.schedIndex)(LoadReplayCauses.C_TM)))
  val replayMemAmbOver4Count       = PopCount(io.enq.map(enq => enq.valid && enq.bits.isLoadReplay && !enq.bits.rep_info.need_rep && (perfReplayCounter(enq.bits.schedIndex) > 4.U) && cause(enq.bits.schedIndex)(LoadReplayCauses.C_MA)))
  val replayNukeOver4Count         = PopCount(io.enq.map(enq => enq.valid && enq.bits.isLoadReplay && !enq.bits.rep_info.need_rep && (perfReplayCounter(enq.bits.schedIndex) > 4.U) && cause(enq.bits.schedIndex)(LoadReplayCauses.C_NK)))
  val replayRARRejectOver4Count    = PopCount(io.enq.map(enq => enq.valid && enq.bits.isLoadReplay && !enq.bits.rep_info.need_rep && (perfReplayCounter(enq.bits.schedIndex) > 4.U) && cause(enq.bits.schedIndex)(LoadReplayCauses.C_RAR)))
  val replayRAWRejectOver4Count    = PopCount(io.enq.map(enq => enq.valid && enq.bits.isLoadReplay && !enq.bits.rep_info.need_rep && (perfReplayCounter(enq.bits.schedIndex) > 4.U) && cause(enq.bits.schedIndex)(LoadReplayCauses.C_RAW)))
  val replayBankConflictOver4Count = PopCount(io.enq.map(enq => enq.valid && enq.bits.isLoadReplay && !enq.bits.rep_info.need_rep && (perfReplayCounter(enq.bits.schedIndex) > 4.U) && cause(enq.bits.schedIndex)(LoadReplayCauses.C_BC)))
  val replayDCacheReplayOver4Count = PopCount(io.enq.map(enq => enq.valid && enq.bits.isLoadReplay && !enq.bits.rep_info.need_rep && (perfReplayCounter(enq.bits.schedIndex) > 4.U) && cause(enq.bits.schedIndex)(LoadReplayCauses.C_DR)))
  val replayForwardFailOver4Count  = PopCount(io.enq.map(enq => enq.valid && enq.bits.isLoadReplay && !enq.bits.rep_info.need_rep && (perfReplayCounter(enq.bits.schedIndex) > 4.U) && cause(enq.bits.schedIndex)(LoadReplayCauses.C_FF)))
  val replayDCacheMissOver4Count   = PopCount(io.enq.map(enq => enq.valid && enq.bits.isLoadReplay && !enq.bits.rep_info.need_rep && (perfReplayCounter(enq.bits.schedIndex) > 4.U) && cause(enq.bits.schedIndex)(LoadReplayCauses.C_DM)))
  val replayMultiMatchOver4Count   = PopCount(io.enq.map(enq => enq.valid && enq.bits.isLoadReplay && !enq.bits.rep_info.need_rep && (perfReplayCounter(enq.bits.schedIndex) > 4.U) && cause(enq.bits.schedIndex)(LoadReplayCauses.C_SMF)))

  XSPerfAccumulate("replay_rar_nack_over4_times", replayRARRejectOver4Count)
  XSPerfAccumulate("replay_raw_nack_over4_times", replayRAWRejectOver4Count)
  XSPerfAccumulate("replay_nuke_over4_times", replayNukeOver4Count)
  XSPerfAccumulate("replay_mem_amb_over4_times", replayMemAmbOver4Count)
  XSPerfAccumulate("replay_tlb_miss_over4_times", replayTlbMissOver4Count)
  XSPerfAccumulate("replay_bank_conflict_over4_times", replayBankConflictOver4Count)
  XSPerfAccumulate("replay_dcache_replay_over4_times", replayDCacheReplayOver4Count)
  XSPerfAccumulate("replay_forward_fail_over4_times", replayForwardFailOver4Count)
  XSPerfAccumulate("replay_dcache_miss_over4_times", replayDCacheMissOver4Count)
  XSPerfAccumulate("replay_storeQueue_multi_match_over4_times", replayMultiMatchOver4Count)


  val perfEvents: Seq[(String, UInt)] = Seq(
    ("enq", enqNumber),
    ("deq", deqNumber),
    ("deq_block", deqBlockCount),
    ("replay_full", io.lqFull),
    ("replay_rar_nack", replayRARRejectCount),
    ("replay_raw_nack", replayRAWRejectCount),
    ("replay_nuke", replayNukeCount),
    ("replay_mem_amb", replayMemAmbCount),
    ("replay_tlb_miss", replayTlbMissCount),
    ("replay_bank_conflict", replayBankConflictCount),
    ("replay_dcache_replay", replayDCacheReplayCount),
    ("replay_forward_fail", replayForwardFailCount),
    ("replay_dcache_miss", replayDCacheMissCount),
  )
  generatePerfEvent()
  // end
}
