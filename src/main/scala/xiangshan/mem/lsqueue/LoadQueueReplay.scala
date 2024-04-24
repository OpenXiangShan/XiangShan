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

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xiangshan._
import xiangshan.backend.rob.{RobLsqIO, RobPtr}
import xiangshan.cache._
import xiangshan.backend.fu.fpu.FPU
import xiangshan.backend.fu.FuConfig._
import xiangshan.cache._
import xiangshan.cache.mmu._
import xiangshan.frontend.FtqPtr
import xiangshan.ExceptionNO._
import xiangshan.cache.wpu.ReplayCarry
import xiangshan.mem.mdp._
import utils._
import utility._
import xiangshan.backend.Bundles.{DynInst, MemExuOutput}

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
  // st-ld violation re-execute check
  val C_MA  = 0
  // tlb miss check
  val C_TM  = 1
  // store-to-load-forwarding check
  val C_FF  = 2
  // dcache replay check
  val C_DR  = 3
  // dcache miss check
  val C_DM  = 4
  // wpu predict fail
  val C_WF  = 5
  // dcache bank conflict check
  val C_BC  = 6
  // RAR queue accept check
  val C_RAR = 7
  // RAW queue accept check
  val C_RAW = 8
  // st-ld violation
  val C_NK  = 9
  // total causes
  val allCauses = 10
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


class LoadQueueReplay(implicit p: Parameters) extends XSModule
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasTlbConst
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    // control
    val redirect = Flipped(ValidIO(new Redirect))

    // from load unit s3
    val enq = Vec(LoadPipelineWidth, Flipped(Decoupled(new LqWriteBundle)))

    // from sta s1
    val storeAddrIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))

    // from std s1
    val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new MemExuOutput)))

    // queue-based replay
    val replay = Vec(LoadPipelineWidth, Decoupled(new LsPipelineBundle))
   // val refill = Flipped(ValidIO(new Refill))
    val tl_d_channel = Input(new DcacheToLduForwardIO)

    // from StoreQueue
    val stAddrReadySqPtr = Input(new SqPtr)
    val stAddrReadyVec   = Input(Vec(StoreQueueSize, Bool()))
    val stDataReadySqPtr = Input(new SqPtr)
    val stDataReadyVec   = Input(Vec(StoreQueueSize, Bool()))

    //
    val sqEmpty = Input(Bool())
    val lqFull  = Output(Bool())
    val ldWbPtr = Input(new LqPtr)
    val rarFull = Input(Bool())
    val rawFull = Input(Bool())
    val l2_hint  = Input(Valid(new L2ToL1Hint()))
    val tlb_hint = Flipped(new TlbHintIO)
    val tlbReplayDelayCycleCtrl = Vec(4, Input(UInt(ReSelectLen.W)))

    val debugTopDown = new LoadQueueTopDownIO
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
  val vaddrModule = Module(new LqVAddrModule(
    gen = UInt(VAddrBits.W),
    numEntries = LoadQueueReplaySize,
    numRead = LoadPipelineWidth,
    numWrite = LoadPipelineWidth,
    numWBank = LoadQueueNWriteBanks,
    numWDelay = 2,
    numCamPort = 0))
  vaddrModule.io := DontCare
  val debug_vaddr = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(VAddrBits.W))))
  val cause = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(LoadReplayCauses.allCauses.W))))
  val blocking = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  val strict = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))

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

  /**
   * Enqueue
   */
  val canEnqueue = io.enq.map(_.valid)
  val cancelEnq = io.enq.map(enq => enq.bits.uop.robIdx.needFlush(io.redirect))
  val needReplay = io.enq.map(enq => enq.bits.rep_info.need_rep)
  val hasExceptions = io.enq.map(enq => ExceptionNO.selectByFu(enq.bits.uop.exceptionVec, LduCfg).asUInt.orR && !enq.bits.tlbMiss)
  val loadReplay = io.enq.map(enq => enq.bits.isLoadReplay)
  val needEnqueue = VecInit((0 until LoadPipelineWidth).map(w => {
    canEnqueue(w) && !cancelEnq(w) && needReplay(w) && !hasExceptions(w)
  }))
  val canFreeVec = VecInit((0 until LoadPipelineWidth).map(w => {
    canEnqueue(w) && loadReplay(w) && (!needReplay(w) || hasExceptions(w))
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

  // store data valid check
  val stAddrReadyVec = io.stAddrReadyVec
  val stDataReadyVec = io.stDataReadyVec

  for (i <- 0 until LoadQueueReplaySize) {
    // dequeue
    //  FIXME: store*Ptr is not accurate
    dataNotBlockVec(i) := isAfter(io.stDataReadySqPtr, blockSqIdx(i)) || stDataReadyVec(blockSqIdx(i).value) || io.sqEmpty // for better timing
    addrNotBlockVec(i) := Mux(strict(i), isAfter(io.stAddrReadySqPtr, blockSqIdx(i)), stAddrReadyVec(blockSqIdx(i).value)) || io.sqEmpty // for better timing

    // store address execute
    storeAddrInSameCycleVec(i) := VecInit((0 until StorePipelineWidth).map(w => {
      io.storeAddrIn(w).valid &&
      !io.storeAddrIn(w).bits.miss &&
      blockSqIdx(i) === io.storeAddrIn(w).bits.uop.sqIdx
    })).asUInt.orR // for better timing

    // store data execute
    storeDataInSameCycleVec(i) := VecInit((0 until StorePipelineWidth).map(w => {
      io.storeDataIn(w).valid &&
      blockSqIdx(i) === io.storeDataIn(w).bits.uop.sqIdx
    })).asUInt.orR // for better timing

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

  // update blocking condition
  (0 until LoadQueueReplaySize).map(i => {
    // case C_MA
    when (cause(i)(LoadReplayCauses.C_MA)) {
      blocking(i) := Mux(stAddrDeqVec(i), false.B, blocking(i))
    }
    // case C_TM
    when (cause(i)(LoadReplayCauses.C_TM)) {
      blocking(i) := Mux(io.tlb_hint.resp.valid &&
                     (io.tlb_hint.resp.bits.replay_all ||
                     io.tlb_hint.resp.bits.id === tlbHintId(i)), false.B, blocking(i))
    }
    // case C_FF
    when (cause(i)(LoadReplayCauses.C_FF)) {
      blocking(i) := Mux(stDataDeqVec(i), false.B, blocking(i))
    }
    // case C_DM
    when (cause(i)(LoadReplayCauses.C_DM)) {
      blocking(i) := Mux(io.tl_d_channel.valid && io.tl_d_channel.mshrid === missMSHRId(i), false.B, blocking(i))
    }
    // case C_RAR
    when (cause(i)(LoadReplayCauses.C_RAR)) {
      blocking(i) := Mux((!io.rarFull || !isAfter(uop(i).lqIdx, io.ldWbPtr)), false.B, blocking(i))
    }
    // case C_RAW
    when (cause(i)(LoadReplayCauses.C_RAW)) {
      blocking(i) := Mux((!io.rawFull || !isAfter(uop(i).sqIdx, io.stAddrReadySqPtr)), false.B, blocking(i))
    }
  })

  //  Replay is splitted into 3 stages
  require((LoadQueueReplaySize % LoadPipelineWidth) == 0)
  def getRemBits(input: UInt)(rem: Int): UInt = {
    VecInit((0 until LoadQueueReplaySize / LoadPipelineWidth).map(i => { input(LoadPipelineWidth * i + rem) })).asUInt
  }

  def getRemSeq(input: Seq[Seq[Bool]])(rem: Int) = {
    (0 until LoadQueueReplaySize / LoadPipelineWidth).map(i => { input(LoadPipelineWidth * i + rem) })
  }

  // stage1: select 2 entries and read their vaddr
  val s0_oldestSel = Wire(Vec(LoadPipelineWidth, Valid(UInt(LoadQueueReplaySize.W))))
  val s1_can_go = Wire(Vec(LoadPipelineWidth, Bool()))
  val s1_oldestSel = Wire(Vec(LoadPipelineWidth, Valid(UInt(log2Up(LoadQueueReplaySize + 1).W))))
  val s2_can_go = Wire(Vec(LoadPipelineWidth, Bool()))
  val s2_oldestSel = Wire(Vec(LoadPipelineWidth, Valid(UInt(log2Up(LoadQueueReplaySize + 1).W))))

  // generate mask
  val needCancel = Wire(Vec(LoadQueueReplaySize, Bool()))
  // generate enq mask
  val enqIndexOH = Wire(Vec(LoadPipelineWidth, UInt(LoadQueueReplaySize.W)))
  val s0_loadEnqFireMask = io.enq.map(x => x.fire && !x.bits.isLoadReplay).zip(enqIndexOH).map(x => Mux(x._1, x._2, 0.U))
  val s0_remLoadEnqFireVec = s0_loadEnqFireMask.map(x => VecInit((0 until LoadPipelineWidth).map(rem => getRemBits(x)(rem))))
  val s0_remEnqSelVec = Seq.tabulate(LoadPipelineWidth)(w => VecInit(s0_remLoadEnqFireVec.map(x => x(w))))

  // generate free mask
  val s0_loadFreeSelMask = RegNext(needCancel.asUInt)
  val s0_remFreeSelVec = VecInit(Seq.tabulate(LoadPipelineWidth)(rem => getRemBits(s0_loadFreeSelMask)(rem)))

  // l2 hint wakes up cache missed load
  // l2 will send GrantData in next 2/3 cycle, wake up the missed load early and sent them to load pipe, so them will hit the data in D channel or mshr in load S1
  val s0_loadHintWakeMask = VecInit((0 until LoadQueueReplaySize).map(i => {
    allocated(i) && !scheduled(i) && cause(i)(LoadReplayCauses.C_DM) && blocking(i) && missMSHRId(i) === io.l2_hint.bits.sourceId && io.l2_hint.valid
  })).asUInt
  // l2 will send 2 beats data in 2 cycles, so if data needed by this load is in first beat, select it this cycle, otherwise next cycle
  // when isKeyword = 1, s0_loadHintSelMask need overturn
    val s0_loadHintSelMask = Mux(
     io.l2_hint.bits.isKeyword,
     s0_loadHintWakeMask & dataInLastBeatReg.asUInt,
     s0_loadHintWakeMask & VecInit(dataInLastBeatReg.map(!_)).asUInt
     )
  val s0_remLoadHintSelMask = VecInit((0 until LoadPipelineWidth).map(rem => getRemBits(s0_loadHintSelMask)(rem)))
  val s0_remHintSelValidVec = VecInit((0 until LoadPipelineWidth).map(rem => ParallelORR(s0_remLoadHintSelMask(rem))))
  val s0_hintSelValid = ParallelORR(s0_loadHintSelMask)

  // wake up cache missed load
  (0 until LoadQueueReplaySize).foreach(i => {
    when(s0_loadHintWakeMask(i)) {
      blocking(i) := false.B
    }
  })

  // generate replay mask
  // replay select priority is given as follow
  // 1. hint wake up load
  // 2. higher priority load
  // 3. lower priority load
  val s0_loadHigherPriorityReplaySelMask = VecInit((0 until LoadQueueReplaySize).map(i => {
    val hasHigherPriority = cause(i)(LoadReplayCauses.C_DM) || cause(i)(LoadReplayCauses.C_FF)
    allocated(i) && !scheduled(i) && !blocking(i) && hasHigherPriority
  })).asUInt // use uint instead vec to reduce verilog lines
  val s0_remLoadHigherPriorityReplaySelMask = VecInit((0 until LoadPipelineWidth).map(rem => getRemBits(s0_loadHigherPriorityReplaySelMask)(rem)))
  val s0_loadLowerPriorityReplaySelMask = VecInit((0 until LoadQueueReplaySize).map(i => {
    val hasLowerPriority = !cause(i)(LoadReplayCauses.C_DM) && !cause(i)(LoadReplayCauses.C_FF)
    allocated(i) && !scheduled(i) && !blocking(i) && hasLowerPriority
  })).asUInt // use uint instead vec to reduce verilog lines
  val s0_remLoadLowerPriorityReplaySelMask = VecInit((0 until LoadPipelineWidth).map(rem => getRemBits(s0_loadLowerPriorityReplaySelMask)(rem)))
  val s0_loadNormalReplaySelMask = s0_loadLowerPriorityReplaySelMask | s0_loadHigherPriorityReplaySelMask | s0_loadHintSelMask
  val s0_remNormalReplaySelVec = VecInit((0 until LoadPipelineWidth).map(rem => s0_remLoadLowerPriorityReplaySelMask(rem) | s0_remLoadHigherPriorityReplaySelMask(rem) | s0_remLoadHintSelMask(rem)))
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
  val s0_remOldsetMatchMaskVec = (0 until LoadPipelineWidth).map(rem => getRemSeq(s0_oldestMatchMaskVec.map(_.take(1)))(rem))
  val s0_remOlderMatchMaskVec = (0 until LoadPipelineWidth).map(rem => getRemSeq(s0_oldestMatchMaskVec.map(_.drop(1)))(rem))
  val s0_remOldestSelVec = VecInit(Seq.tabulate(LoadPipelineWidth)(rem => {
    VecInit((0 until LoadQueueReplaySize / LoadPipelineWidth).map(i => {
      Mux(ParallelORR(s0_remOldsetMatchMaskVec(rem).map(_(0))), s0_remOldsetMatchMaskVec(rem)(i)(0), s0_remOlderMatchMaskVec(rem)(i).reduce(_|_))
    })).asUInt
  }))
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
    val l2HintFirst = io.l2_hint.valid && ParallelORR(s0_remOldestHintSelVec(rport))
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

    oldest.valid := ageOldest.valid || issOldestValid
    oldest.bits := oldestBitsVec.asUInt
    oldest
  }))

  // stage2: send replay request to load unit
  // replay cold down
  val ColdDownCycles = 16
  val coldCounter = RegInit(VecInit(List.fill(LoadPipelineWidth)(0.U(log2Up(ColdDownCycles).W))))
  val ColdDownThreshold = Wire(UInt(log2Up(ColdDownCycles).W))
  ColdDownThreshold := Constantin.createRecord("ColdDownThreshold_"+p(XSCoreParamsKey).HartId.toString(), initValue = 12.U)
  assert(ColdDownCycles.U > ColdDownThreshold, "ColdDownCycles must great than ColdDownThreshold!")

  def replayCanFire(i: Int) = coldCounter(i) >= 0.U && coldCounter(i) < ColdDownThreshold
  def coldDownNow(i: Int) = coldCounter(i) >= ColdDownThreshold

  val replay_req = Wire(Vec(LoadPipelineWidth, DecoupledIO(new LsPipelineBundle)))

  for (i <- 0 until LoadPipelineWidth) {
    val s0_can_go = s1_can_go(i) ||
                    uop(s1_oldestSel(i).bits).robIdx.needFlush(io.redirect) ||
                    uop(s1_oldestSel(i).bits).robIdx.needFlush(RegNext(io.redirect))
    val s0_oldestSelIndexOH = s0_oldestSel(i).bits // one-hot
    s1_oldestSel(i).valid := RegEnable(s0_oldestSel(i).valid, false.B, s0_can_go)
    s1_oldestSel(i).bits := RegEnable(OHToUInt(s0_oldestSel(i).bits), s0_can_go)

    for (j <- 0 until LoadQueueReplaySize) {
      when (s0_can_go && s0_oldestSel(i).valid && s0_oldestSelIndexOH(j)) {
        scheduled(j) := true.B
      }
    }
  }
  val s2_cancelReplay = Wire(Vec(LoadPipelineWidth, Bool()))
  for (i <- 0 until LoadPipelineWidth) {
    val s1_cancel = uop(s1_oldestSel(i).bits).robIdx.needFlush(io.redirect) ||
                    uop(s1_oldestSel(i).bits).robIdx.needFlush(RegNext(io.redirect))
    val s1_oldestSelV = s1_oldestSel(i).valid && !s1_cancel
    s1_can_go(i)          := replayCanFire(i) && (!s2_oldestSel(i).valid || replay_req(i).fire) || s2_cancelReplay(i)
    s2_oldestSel(i).valid := RegEnable(Mux(s1_can_go(i), s1_oldestSelV, false.B), false.B, (s1_can_go(i) || replay_req(i).fire))
    s2_oldestSel(i).bits  := RegEnable(s1_oldestSel(i).bits, s1_can_go(i))

    vaddrModule.io.ren(i) := s1_oldestSel(i).valid && s1_can_go(i)
    vaddrModule.io.raddr(i) := s1_oldestSel(i).bits
  }

  for (i <- 0 until LoadPipelineWidth) {
    val s1_replayIdx = s1_oldestSel(i).bits
    val s2_replayUop = RegEnable(uop(s1_replayIdx), s1_can_go(i))
    val s2_replayMSHRId = RegEnable(missMSHRId(s1_replayIdx), s1_can_go(i))
    val s2_replacementUpdated = RegEnable(replacementUpdated(s1_replayIdx), s1_can_go(i))
    val s2_missDbUpdated = RegEnable(missDbUpdated(s1_replayIdx), s1_can_go(i))
    val s2_replayCauses = RegEnable(cause(s1_replayIdx), s1_can_go(i))
    val s2_replayCarry = RegEnable(replayCarryReg(s1_replayIdx), s1_can_go(i))
    val s2_replayCacheMissReplay = RegEnable(trueCacheMissReplay(s1_replayIdx), s1_can_go(i))
    s2_cancelReplay(i) := s2_replayUop.robIdx.needFlush(io.redirect)

    s2_can_go(i) := DontCare
    replay_req(i).valid             := s2_oldestSel(i).valid
    replay_req(i).bits              := DontCare
    replay_req(i).bits.uop          := s2_replayUop
    replay_req(i).bits.vaddr        := vaddrModule.io.rdata(i)
    replay_req(i).bits.isFirstIssue := false.B
    replay_req(i).bits.isLoadReplay := true.B
    replay_req(i).bits.replayCarry  := s2_replayCarry
    replay_req(i).bits.mshrid       := s2_replayMSHRId
    replay_req(i).bits.replacementUpdated := s2_replacementUpdated
    replay_req(i).bits.missDbUpdated := s2_missDbUpdated
    replay_req(i).bits.forward_tlDchannel := s2_replayCauses(LoadReplayCauses.C_DM)
    replay_req(i).bits.schedIndex   := s2_oldestSel(i).bits
    replay_req(i).bits.uop.loadWaitStrict := false.B

    when (replay_req(i).fire) {
      XSError(!allocated(s2_oldestSel(i).bits), p"LoadQueueReplay: why replay an invalid entry ${s2_oldestSel(i).bits} ?")
    }
  }

  val EnableHybridUnitReplay = Constantin.createRecord("EnableHybridUnitReplay", true.B)(0)
  when(EnableHybridUnitReplay) {
    for (i <- 0 until LoadPipelineWidth)
      io.replay(i) <> replay_req(i)
  }.otherwise {
    io.replay(0) <> replay_req(0)
    io.replay(2).valid := false.B
    io.replay(2).bits := DontCare

    val arbiter = Module(new RRArbiter(new LsPipelineBundle, 2))
    arbiter.io.in(0) <> replay_req(1)
    arbiter.io.in(1) <> replay_req(2)
    io.replay(1) <> arbiter.io.out
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

 // when(io.refill.valid) {
 //   XSDebug("miss resp: paddr:0x%x data %x\n", io.refill.bits.addr, io.refill.bits.data)
 // }

  //  LoadQueueReplay deallocate
  val freeMaskVec = Wire(Vec(LoadQueueReplaySize, Bool()))

  // init
  freeMaskVec.map(e => e := false.B)

  // Allocate logic
  val newEnqueue = (0 until LoadPipelineWidth).map(i => {
    needEnqueue(i) && !io.enq(i).bits.isLoadReplay
  })

  for ((enq, w) <- io.enq.zipWithIndex) {
    vaddrModule.io.wen(w) := false.B
    freeList.io.doAllocate(w) := false.B

    freeList.io.allocateReq(w) := true.B

    //  Allocated ready
    val offset = PopCount(newEnqueue.take(w))
    val canAccept = freeList.io.canAllocate(offset)
    val enqIndex = Mux(enq.bits.isLoadReplay, enq.bits.schedIndex, freeList.io.allocateSlot(offset))
    enqIndexOH(w) := UIntToOH(enqIndex)
    enq.ready := Mux(enq.bits.isLoadReplay, true.B, canAccept)

    when (needEnqueue(w) && enq.ready) {

      val debug_robIdx = enq.bits.uop.robIdx.asUInt
      XSError(allocated(enqIndex) && !enq.bits.isLoadReplay, p"LoadQueueReplay: can not accept more load, check: ldu $w, robIdx $debug_robIdx!")
      XSError(hasExceptions(w), p"LoadQueueReplay: The instruction has exception, it can not be replay, check: ldu $w, robIdx $debug_robIdx!")

      freeList.io.doAllocate(w) := !enq.bits.isLoadReplay

      //  Allocate new entry
      allocated(enqIndex) := true.B
      scheduled(enqIndex) := false.B
      uop(enqIndex)       := enq.bits.uop

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
      strict(enqIndex)       := false.B

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
          !(io.tlb_hint.resp.valid && (io.tlb_hint.resp.bits.id === replayInfo.tlb_id || io.tlb_hint.resp.bits.replay_all))
        tlbHintId(enqIndex) := replayInfo.tlb_id
      }

      // special case: dcache miss
      when (replayInfo.cause(LoadReplayCauses.C_DM) && enq.bits.handledByMSHR) {
        blocking(enqIndex) := !replayInfo.full_fwd && //  dcache miss
                              !(io.tl_d_channel.valid && io.tl_d_channel.mshrid === replayInfo.mshr_id) // no refill in this cycle
      }

      // special case: st-ld violation
      when (replayInfo.cause(LoadReplayCauses.C_MA)) {
        blockSqIdx(enqIndex) := replayInfo.addr_inv_sq_idx
        strict(enqIndex) := enq.bits.uop.loadWaitStrict
      }

      // special case: data forward fail
      when (replayInfo.cause(LoadReplayCauses.C_FF)) {
        blockSqIdx(enqIndex) := replayInfo.data_inv_sq_idx
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
      when (!needReplay(w) || hasExceptions(w)) {
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
  val rob_head_dcache_miss     = lq_match && cause(lq_match_idx)(LoadReplayCauses.C_DM)
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
  val perfValidCount = RegNext(PopCount(allocated))

  //  perf cnt
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
  XSPerfAccumulate("replay_dcache_miss", replayDCacheMissCount)
  XSPerfAccumulate("replay_hint_wakeup", s0_hintSelValid)
  XSPerfAccumulate("replay_hint_priority_beat1", io.l2_hint.valid && io.l2_hint.bits.isKeyword)

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
