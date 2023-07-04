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
import chipsalliance.rocketchip.config._
import xiangshan._
import xiangshan.backend.rob.{RobPtr, RobLsqIO}
import xiangshan.cache._
import xiangshan.backend.fu.fpu.FPU
import xiangshan.cache._
import xiangshan.frontend.FtqPtr
import xiangshan.ExceptionNO._
import xiangshan.cache.dcache.ReplayCarry
import xiangshan.mem.mdp._
import utils._
import utility._

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
  // st-ld violation 
  val waitStore         = 0
  // tlb miss check
  val tlbMiss           = 1
  // st-ld violation re-execute check
  val schedError        = 2
  // dcache bank conflict check
  val bankConflict      = 3
  // store-to-load-forwarding check
  val forwardFail       = 4
  // dcache replay check
  val dcacheReplay      = 5
  // dcache miss check
  val dcacheMiss        = 6
  // RAR queue accept check
  val rarReject         = 7
  // RAW queue accept check
  val rawReject         = 8
  // total causes
  val allCauses         = 9
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
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))  
    val enq = Vec(LoadPipelineWidth, Flipped(Decoupled(new LqWriteBundle)))
    val storeAddrIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))  
    val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new ExuOutput)))
    val replay = Vec(LoadPipelineWidth, Decoupled(new LsPipelineBundle))
    val refill = Flipped(ValidIO(new Refill)) 
    val stAddrReadySqPtr = Input(new SqPtr)
    val stAddrReadyVec = Input(Vec(StoreQueueSize, Bool()))
    val stDataReadySqPtr = Input(new SqPtr)
    val stDataReadyVec = Input(Vec(StoreQueueSize, Bool()))
    val sqEmpty = Input(Bool())
    val lqFull = Output(Bool())
    val ldWbPtr = Input(new LqPtr)
    val tlbReplayDelayCycleCtrl = Vec(4, Input(UInt(ReSelectLen.W))) 
    val rarFull = Input(Bool())
    val rawFull = Input(Bool())
    val l2Hint  = Input(Valid(new L2ToL1Hint()))
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
  val uop = Reg(Vec(LoadQueueReplaySize, new MicroOp))
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

  // freeliset: store valid entries index.
  // +---+---+--------------+-----+-----+
  // | 0 | 1 |      ......  | n-2 | n-1 |
  // +---+---+--------------+-----+-----+
  val freeList = Module(new FreeList(
    size = LoadQueueReplaySize, 
    allocWidth = LoadPipelineWidth,
    freeWidth = 4,
    moduleName = "LoadQueueReplay freelist"
  ))
  freeList.io := DontCare
  /**
   * used for re-select control
   */
  val credit = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(ReSelectLen.W))))
  val selBlocked = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))  
  //  Ptrs to control which cycle to choose
  val blockPtrTlb = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(2.W))))
  val blockPtrCache = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(2.W))))
  val blockPtrOthers = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(2.W))))
  //  Specific cycles to block
  val blockCyclesTlb = Reg(Vec(4, UInt(ReSelectLen.W)))
  blockCyclesTlb := io.tlbReplayDelayCycleCtrl
  val blockCyclesCache = RegInit(VecInit(Seq(0.U(ReSelectLen.W), 0.U(ReSelectLen.W), 0.U(ReSelectLen.W), 0.U(ReSelectLen.W))))
  val blockCyclesOthers = RegInit(VecInit(Seq(0.U(ReSelectLen.W), 0.U(ReSelectLen.W), 0.U(ReSelectLen.W), 0.U(ReSelectLen.W))))
  val blockSqIdx = Reg(Vec(LoadQueueReplaySize, new SqPtr))
  // block causes
  val blockByTlbMiss = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))  
  val blockByForwardFail = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B))) 
  val blockByWaitStore = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  val blockByCacheMiss = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  val blockByRARReject = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  val blockByRAWReject = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  val blockByOthers = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  // DCache miss block
  val missMSHRId = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U((log2Up(cfg.nMissEntries).W)))))
  // Has this load already updated dcache replacement?
  val replacementUpdated = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  val trueCacheMissReplay = WireInit(VecInit(cause.map(_(LoadReplayCauses.dcacheMiss))))
  val creditUpdate = WireInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(ReSelectLen.W))))
  (0 until LoadQueueReplaySize).map(i => {
    creditUpdate(i) := Mux(credit(i) > 0.U(ReSelectLen.W), credit(i)-1.U(ReSelectLen.W), credit(i))
    selBlocked(i) := creditUpdate(i) =/= 0.U(ReSelectLen.W) || credit(i) =/= 0.U(ReSelectLen.W)
  })
  val replayCarryReg = RegInit(VecInit(List.fill(LoadQueueReplaySize)(ReplayCarry(0.U, false.B))))
  val dataInLastBeatReg = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))

  /**
   * Enqueue
   */
  val canEnqueue = io.enq.map(_.valid)
  val cancelEnq = io.enq.map(enq => enq.bits.uop.robIdx.needFlush(io.redirect))
  val needReplay = io.enq.map(enq => enq.bits.replayInfo.needReplay())
  val hasExceptions = io.enq.map(enq => ExceptionNO.selectByFu(enq.bits.uop.cf.exceptionVec, lduCfg).asUInt.orR && !enq.bits.tlbMiss)
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
    dataNotBlockVec(i) := !isBefore(io.stDataReadySqPtr, blockSqIdx(i)) || stDataReadyVec(blockSqIdx(i).value) || io.sqEmpty // for better timing  
    addrNotBlockVec(i) := !isBefore(io.stAddrReadySqPtr, blockSqIdx(i)) || stAddrReadyVec(blockSqIdx(i).value) || io.sqEmpty // for better timing 

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

  // update block condition
  (0 until LoadQueueReplaySize).map(i => {
    blockByForwardFail(i) := Mux(blockByForwardFail(i) && stDataDeqVec(i), false.B, blockByForwardFail(i))
    blockByWaitStore(i) := Mux(blockByWaitStore(i) && stAddrDeqVec(i), false.B, blockByWaitStore(i))
    blockByCacheMiss(i) := Mux(blockByCacheMiss(i) && io.refill.valid && io.refill.bits.id === missMSHRId(i), false.B, blockByCacheMiss(i))

    when (blockByCacheMiss(i) && io.refill.valid && io.refill.bits.id === missMSHRId(i)) { creditUpdate(i) := 0.U }
    when (blockByRARReject(i) && (!io.rarFull || !isAfter(uop(i).lqIdx, io.ldWbPtr))) { blockByRARReject(i) := false.B }
    when (blockByRAWReject(i) && (!io.rawFull || !isAfter(uop(i).sqIdx, io.stAddrReadySqPtr))) { blockByRAWReject(i) := false.B }
    when (blockByTlbMiss(i) && creditUpdate(i) === 0.U) { blockByTlbMiss(i) := false.B }
    when (blockByOthers(i) && creditUpdate(i) === 0.U) { blockByOthers(i) := false.B }
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
  val s0_oldestSel = Wire(Vec(LoadPipelineWidth, Valid(UInt(log2Up(LoadQueueReplaySize + 1).W))))
  val s1_can_go = Wire(Vec(LoadPipelineWidth, Bool()))
  val s1_oldestSel = Wire(Vec(LoadPipelineWidth, Valid(UInt(log2Up(LoadQueueReplaySize + 1).W))))
  val s2_can_go = Wire(Vec(LoadPipelineWidth, Bool()))
  val s2_oldestSel = Wire(Vec(LoadPipelineWidth, Valid(UInt(log2Up(LoadQueueReplaySize + 1).W))))

  // generate mask
  val needCancel = Wire(Vec(LoadQueueReplaySize, Bool()))
  // generate enq mask
  val selectIndexOH = Wire(Vec(LoadPipelineWidth, UInt(LoadQueueReplaySize.W)))
  val s0_loadEnqFireMask = io.enq.map(x => x.fire && !x.bits.isLoadReplay).zip(selectIndexOH).map(x => Mux(x._1, x._2, 0.U))
  val s0_remLoadEnqFireVec = s0_loadEnqFireMask.map(x => VecInit((0 until LoadPipelineWidth).map(rem => getRemBits(x)(rem))))
  val s0_remEnqSelVec = Seq.tabulate(LoadPipelineWidth)(w => VecInit(s0_remLoadEnqFireVec.map(x => x(w))))

  // generate free mask
  val s0_loadFreeSelMask = needCancel.asUInt
  val s0_remFreeSelVec = VecInit(Seq.tabulate(LoadPipelineWidth)(rem => getRemBits(s0_loadFreeSelMask)(rem)))

  // l2 hint wakes up cache missed load
  // l2 will send GrantData in next 2/3 cycle, wake up the missed load early and sent them to load pipe, so them will hit the data in D channel or mshr in load S1
  val s0_loadHintWakeMask = VecInit((0 until LoadQueueReplaySize).map(i => {
    allocated(i) && !scheduled(i) && blockByCacheMiss(i) && missMSHRId(i) === io.l2Hint.bits.sourceId && io.l2Hint.valid
  })).asUInt()
  // l2 will send 2 beats data in 2 cycles, so if data needed by this load is in first beat, select it this cycle, otherwise next cycle
  val s0_loadHintSelMask = s0_loadHintWakeMask & VecInit(dataInLastBeatReg.map(!_)).asUInt
  val s0_remLoadHintSelMask = VecInit((0 until LoadPipelineWidth).map(rem => getRemBits(s0_loadHintSelMask)(rem)))
  val s0_hintSelValid = s0_loadHintSelMask.orR

  // wake up cache missed load
  (0 until LoadQueueReplaySize).foreach(i => {
    when(s0_loadHintWakeMask(i)) {
      blockByCacheMiss(i) := false.B
      creditUpdate(i) := 0.U
    }
  })
  
  // generate replay mask
  // replay select priority is given as follow
  // 1. hint wake up load
  // 2. higher priority load
  // 3. lower priority load
  val s0_loadHigherPriorityReplaySelMask = VecInit((0 until LoadQueueReplaySize).map(i => {
    val blocked = selBlocked(i) || blockByWaitStore(i) || blockByRARReject(i) || blockByRAWReject(i) || blockByOthers(i) || blockByForwardFail(i) || blockByCacheMiss(i) || blockByTlbMiss(i)
    val hasHigherPriority = cause(i)(LoadReplayCauses.dcacheMiss) || cause(i)(LoadReplayCauses.forwardFail)
    allocated(i) && !scheduled(i) && !blocked && hasHigherPriority
  })).asUInt // use uint instead vec to reduce verilog lines
  val s0_loadLowerPriorityReplaySelMask = VecInit((0 until LoadQueueReplaySize).map(i => {
    val blocked = selBlocked(i) || blockByWaitStore(i) || blockByRARReject(i) || blockByRAWReject(i) || blockByOthers(i) || blockByForwardFail(i) || blockByCacheMiss(i) || blockByTlbMiss(i)
    val hasLowerPriority = !cause(i)(LoadReplayCauses.dcacheMiss) && !cause(i)(LoadReplayCauses.forwardFail)
    allocated(i) && !scheduled(i) && !blocked && hasLowerPriority
  })).asUInt // use uint instead vec to reduce verilog lines
  val s0_loadNormalReplaySelMask = s0_loadLowerPriorityReplaySelMask | s0_loadHigherPriorityReplaySelMask | s0_loadHintSelMask
  val s0_remNormalReplaySelVec = VecInit((0 until LoadPipelineWidth).map(rem => getRemBits(s0_loadNormalReplaySelMask)(rem)))
  val s0_loadPriorityReplaySelMask = Mux(s0_hintSelValid, s0_loadHintSelMask, Mux(s0_loadHigherPriorityReplaySelMask.orR, s0_loadHigherPriorityReplaySelMask, s0_loadLowerPriorityReplaySelMask))
  val s0_remPriorityReplaySelVec = VecInit((0 until LoadPipelineWidth).map(rem => getRemBits(s0_loadPriorityReplaySelMask)(rem)))

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
      Mux(VecInit(s0_remOldsetMatchMaskVec(rem).map(_(0))).asUInt.orR, s0_remOldsetMatchMaskVec(rem)(i)(0), s0_remOlderMatchMaskVec(rem)(i).reduce(_|_))
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
    val l2HintFirst = io.l2Hint.valid && s0_remOldestHintSelVec(rport).orR
    val issOldestValid = l2HintFirst || s0_remOldestSelVec(rport).orR
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
    oldest.bits := OHToUInt(oldestBitsVec.asUInt)
    oldest
  }))


  // Replay port reorder
  class BalanceEntry extends XSBundle {
    val balance = Bool()
    val index = UInt(log2Up(LoadQueueReplaySize).W)
    val port = UInt(log2Up(LoadPipelineWidth).W)
  }

  def balanceReOrder(sel: Seq[ValidIO[BalanceEntry]]): Seq[ValidIO[BalanceEntry]] = {
    require(sel.length > 0)
    val balancePick = ParallelPriorityMux(sel.map(x => (x.valid && x.bits.balance) -> x))
    val reorderSel = Wire(Vec(sel.length, ValidIO(new BalanceEntry)))
    (0 until sel.length).map(i =>
      if (i == 0) {
        when (balancePick.valid && balancePick.bits.balance) {
          reorderSel(i) := balancePick
        } .otherwise {
          reorderSel(i) := sel(i)
        }
      } else {
        when (balancePick.valid && balancePick.bits.balance && i.U === balancePick.bits.port) {
          reorderSel(i) := sel(0)
        } .otherwise {
          reorderSel(i) := sel(i)
        }
      }
    )
    reorderSel
  }

  // stage2: send replay request to load unit
  // replay cold down
  val ColdDownCycles = 16
  val coldCounter = RegInit(VecInit(List.fill(LoadPipelineWidth)(0.U(log2Up(ColdDownCycles).W))))
  val ColdDownThreshold = Wire(UInt(log2Up(ColdDownCycles).W))
  ColdDownThreshold := 12.U
  assert(ColdDownCycles.U > ColdDownThreshold, "ColdDownCycles must great than ColdDownThreshold!")

  def replayCanFire(i: Int) = coldCounter(i) >= 0.U && coldCounter(i) < ColdDownThreshold
  def coldDownNow(i: Int) = coldCounter(i) >= ColdDownThreshold

  val s1_balanceOldestSelExt = (0 until LoadPipelineWidth).map(i => {
    val wrapper = Wire(Valid(new BalanceEntry))
    wrapper.valid := s1_oldestSel(i).valid
    wrapper.bits.balance := cause(s1_oldestSel(i).bits)(LoadReplayCauses.bankConflict)
    wrapper.bits.index := s1_oldestSel(i).bits
    wrapper.bits.port := i.U
    wrapper
  })

  val s1_balanceOldestSel = VecInit(balanceReOrder(s1_balanceOldestSelExt))
  for (i <- 0 until LoadPipelineWidth) {
    val s0_can_go = s1_can_go(s1_balanceOldestSel(i).bits.port) || uop(s1_oldestSel(i).bits).robIdx.needFlush(io.redirect)
    val s0_cancel = uop(s0_oldestSel(i).bits).robIdx.needFlush(io.redirect)
    val s0_oldestSelV = s0_oldestSel(i).valid && !s0_cancel
    s1_oldestSel(i).valid := RegEnable(s0_oldestSelV, s0_can_go)
    s1_oldestSel(i).bits := RegEnable(s0_oldestSel(i).bits, s0_can_go)

    when (s0_can_go && s0_oldestSelV) {
      scheduled(s0_oldestSel(i).bits) := true.B
    }
  }
  val s2_cancelReplay = Wire(Vec(LoadPipelineWidth, Bool()))
  for (i <- 0 until LoadPipelineWidth) {
    val s1_cancel = uop(s1_balanceOldestSel(i).bits.index).robIdx.needFlush(io.redirect)
    val s1_oldestSelV = s1_balanceOldestSel(i).valid && !s1_cancel
    s1_can_go(i) := Mux(s2_oldestSel(i).valid && !s2_cancelReplay(i), io.replay(i).ready && replayCanFire(i), true.B)
    s2_oldestSel(i).valid := RegEnable(s1_oldestSelV, s1_can_go(i))
    s2_oldestSel(i).bits := RegEnable(s1_balanceOldestSel(i).bits.index, s1_can_go(i))

    vaddrModule.io.ren(i) := s1_balanceOldestSel(i).valid && s1_can_go(i)
    vaddrModule.io.raddr(i) := s1_balanceOldestSel(i).bits.index
  }

  for (i <- 0 until LoadPipelineWidth) {
    val s1_replayIdx = s1_balanceOldestSel(i).bits.index
    val s2_replayUop = RegEnable(uop(s1_replayIdx), s1_can_go(i))
    val s2_replayMSHRId = RegEnable(missMSHRId(s1_replayIdx), s1_can_go(i))
    val s2_replacementUpdated = RegEnable(replacementUpdated(s1_replayIdx), s1_can_go(i))
    val s2_replayCauses = RegEnable(cause(s1_replayIdx), s1_can_go(i))
    val s2_replayCarry = RegEnable(replayCarryReg(s1_replayIdx), s1_can_go(i))
    val s2_replayCacheMissReplay = RegEnable(trueCacheMissReplay(s1_replayIdx), s1_can_go(i))
    s2_cancelReplay(i) := s2_replayUop.robIdx.needFlush(io.redirect)

    s2_can_go(i) := DontCare
    io.replay(i).valid := s2_oldestSel(i).valid && !s2_cancelReplay(i) && replayCanFire(i) 
    io.replay(i).bits := DontCare
    io.replay(i).bits.uop := s2_replayUop
    io.replay(i).bits.vaddr := vaddrModule.io.rdata(i)
    io.replay(i).bits.isFirstIssue := false.B
    io.replay(i).bits.isLoadReplay := true.B
    io.replay(i).bits.replayCarry := s2_replayCarry
    io.replay(i).bits.mshrid := s2_replayMSHRId
    io.replay(i).bits.replacementUpdated := s2_replacementUpdated
    io.replay(i).bits.forward_tlDchannel := s2_replayCauses(LoadReplayCauses.dcacheMiss)
    io.replay(i).bits.sleepIndex := s2_oldestSel(i).bits

    when (io.replay(i).fire) {
      XSError(!allocated(s2_oldestSel(i).bits), p"LoadQueueReplay: why replay an invalid entry ${s2_oldestSel(i).bits} ?")
    }
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

  when(io.refill.valid) {
    XSDebug("miss resp: paddr:0x%x data %x\n", io.refill.bits.addr, io.refill.bits.data)
  }

  //  LoadQueueReplay deallocate
  val freeMaskVec = Wire(Vec(LoadQueueReplaySize, Bool()))

  // init
  freeMaskVec.map(e => e := false.B)

  // Allocate logic 
  val enqValidVec = Wire(Vec(LoadPipelineWidth, Bool()))
  val enqIndexVec = Wire(Vec(LoadPipelineWidth, UInt()))

  val newEnqueue = (0 until LoadPipelineWidth).map(i => {
    needEnqueue(i) && !io.enq(i).bits.isLoadReplay
  })

  for ((enq, w) <- io.enq.zipWithIndex) {
    vaddrModule.io.wen(w) := false.B
    freeList.io.doAllocate(w) := false.B

    freeList.io.allocateReq(w) := newEnqueue(w)

    //  Allocated ready
    enqValidVec(w) := freeList.io.canAllocate(w)
    enqIndexVec(w) := Mux(enq.bits.isLoadReplay, enq.bits.sleepIndex, freeList.io.allocateSlot(w))
    selectIndexOH(w) := UIntToOH(enqIndexVec(w))
    enq.ready := Mux(enq.bits.isLoadReplay, true.B, enqValidVec(w))

    val enqIndex = enqIndexVec(w)
    when (needEnqueue(w) && enq.ready) {

      val debug_robIdx = enq.bits.uop.robIdx.asUInt
      XSError(allocated(enqIndex) && !enq.bits.isLoadReplay, p"LoadQueueReplay: can not accept more load, check: ldu $w, robIdx $debug_robIdx!")
      XSError(hasExceptions(w), p"LoadQueueReplay: The instruction has exception, it can not be replay, check: ldu $w, robIdx $debug_robIdx!")

      freeList.io.doAllocate(w) := !enq.bits.isLoadReplay

      //  Allocate new entry
      allocated(enqIndex) := true.B
      scheduled(enqIndex) := false.B
      uop(enqIndex) := enq.bits.uop

      vaddrModule.io.wen(w) := true.B
      vaddrModule.io.waddr(w) := enqIndex 
      vaddrModule.io.wdata(w) := enq.bits.vaddr
      debug_vaddr(enqIndex) := enq.bits.vaddr

      /**
       * used for feedback and replay
       */
      // set flags
      val replayInfo = enq.bits.replayInfo
      val dataInLastBeat = replayInfo.dataInLastBeat
      cause(enqIndex) := replayInfo.cause.asUInt

      // update credit
      val blockCyclesTlbPtr = blockPtrTlb(enqIndex)
      val blockCyclesCachePtr = blockPtrCache(enqIndex)
      val blockCyclesOtherPtr = blockPtrOthers(enqIndex)
      creditUpdate(enqIndex) := Mux(replayInfo.cause(LoadReplayCauses.tlbMiss), blockCyclesTlb(blockCyclesTlbPtr), 
                                Mux(replayInfo.cause(LoadReplayCauses.dcacheMiss), blockCyclesCache(blockCyclesCachePtr) + dataInLastBeat, blockCyclesOthers(blockCyclesOtherPtr)))

      // init
      blockByTlbMiss(enqIndex) := false.B
      blockByWaitStore(enqIndex) := false.B
      blockByForwardFail(enqIndex) := false.B
      blockByCacheMiss(enqIndex) := false.B
      blockByRARReject(enqIndex) := false.B
      blockByRAWReject(enqIndex) := false.B
      blockByOthers(enqIndex) := false.B

      // update block pointer
      when (replayInfo.cause(LoadReplayCauses.dcacheReplay)) {
        // normal case: dcache replay
        blockByOthers(enqIndex) := true.B
        blockPtrOthers(enqIndex) :=  Mux(blockPtrOthers(enqIndex) === 3.U(2.W), blockPtrOthers(enqIndex), blockPtrOthers(enqIndex) + 1.U(2.W)) 
      } .elsewhen (replayInfo.cause(LoadReplayCauses.bankConflict) || replayInfo.cause(LoadReplayCauses.schedError)) {
        // normal case: bank conflict or schedule error
        // can replay next cycle
        creditUpdate(enqIndex) := 0.U
        blockByOthers(enqIndex) := false.B
      }

      // special case: tlb miss
      when (replayInfo.cause(LoadReplayCauses.tlbMiss)) {
        blockByTlbMiss(enqIndex) := true.B
        blockPtrTlb(enqIndex) := Mux(blockPtrTlb(enqIndex) === 3.U(2.W), blockPtrTlb(enqIndex), blockPtrTlb(enqIndex) + 1.U(2.W))
      }

      // special case: dcache miss
      when (replayInfo.cause(LoadReplayCauses.dcacheMiss) && enq.bits.handledByMSHR) {
        blockByCacheMiss(enqIndex) := !replayInfo.canForwardFullData && //  dcache miss
                                  !(io.refill.valid && io.refill.bits.id === replayInfo.missMSHRId) // no refill in this cycle
                                  
        blockPtrCache(enqIndex) := Mux(blockPtrCache(enqIndex) === 3.U(2.W), blockPtrCache(enqIndex), blockPtrCache(enqIndex) + 1.U(2.W))
      }

      // special case: st-ld violation
      when (replayInfo.cause(LoadReplayCauses.waitStore)) {
        blockByWaitStore(enqIndex) := true.B
        blockSqIdx(enqIndex) := replayInfo.addrInvalidSqIdx
        blockPtrOthers(enqIndex) :=  Mux(blockPtrOthers(enqIndex) === 3.U(2.W), blockPtrOthers(enqIndex), blockPtrOthers(enqIndex) + 1.U(2.W)) 
      }

      // special case: data forward fail
      when (replayInfo.cause(LoadReplayCauses.forwardFail)) {
        blockByForwardFail(enqIndex) := true.B
        blockSqIdx(enqIndex) := replayInfo.dataInvalidSqIdx
        blockPtrOthers(enqIndex) :=  Mux(blockPtrOthers(enqIndex) === 3.U(2.W), blockPtrOthers(enqIndex), blockPtrOthers(enqIndex) + 1.U(2.W)) 
      }

      // special case: rar reject
      when (replayInfo.cause(LoadReplayCauses.rarReject)) {
        blockByRARReject(enqIndex) := true.B
        blockPtrOthers(enqIndex) :=  Mux(blockPtrOthers(enqIndex) === 3.U(2.W), blockPtrOthers(enqIndex), blockPtrOthers(enqIndex) + 1.U(2.W))
      }

      // special case: raw reject
      when (replayInfo.cause(LoadReplayCauses.rawReject)) {
        blockByRAWReject(enqIndex) := true.B
        blockPtrOthers(enqIndex) :=  Mux(blockPtrOthers(enqIndex) === 3.U(2.W), blockPtrOthers(enqIndex), blockPtrOthers(enqIndex) + 1.U(2.W))
      }

      // extra info
      replayCarryReg(enqIndex) := replayInfo.replayCarry
      replacementUpdated(enqIndex) := enq.bits.replacementUpdated
      // update missMSHRId only when the load has already been handled by mshr
      when(enq.bits.handledByMSHR) {
        missMSHRId(enqIndex) := replayInfo.missMSHRId
      }
      dataInLastBeatReg(enqIndex) := dataInLastBeat
    }

    //
    val sleepIndex = enq.bits.sleepIndex 
    when (enq.valid && enq.bits.isLoadReplay) {
      when (!needReplay(w) || hasExceptions(w)) {
        allocated(sleepIndex) := false.B 
        freeMaskVec(sleepIndex) := true.B
      } .otherwise {
        scheduled(sleepIndex) := false.B
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
  val sourceVaddr = WireInit(0.U.asTypeOf(new Valid(UInt(VAddrBits.W))))

  ExcitingUtils.addSink(sourceVaddr, s"rob_head_vaddr_${coreParams.HartId}", ExcitingUtils.Perf)

  val uop_wrapper = Wire(Vec(LoadQueueReplaySize, new XSBundleWithMicroOp))
  (uop_wrapper.zipWithIndex).foreach {
    case (u, i) => {
      u.uop := uop(i)
    }
  }
  val lq_match_vec = (debug_vaddr.zip(allocated)).map{case(va, alloc) => alloc && (va === sourceVaddr.bits)}
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
  val lq_match = rob_head_lq_match._1 && sourceVaddr.valid
  val lq_match_idx = lq_match_bits.lqIdx.value

  val rob_head_tlb_miss = lq_match && cause(lq_match_idx)(LoadReplayCauses.tlbMiss)
  val rob_head_sched_error = lq_match && cause(lq_match_idx)(LoadReplayCauses.schedError)
  val rob_head_wait_store = lq_match && cause(lq_match_idx)(LoadReplayCauses.waitStore)
  val rob_head_confilct_replay = lq_match && cause(lq_match_idx)(LoadReplayCauses.bankConflict)
  val rob_head_forward_fail = lq_match && cause(lq_match_idx)(LoadReplayCauses.forwardFail)
  val rob_head_mshrfull_replay = lq_match && cause(lq_match_idx)(LoadReplayCauses.dcacheReplay)
  val rob_head_dcache_miss = lq_match && cause(lq_match_idx)(LoadReplayCauses.dcacheMiss)
  val rob_head_rar_reject = lq_match && cause(lq_match_idx)(LoadReplayCauses.rarReject)
  val rob_head_raw_reject = lq_match && cause(lq_match_idx)(LoadReplayCauses.rawReject)
  val rob_head_other_replay    = lq_match && (rob_head_rar_reject || rob_head_raw_reject || rob_head_forward_fail)
    
  val rob_head_vio_replay = rob_head_sched_error || rob_head_wait_store

  val rob_head_miss_in_dtlb = WireInit(false.B)
  ExcitingUtils.addSink(rob_head_miss_in_dtlb, s"miss_in_dtlb_${coreParams.HartId}", ExcitingUtils.Perf)
  ExcitingUtils.addSource(rob_head_tlb_miss && !rob_head_miss_in_dtlb, s"load_tlb_replay_stall_${coreParams.HartId}", ExcitingUtils.Perf, true)
  ExcitingUtils.addSource(rob_head_tlb_miss &&  rob_head_miss_in_dtlb, s"load_tlb_miss_stall_${coreParams.HartId}", ExcitingUtils.Perf, true)
  ExcitingUtils.addSource(rob_head_vio_replay, s"load_vio_replay_stall_${coreParams.HartId}", ExcitingUtils.Perf, true)
  ExcitingUtils.addSource(rob_head_mshrfull_replay, s"load_mshr_replay_stall_${coreParams.HartId}", ExcitingUtils.Perf, true)
  // ExcitingUtils.addSource(rob_head_confilct_replay, s"load_l1_cache_stall_with_bank_conflict_${coreParams.HartId}", ExcitingUtils.Perf, true)
  ExcitingUtils.addSource(rob_head_other_replay, s"rob_head_other_replay_${coreParams.HartId}", ExcitingUtils.Perf, true)
  val perfValidCount = RegNext(PopCount(allocated))

  //  perf cnt
  val enqCount = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay)) 
  val deqCount = PopCount(io.replay.map(_.fire)) 
  val deqBlockCount = PopCount(io.replay.map(r => r.valid && !r.ready))
  val replayTlbMissCount = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.replayInfo.cause(LoadReplayCauses.tlbMiss)))
  val replayWaitStoreCount = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.replayInfo.cause(LoadReplayCauses.waitStore)))
  val replaySchedErrorCount = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.replayInfo.cause(LoadReplayCauses.schedError)))
  val replayRARRejectCount = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.replayInfo.cause(LoadReplayCauses.rarReject)))
  val replayRAWRejectCount = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.replayInfo.cause(LoadReplayCauses.rawReject)))
  val replayBankConflictCount = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.replayInfo.cause(LoadReplayCauses.bankConflict)))
  val replayDCacheReplayCount = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.replayInfo.cause(LoadReplayCauses.dcacheReplay)))
  val replayForwardFailCount = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.replayInfo.cause(LoadReplayCauses.forwardFail)))
  val replayDCacheMissCount = PopCount(io.enq.map(enq => enq.fire && !enq.bits.isLoadReplay && enq.bits.replayInfo.cause(LoadReplayCauses.dcacheMiss)))
  XSPerfAccumulate("enq", enqCount)
  XSPerfAccumulate("deq", deqCount)
  XSPerfAccumulate("deq_block", deqBlockCount)
  XSPerfAccumulate("replay_full", io.lqFull)
  XSPerfAccumulate("replay_rar_reject", replayRARRejectCount)
  XSPerfAccumulate("replay_raw_reject", replayRAWRejectCount)
  XSPerfAccumulate("replay_sched_error", replaySchedErrorCount)
  XSPerfAccumulate("replay_wait_store", replayWaitStoreCount)
  XSPerfAccumulate("replay_tlb_miss", replayTlbMissCount)
  XSPerfAccumulate("replay_bank_conflict", replayBankConflictCount)
  XSPerfAccumulate("replay_dcache_replay", replayDCacheReplayCount)
  XSPerfAccumulate("replay_forward_fail", replayForwardFailCount)
  XSPerfAccumulate("replay_dcache_miss", replayDCacheMissCount)
  XSPerfAccumulate("replay_hint_wakeup", s0_hintSelValid)

  val perfEvents: Seq[(String, UInt)] = Seq(
    ("enq", enqCount),
    ("deq", deqCount),
    ("deq_block", deqBlockCount),
    ("replay_full", io.lqFull),
    ("replay_rar_reject", replayRARRejectCount),
    ("replay_raw_reject", replayRAWRejectCount),
    ("replay_advance_sched", replaySchedErrorCount),
    ("replay_wait_store", replayWaitStoreCount),
    ("replay_tlb_miss", replayTlbMissCount),
    ("replay_bank_conflict", replayBankConflictCount),
    ("replay_dcache_replay", replayDCacheReplayCount),
    ("replay_forward_fail", replayForwardFailCount),
    ("replay_dcache_miss", replayDCacheMissCount),
  )
  generatePerfEvent()
  // end
}
