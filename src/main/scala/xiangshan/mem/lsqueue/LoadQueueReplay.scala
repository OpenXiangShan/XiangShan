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

  // RAR/RAW queue accept check
  val rejectEnq         = 0
  // st-ld violation re-execute check
  val schedError        = 1
  // st-ld violation 
  val waitStore         = 2
  // tlb miss check
  val tlbMiss           = 3
  // dcache bank conflict check
  val bankConflict      = 4
  // store-to-load-forwarding check
  val forwardFail       = 5
  // dcache replay check
  val dcacheReplay      = 6
  // dcache miss check
  val dcacheMiss        = 7
  // total causes
  val allCauses         = 8
}

class LoadQueueReplay(implicit p: Parameters) extends XSModule 
  with HasDCacheParameters
  with HasCircularQueuePtrHelper
  with HasLoadHelper
  with HasPerfEvents
{
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))  
    val enq = Vec(LoadPipelineWidth, Flipped(Valid(new LqWriteBundle)))
    val storeAddrIn = Vec(StorePipelineWidth, Flipped(Valid(new LsPipelineBundle)))  
    val storeDataIn = Vec(StorePipelineWidth, Flipped(Valid(new ExuOutput)))
    val replay = Vec(LoadPipelineWidth, Decoupled(new LsPipelineBundle))
    val refill = Flipped(ValidIO(new Refill)) 
    val stAddrReadySqPtr = Input(new SqPtr)
    val stDataReadySqPtr = Input(new SqPtr)
    val sqEmpty = Input(Bool())
    val lqFull = Output(Bool())
    val tlbReplayDelayCycleCtrl = Vec(4, Input(UInt(ReSelectLen.W))) 
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
  val uop = Reg(Vec(LoadQueueReplaySize, new MicroOp))
  val vaddrModule = Module(new LqVAddrModule(
    gen = UInt(VAddrBits.W), 
    numEntries = LoadQueueReplaySize, 
    numRead = 2, 
    numWrite = LoadPipelineWidth,
    numWBank = LoadQueueNWriteBanks,
    numCamPort = 0))
  vaddrModule.io := DontCare 
  val cause = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(LoadReplayCauses.allCauses.W))))

  class QueueAllocateFlags extends Bundle {
    // rar queue has been allocated already
    val rarAllocated = Bool()
    val rarIndex = UInt(log2Up(LoadQueueRARSize).W)
    // raw queue has been allocated already
    val rawAllocated = Bool()
    val rawIndex = UInt(log2Up(LoadQueueRAWSize).W)
  }
  val flags = Reg(Vec(LoadQueueReplaySize, new QueueAllocateFlags))

  /**
   * used for re-select control
   */

  val credit = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(ReSelectLen.W))))
  //  Ptrs to control which cycle to choose
  val blockPtrTlb = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(2.W))))
  val blockPtrCache = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(2.W))))
  val blockPtrOthers = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(2.W))))
  //  Specific cycles to block
  val blockCyclesTlb = Reg(Vec(4, UInt(ReSelectLen.W)))
  blockCyclesTlb := io.tlbReplayDelayCycleCtrl
  val blockCyclesCache = RegInit(VecInit(Seq(11.U(ReSelectLen.W), 0.U(ReSelectLen.W), 31.U(ReSelectLen.W), 0.U(ReSelectLen.W))))
  val blockCyclesOthers = RegInit(VecInit(Seq(0.U(ReSelectLen.W), 0.U(ReSelectLen.W), 0.U(ReSelectLen.W), 0.U(ReSelectLen.W))))
  val blockCounter = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(5.W))))
  val selBlocked = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))  
  //  Data forward block  
  val blockSqIdx = Reg(Vec(LoadQueueReplaySize, new SqPtr))
  val blockByTlbMiss = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))  
  val blockByForwardFail = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B))) 
  val blockByWaitStore = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  val blockByCacheMiss = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  val blockByOthers = RegInit(VecInit(List.fill(LoadQueueReplaySize)(false.B)))
  //  DCache miss block
  val missMSHRId = RegInit(VecInit(List.fill(LoadQueueReplaySize)(0.U((log2Up(cfg.nMissEntries).W)))))
  val trueCacheMissReplay = WireInit(VecInit(cause.map(_(LoadReplayCauses.dcacheMiss))))
  val creditUpdate = WireInit(VecInit(List.fill(LoadQueueReplaySize)(0.U(ReSelectLen.W))))
  (0 until LoadQueueReplaySize).map(i => {
    creditUpdate(i) := Mux(credit(i) > 0.U(ReSelectLen.W), credit(i)-1.U(ReSelectLen.W), credit(i))
    selBlocked(i) := creditUpdate(i) =/= 0.U(ReSelectLen.W) || credit(i) =/= 0.U(ReSelectLen.W)
  })
  val replayCarryReg = RegInit(VecInit(List.fill(LoadQueueReplaySize)(ReplayCarry(0.U, false.B))))

  // release logic generation
  val storeAddrInSameCycleVec = Wire(Vec(LoadQueueReplaySize, Bool()))
  val storeDataInSameCycleVec = Wire(Vec(LoadQueueReplaySize, Bool()))
  val addrNotBlockVec = Wire(Vec(LoadQueueReplaySize, Bool()))
  val dataNotBlockVec = Wire(Vec(LoadQueueReplaySize, Bool()))
  val storeAddrValidVec = RegNext(addrNotBlockVec.asUInt | storeAddrInSameCycleVec.asUInt)
  val storeDataValidVec = RegNext(dataNotBlockVec.asUInt | storeDataInSameCycleVec.asUInt)

  // 
  val canEnqueue = io.enq.map(_.valid)
  val cancelEnq = io.enq.map(enq => enq.bits.uop.robIdx.needFlush(io.redirect))
  val needReplay = io.enq.map(enq => enq.bits.replayInfo.needReplay())
  val hasExceptions = io.enq.map(enq => ExceptionNO.selectByFu(enq.bits.uop.cf.exceptionVec, lduCfg).asUInt.orR && !enq.bits.tlbMiss)
  val canEnqueueVec = VecInit((0 until LoadPipelineWidth).map(w => {
    canEnqueue(w) && !cancelEnq(w) && needReplay(w) && !hasExceptions(w) 
  }))

  // store data valid check
  for (i <- 0 until LoadQueueReplaySize) {
    // dequeue
    //  FIXME: store*Ptr is not accurate
    dataNotBlockVec(i) := !isBefore(io.stDataReadySqPtr, blockSqIdx(i)) || io.sqEmpty // for better timing
    addrNotBlockVec(i) := !isBefore(io.stAddrReadySqPtr, blockSqIdx(i)) || io.sqEmpty  // for better timing

    // store address execute
    storeAddrInSameCycleVec(i) := (0 until StorePipelineWidth).map(w => {
      io.storeAddrIn(w).valid && 
      !io.storeAddrIn(w).bits.miss && 
      blockSqIdx(i) === io.storeAddrIn(w).bits.uop.sqIdx
    }).reduce(_|_) // for better timing

    // store data execute
    storeDataInSameCycleVec(i) := (0 until StorePipelineWidth).map(w => {
      io.storeDataIn(w).valid && 
      blockSqIdx(i) === io.storeDataIn(w).bits.uop.sqIdx
    }).reduce(_|_) // for better timing
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
    when (blockByCacheMiss(i) && creditUpdate(i) === 0.U) { blockByCacheMiss(i) := false.B }
    when (blockByTlbMiss(i) && creditUpdate(i) === 0.U) { blockByTlbMiss(i) := false.B }
    when (blockByOthers(i) && creditUpdate(i) === 0.U) { blockByOthers(i) := false.B }

  })  

  //  Replay is splitted into 2 stages
  val oldestMask = Wire(Vec(LoadQueueReplaySize, Bool()))
  val oldestMaskUInt = oldestMask.asUInt
  val loadReplaySelVec = VecInit((0 until LoadQueueReplaySize).map(i => {
    val blocked = selBlocked(i) || blockByTlbMiss(i) || blockByForwardFail(i) || blockByCacheMiss(i) || blockByWaitStore(i) || blockByOthers(i)
    allocated(i) && !blocked
  })).asUInt // use uint instead vec to reduce verilog lines
  // check oldest inst
  (0 until LoadQueueReplaySize).map(i => {
    oldestMask(i) := loadReplaySelVec(i) && blockCounter(i) === 16.U
  })

  // update block counter
  (0 until LoadQueueReplaySize).map(i => {
    blockCounter(i) := Mux(blockCounter(i) =/= 16.U && loadReplaySelVec(i) && !oldestMaskUInt.orR, blockCounter(i) + 1.U, blockCounter(i))
  })

  //
  val replayCancelMask = Wire(Vec(LoadQueueReplaySize, Bool()))
  (0 until LoadQueueReplaySize).map(i => {
    replayCancelMask(i) := allocated(i) && uop(i).robIdx.needFlush(io.redirect)
  })
  val replaySelMask = Wire(UInt(LoadQueueReplaySize.W))
  // stage 0 generate select mask
  // if replay queue has oldest inst, replay first
  val deqOrCancelMask = replaySelMask | replayCancelMask.asUInt
  val oldestDeqMask = oldestMaskUInt & ~deqOrCancelMask
  // make chisel happy
  val loadReplayDeqMask = Wire(UInt(LoadQueueReplaySize.W))
  loadReplayDeqMask := loadReplaySelVec.asUInt & ~deqOrCancelMask
  val s0_deqMask = Mux(oldestDeqMask.orR, oldestDeqMask, loadReplayDeqMask)

  // stage 1 generate select index
  // make chisel happy
  val s1_selectMask = Wire(UInt(LoadQueueReplaySize.W)) 
  s1_selectMask := RegNext(s0_deqMask) & ~deqOrCancelMask
  val s1_selectIndexOH = SelectFirstN(s1_selectMask, LoadPipelineWidth, Fill(LoadPipelineWidth, true.B))
  val s1_selectIndex = s1_selectIndexOH.map(OHToUInt(_))

  (0 until LoadPipelineWidth).map(w => {
    vaddrModule.io.raddr(w) := s1_selectIndex(w)
  })  

  // stage 2 replay now
  val s2_selectIndexV = s1_selectIndexOH.map(idx => RegNext(idx =/= 0.U))
  val s2_selectIndex = s1_selectIndex.map(idx => RegNext(idx))
  replaySelMask := s1_selectIndexOH.zipWithIndex.map(x => Fill(LoadQueueReplaySize, io.replay(x._2).fire) & RegNext(x._1)).reduce(_|_)

  val hasBankConflictVec = VecInit(s2_selectIndexV.zip(s2_selectIndex).map(w => w._1 && cause(w._2)(LoadReplayCauses.bankConflict)))
  val hasBankConflict = hasBankConflictVec.asUInt.orR
  val allBankConflict = hasBankConflictVec.asUInt.andR
  val coldCounter = RegInit(0.U(3.W))
  for (i <- 0 until LoadPipelineWidth) {
    val replayIdx = s2_selectIndex(i)
    val cancelReplay = replayCancelMask(replayIdx)
    // In order to avoid deadlock, replay one inst which blocked by bank conflict
    val bankConflictReplay = Mux(hasBankConflict && !allBankConflict, cause(replayIdx)(LoadReplayCauses.bankConflict), true.B)

    io.replay(i).valid := s2_selectIndexV(i) && !cancelReplay && bankConflictReplay && coldCounter >= 0.U && coldCounter < 5.U
    io.replay(i).bits := DontCare
    io.replay(i).bits.uop := uop(replayIdx)
    io.replay(i).bits.vaddr := vaddrModule.io.rdata(i)
    io.replay(i).bits.isFirstIssue := cause(replayIdx)(LoadReplayCauses.rejectEnq)
    io.replay(i).bits.isLoadReplay := true.B
    io.replay(i).bits.replayCarry := replayCarryReg(replayIdx)
    io.replay(i).bits.mshrid := missMSHRId(replayIdx)
    io.replay(i).bits.forward_tlDchannel := trueCacheMissReplay(replayIdx)
    io.replay(i).bits.rarAllocated := flags(replayIdx).rarAllocated
    io.replay(i).bits.rarIndex := flags(replayIdx).rarIndex
    io.replay(i).bits.rawAllocated := flags(replayIdx).rawAllocated
    io.replay(i).bits.rawIndex := flags(replayIdx).rawIndex

    when (io.replay(i).fire) {
      allocated(replayIdx) := false.B
      XSError(!allocated(replayIdx), s"why replay an invalid entry ${replayIdx} ?\n")
    }
  }

  // update cold counter
  val lastReplay = RegNext(io.replay.map(_.fire).reduce(_|_))
  when (lastReplay && io.replay.map(_.fire).reduce(_|_)) {
    coldCounter := coldCounter + 1.U
  } .elsewhen (coldCounter >= 5.U) {
    coldCounter := coldCounter + 1.U
  } .otherwise {
    coldCounter := 0.U
  }

  /**
   * Enqueue
   */
  val EnqueueThreshold = LoadPipelineWidth * 5
  val issueThreshold = LoadPipelineWidth * 5
  val enqIdxOH = SelectFirstN(~allocated.asUInt, LoadPipelineWidth, canEnqueueVec.asUInt)
  val lqFull = (LoadQueueReplaySize.U - PopCount(allocated)) <= EnqueueThreshold.U

  for ((enq, w) <- io.enq.zipWithIndex) {
    val enqIdx = OHToUInt(enqIdxOH(w))
    vaddrModule.io.wen(w) := false.B

    when (canEnqueueVec(w)) {
      assert(!allocated(enqIdx), s"Can not accept more load, check robIdx: ${enq.bits.uop.robIdx}!")
      assert(!hasExceptions(w), s"Exception, can not be replay, check robIdx: ${enq.bits.uop.robIdx}!")
      //  Allocate new entry
      allocated(enqIdx) := true.B
      uop(enqIdx) := enq.bits.uop

      vaddrModule.io.wen(w) := true.B
      vaddrModule.io.waddr(w) := enqIdx 
      vaddrModule.io.wdata(w) := enq.bits.vaddr

      /**
       * used for feedback and replay
       */
      // set flags
      val replayInfo = enq.bits.replayInfo
      val dataInLastBeat = replayInfo.dataInLastBeat
      cause(enqIdx) := replayInfo.cause.asUInt
      creditUpdate(enqIdx) := Mux(replayInfo.cause(LoadReplayCauses.tlbMiss), blockCyclesTlb(enqIdx), 
                                Mux(replayInfo.cause(LoadReplayCauses.dcacheMiss), blockCyclesCache(enqIdx) + dataInLastBeat, blockPtrOthers(enqIdx)))

      // init
      blockByTlbMiss(enqIdx) := false.B
      blockByWaitStore(enqIdx) := false.B
      blockByForwardFail(enqIdx) := false.B
      blockByCacheMiss(enqIdx) := false.B
      blockByOthers(enqIdx) := false.B

      when (!replayInfo.cause(LoadReplayCauses.rejectEnq)) {
        // update block pointer
        when (replayInfo.cause(LoadReplayCauses.tlbMiss)) {
          // special case: tlb miss
          blockByTlbMiss(enqIdx) := true.B
          blockPtrTlb(enqIdx) := blockPtrTlb(enqIdx) + Mux(blockPtrTlb(enqIdx) === 3.U(2.W), 0.U, 1.U)
        } .elsewhen (replayInfo.cause(LoadReplayCauses.dcacheMiss)) {
          // special case: dcache miss
          blockPtrCache(enqIdx) := blockPtrCache(enqIdx) + Mux(blockPtrCache(enqIdx) === 3.U(2.W), 0.U, 1.U)
        } .elsewhen (replayInfo.cause(LoadReplayCauses.dcacheReplay) || replayInfo.cause(LoadReplayCauses.waitStore)) {
          blockByOthers(enqIdx) := true.B
          blockPtrOthers(enqIdx) := blockPtrOthers(enqIdx) + Mux(blockPtrOthers(enqIdx) === 3.U(2.W), 0.U, 1.U)
        } .elsewhen (replayInfo.cause(LoadReplayCauses.bankConflict) || replayInfo.cause(LoadReplayCauses.schedError)) {
          blockByOthers(enqIdx) := true.B
          blockPtrOthers(enqIdx) := Mux(blockPtrOthers(enqIdx) === 3.U(2.W), blockPtrOthers(enqIdx), blockPtrOthers(enqIdx) + 1.U(2.W)) 
        }

        // special case: st-ld violation
        when (replayInfo.cause(LoadReplayCauses.waitStore)) {
          blockByWaitStore(enqIdx) := true.B
          blockSqIdx(enqIdx) := replayInfo.addrInvalidSqIdx
        }

        // special case: data forward fail
        when (replayInfo.cause(LoadReplayCauses.forwardFail)) {
          blockByForwardFail(enqIdx) := true.B
          blockSqIdx(enqIdx) := replayInfo.dataInvalidSqIdx
        }

        // 
        replayCarryReg(enqIdx) := replayInfo.replayCarry
        missMSHRId(enqIdx) := replayInfo.missMSHRId
        blockByCacheMiss(enqIdx) := replayInfo.cause(LoadReplayCauses.dcacheMiss) && !replayInfo.canForwardFullData && //  dcache miss
                                    !(io.refill.valid && io.refill.bits.id === replayInfo.missMSHRId) &&// no refill in this cycle
                                    creditUpdate(enqIdx) =/= 0.U //  credit is not zero
      } .otherwise {
        blockByOthers(enqIdx) := true.B
        blockPtrOthers(enqIdx) := Mux(blockPtrOthers(enqIdx) === 3.U(2.W), blockPtrOthers(enqIdx), blockPtrOthers(enqIdx) + 1.U(2.W)) 
      }

      // fill replay flags
      flags(enqIdx).rarAllocated := enq.bits.rarAllocated
      flags(enqIdx).rarIndex := enq.bits.rarIndex
      flags(enqIdx).rawAllocated := enq.bits.rawAllocated
      flags(enqIdx).rawIndex := enq.bits.rawIndex

      // reset block counter
      blockCounter(enqIdx) := 0.U // reset count
    }
  }

  when(io.refill.valid) {
    XSDebug("miss resp: paddr:0x%x data %x\n", io.refill.bits.addr, io.refill.bits.data)
  }

  // misprediction recovery / exception redirect
  val needCancel = Wire(Vec(LoadQueueReplaySize, Bool()))
  for (i <- 0 until LoadQueueReplaySize) {
    needCancel(i) := uop(i).robIdx.needFlush(io.redirect) && allocated(i)
    when (needCancel(i)) {
      allocated(i) := false.B
    }
  }  

  io.lqFull := lqFull

  //  perf cnt
  val enqCount = PopCount(io.enq.map(_.valid)) 
  val deqCount = PopCount(io.replay.map(_.fire)) 
  val deqBlockCount = PopCount(io.replay.map(r => r.valid && !r.ready))
  val replayRejectEnqCount = PopCount(io.enq.map(enq => enq.valid && enq.bits.replayInfo.cause(LoadReplayCauses.rejectEnq)))
  val replaySchedErrorCount = PopCount(io.enq.map(enq => enq.valid && enq.bits.replayInfo.cause(LoadReplayCauses.schedError)))
  val replayWaitStoreCount = PopCount(io.enq.map(enq => enq.valid && enq.bits.replayInfo.cause(LoadReplayCauses.waitStore)))
  val replayTlbMissCount = PopCount(io.enq.map(enq => enq.valid && enq.bits.replayInfo.cause(LoadReplayCauses.tlbMiss)))
  val replayBankConflictCount = PopCount(io.enq.map(enq => enq.valid && enq.bits.replayInfo.cause(LoadReplayCauses.bankConflict)))
  val replayDCacheReplayCount = PopCount(io.enq.map(enq => enq.valid && enq.bits.replayInfo.cause(LoadReplayCauses.dcacheReplay)))
  val replayForwardFailCount = PopCount(io.enq.map(enq => enq.valid && enq.bits.replayInfo.cause(LoadReplayCauses.forwardFail)))
  val replayDCacheMissCount = PopCount(io.enq.map(enq => enq.valid && enq.bits.replayInfo.cause(LoadReplayCauses.dcacheMiss)))
  XSPerfAccumulate("enq", enqCount)
  XSPerfAccumulate("deq", deqCount)
  XSPerfAccumulate("deq_block", deqBlockCount)
  XSPerfAccumulate("replay_full", io.lqFull)
  XSPerfAccumulate("replay_reject_enq", replayRejectEnqCount)
  XSPerfAccumulate("replay_sched_error", replaySchedErrorCount)
  XSPerfAccumulate("replay_wait_store", replayWaitStoreCount)
  XSPerfAccumulate("replay_tlb_miss", replayTlbMissCount)
  XSPerfAccumulate("replay_bank_conflict", replayBankConflictCount)
  XSPerfAccumulate("replay_dcache_replay", replayDCacheReplayCount)
  XSPerfAccumulate("replay_forward_fail", replayForwardFailCount)
  XSPerfAccumulate("replay_dcache_miss", replayDCacheMissCount)

  val perfEvents: Seq[(String, UInt)] = Seq(
    ("enq", enqCount),
    ("deq", deqCount),
    ("deq_block", deqBlockCount),
    ("replay_full", io.lqFull),
    ("replay_reject_enq", replayRejectEnqCount),
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