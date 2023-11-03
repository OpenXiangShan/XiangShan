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

package xiangshan.backend.dispatch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import utils._
import utility._
import xiangshan.ExceptionNO._
import xiangshan._
import xiangshan.backend.MemCoreTopDownIO
import xiangshan.backend.rob.{RobDispatchTopDownIO, RobEnqIO}
import xiangshan.mem.mdp._
import xiangshan.backend.Bundles.DynInst
import xiangshan.backend.fu.FuType

case class DispatchParameters
(
  IntDqSize: Int,
  FpDqSize: Int,
  LsDqSize: Int,
  IntDqDeqWidth: Int,
  FpDqDeqWidth: Int,
  LsDqDeqWidth: Int
)

class CoreDispatchTopDownIO extends Bundle {
  val l2MissMatch = Input(Bool())
  val l3MissMatch = Input(Bool())
  val fromMem = Flipped(new MemCoreTopDownIO)
}

// read rob and enqueue
class Dispatch(implicit p: Parameters) extends XSModule with HasPerfEvents {
  val io = IO(new Bundle() {
    val hartId = Input(UInt(8.W))
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new DynInst)))
    val recv = Output(Vec(RenameWidth, Bool()))
    // enq Rob
    val enqRob = Flipped(new RobEnqIO)
    // enq Lsq
    val allocPregs = Vec(RenameWidth, Output(new ResetPregStateReq))
    // to dispatch queue
    val toIntDq = new Bundle {
      val canAccept = Input(Bool())
      val needAlloc = Vec(RenameWidth, Output(Bool()))
      val req = Vec(RenameWidth, ValidIO(new DynInst))
    }
    val toFpDq = new Bundle {
      val canAccept = Input(Bool())
      val needAlloc = Vec(RenameWidth, Output(Bool()))
      val req = Vec(RenameWidth, ValidIO(new DynInst))
    }
    val toLsDq = new Bundle {
      val canAccept = Input(Bool())
      val needAlloc = Vec(RenameWidth, Output(Bool()))
      val req = Vec(RenameWidth, ValidIO(new DynInst))
    }
    val redirect = Flipped(ValidIO(new Redirect))
    // singleStep
    val singleStep = Input(Bool())
    // lfst
    val lfst = new DispatchLFSTIO
    // perf only
    val robHead = Input(new DynInst)
    val stallReason = Flipped(new StallReasonIO(RenameWidth))
    val lqCanAccept = Input(Bool())
    val sqCanAccept = Input(Bool())
    val robHeadNotReady = Input(Bool())
    val robFull = Input(Bool())
    val debugTopDown = new Bundle {
      val fromRob = Flipped(new RobDispatchTopDownIO)
      val fromCore = new CoreDispatchTopDownIO
    }
  })

  /**
    * Part 1: choose the target dispatch queue and the corresponding write ports
    */
  // valid bits for different dispatch queues
  val isInt    = VecInit(io.fromRename.map(req => FuType.isInt(req.bits.fuType)))
  val isBranch = VecInit(io.fromRename.map(req =>
    // cover auipc (a fake branch)
    !req.bits.preDecodeInfo.notCFI || FuType.isJump(req.bits.fuType)
  ))
  val isFp     = VecInit(io.fromRename.map(req => FuType.isFArith (req.bits.fuType) ||
                                                  FuType.isVArith (req.bits.fuType)))
  val isMem    = VecInit(io.fromRename.map(req => FuType.isMem(req.bits.fuType) ||
                                                  FuType.isVls (req.bits.fuType)))
  val isLs     = VecInit(io.fromRename.map(req => FuType.isLoadStore(req.bits.fuType)))
  val isVls    = VecInit(io.fromRename.map(req => FuType.isVls (req.bits.fuType)))
  val isStore  = VecInit(io.fromRename.map(req => FuType.isStore(req.bits.fuType)))
  val isVStore = VecInit(io.fromRename.map(req => FuType.isVStore(req.bits.fuType)))
  val isAMO    = VecInit(io.fromRename.map(req => FuType.isAMO(req.bits.fuType)))
  val isBlockBackward = VecInit(io.fromRename.map(_.bits.blockBackward))
  val isWaitForward    = VecInit(io.fromRename.map(_.bits.waitForward))

  val singleStepStatus = RegInit(false.B)
  when (io.redirect.valid) {
    singleStepStatus := false.B
  }.elsewhen (io.singleStep && io.fromRename(0).fire) {
    singleStepStatus := true.B
  }
  XSDebug(singleStepStatus, "Debug Mode: Singlestep status is asserted\n")

  val updatedUop = Wire(Vec(RenameWidth, new DynInst))
  val checkpoint_id = RegInit(0.U(64.W))
  checkpoint_id := checkpoint_id + PopCount((0 until RenameWidth).map(i =>
    io.fromRename(i).fire
  ))


  for (i <- 0 until RenameWidth) {

    updatedUop(i) := io.fromRename(i).bits
    updatedUop(i).debugInfo.eliminatedMove := io.fromRename(i).bits.eliminatedMove
    // For the LUI instruction: psrc(0) is from register file and should always be zero.
    when (io.fromRename(i).bits.isLUI) {
      updatedUop(i).psrc(0) := 0.U
    }
    //TODO: vec ls mdp
    io.lfst.req(i).valid := io.fromRename(i).fire && updatedUop(i).storeSetHit
    io.lfst.req(i).bits.isstore := isStore(i)
    io.lfst.req(i).bits.ssid := updatedUop(i).ssid
    io.lfst.req(i).bits.robIdx := updatedUop(i).robIdx // speculatively assigned in rename

    // override load delay ctrl signal with store set result
    if(StoreSetEnable) {
      updatedUop(i).loadWaitBit := io.lfst.resp(i).bits.shouldWait
      updatedUop(i).waitForRobIdx := io.lfst.resp(i).bits.robIdx
    } else {
      updatedUop(i).loadWaitBit := isLs(i) && !isStore(i) && io.fromRename(i).bits.loadWaitBit
    }

    // update singleStep
    updatedUop(i).singleStep := io.singleStep && (if (i == 0) singleStepStatus else true.B)
    when (io.fromRename(i).fire) {
      XSDebug(updatedUop(i).trigger.getHitFrontend, s"Debug Mode: inst ${i} has frontend trigger exception\n")
      XSDebug(updatedUop(i).singleStep, s"Debug Mode: inst ${i} has single step exception\n")
    }
    if (env.EnableDifftest) {
      // debug runahead hint
      val debug_runahead_checkpoint_id = Wire(checkpoint_id.cloneType)
      if(i == 0){
        debug_runahead_checkpoint_id := checkpoint_id
      } else {
        debug_runahead_checkpoint_id := checkpoint_id + PopCount((0 until i).map(i =>
          io.fromRename(i).fire
        ))
      }
    }
  }

  // store set perf count
  XSPerfAccumulate("waittable_load_wait", PopCount((0 until RenameWidth).map(i =>
    io.fromRename(i).fire && io.fromRename(i).bits.loadWaitBit && !isStore(i) && isLs(i)
  )))
  XSPerfAccumulate("storeset_load_wait", PopCount((0 until RenameWidth).map(i =>
    io.fromRename(i).fire && updatedUop(i).loadWaitBit && !isStore(i) && isLs(i)
  )))
  XSPerfAccumulate("storeset_load_strict_wait", PopCount((0 until RenameWidth).map(i =>
    io.fromRename(i).fire && updatedUop(i).loadWaitBit && updatedUop(i).loadWaitStrict && !isStore(i) && isLs(i)
  )))
  XSPerfAccumulate("storeset_store_wait", PopCount((0 until RenameWidth).map(i =>
    io.fromRename(i).fire && updatedUop(i).loadWaitBit && isStore(i)
  )))

  /**
    * Part 3:
    *   acquire ROB (all), LSQ (load/store only) and dispatch queue slots
    *   only set valid when all of them provides enough entries
    */
  val allResourceReady = io.enqRob.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept

  // Instructions should enter dispatch queues in order.
  // thisIsBlocked: this instruction is blocked by itself (based on noSpecExec)
  // nextCanOut: next instructions can out (based on blockBackward)
  // notBlockedByPrevious: previous instructions can enqueue
  val hasException = VecInit(io.fromRename.map(
    r => selectFrontend(r.bits.exceptionVec).asUInt.orR || r.bits.singleStep || r.bits.trigger.getHitFrontend))
  val thisIsBlocked = VecInit((0 until RenameWidth).map(i => {
    // for i > 0, when Rob is empty but dispatch1 have valid instructions to enqueue, it's blocked
    if (i > 0) isWaitForward(i) && (!io.enqRob.isEmpty || Cat(io.fromRename.take(i).map(_.valid)).orR)
    else isWaitForward(i) && !io.enqRob.isEmpty
  }))
  val nextCanOut = VecInit((0 until RenameWidth).map(i =>
    (!isWaitForward(i) && !isBlockBackward(i)) || !io.fromRename(i).valid
  ))
  val notBlockedByPrevious = VecInit((0 until RenameWidth).map(i =>
    if (i == 0) true.B
    else Cat((0 until i).map(j => nextCanOut(j))).andR
  ))

  // for noSpecExec: (robEmpty || !this.noSpecExec) && !previous.noSpecExec
  // For blockBackward:
  // this instruction can actually dequeue: 3 conditions
  // (1) resources are ready
  // (2) previous instructions are ready
  val thisCanActualOut = (0 until RenameWidth).map(i => !thisIsBlocked(i) && notBlockedByPrevious(i))
  val thisActualOut = (0 until RenameWidth).map(i => io.enqRob.req(i).valid && io.enqRob.canAccept)
  val hasValidException = io.fromRename.zip(hasException).map(x => x._1.valid && x._2)

  // input for ROB, LSQ, Dispatch Queue
  for (i <- 0 until RenameWidth) {
    io.enqRob.needAlloc(i) := io.fromRename(i).valid
    io.enqRob.req(i).valid := io.fromRename(i).valid && thisCanActualOut(i) && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept
    io.enqRob.req(i).bits := updatedUop(i)
    XSDebug(io.enqRob.req(i).valid, p"pc 0x${Hexadecimal(io.fromRename(i).bits.pc)} receives nrob ${io.enqRob.resp(i)}\n")

    // When previous instructions have exceptions, following instructions should not enter dispatch queues.
    val previousHasException = if (i == 0) false.B else VecInit(hasValidException.take(i)).asUInt.orR
    val canEnterDpq = !hasException(i) && thisCanActualOut(i) && !previousHasException && io.enqRob.canAccept

    // send uops to dispatch queues
    // Note that if one of their previous instructions cannot enqueue, they should not enter dispatch queue.
    val doesNotNeedExec = io.fromRename(i).bits.eliminatedMove
    io.toIntDq.needAlloc(i) := io.fromRename(i).valid && isInt(i) && !doesNotNeedExec
    io.toIntDq.req(i).valid := io.fromRename(i).valid && isInt(i) && !doesNotNeedExec &&
                               canEnterDpq && io.toFpDq.canAccept && io.toLsDq.canAccept
    io.toIntDq.req(i).bits  := updatedUop(i)

    io.toFpDq.needAlloc(i)  := io.fromRename(i).valid && isFp(i)
    io.toFpDq.req(i).valid  := io.fromRename(i).valid && isFp(i) &&
                               canEnterDpq && io.toIntDq.canAccept && io.toLsDq.canAccept
    io.toFpDq.req(i).bits   := updatedUop(i)

    io.toLsDq.needAlloc(i)  := io.fromRename(i).valid && isMem(i)
    io.toLsDq.req(i).valid  := io.fromRename(i).valid && isMem(i) &&
                               canEnterDpq && io.toIntDq.canAccept && io.toFpDq.canAccept
    io.toLsDq.req(i).bits   := updatedUop(i)

    XSDebug(io.toIntDq.req(i).valid, p"pc 0x${Hexadecimal(io.toIntDq.req(i).bits.pc)} int index $i\n")
    XSDebug(io.toFpDq.req(i).valid , p"pc 0x${Hexadecimal(io.toFpDq.req(i).bits.pc )} fp  index $i\n")
    XSDebug(io.toLsDq.req(i).valid , p"pc 0x${Hexadecimal(io.toLsDq.req(i).bits.pc )} ls  index $i\n")
  }

  /**
    * Part 4: send response to rename when dispatch queue accepts the uop
    */
  val hasValidInstr = VecInit(io.fromRename.map(_.valid)).asUInt.orR
  val hasSpecialInstr = Cat((0 until RenameWidth).map(i => io.fromRename(i).valid && (isBlockBackward(i) || isWaitForward(i)))).orR
  for (i <- 0 until RenameWidth) {
    io.recv(i) := thisCanActualOut(i) && io.enqRob.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept
    io.fromRename(i).ready := !hasValidInstr || !hasSpecialInstr && io.enqRob.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept

    XSInfo(io.recv(i) && io.fromRename(i).valid,
      p"pc 0x${Hexadecimal(io.fromRename(i).bits.pc)}, type(${isInt(i)}, ${isFp(i)}, ${isLs(i)}), " +
      p"rob ${updatedUop(i).robIdx})\n"
    )

    io.allocPregs(i).isInt := io.fromRename(i).valid && io.fromRename(i).bits.rfWen && (io.fromRename(i).bits.ldest =/= 0.U) && !io.fromRename(i).bits.eliminatedMove
    io.allocPregs(i).isFp  := io.fromRename(i).valid && (io.fromRename(i).bits.fpWen || io.fromRename(i).bits.vecWen)
    io.allocPregs(i).preg  := io.fromRename(i).bits.pdest
  }
  val renameFireCnt = PopCount(io.recv)
  val enqFireCnt = PopCount(io.toIntDq.req.map(_.valid && io.toIntDq.canAccept)) +
    PopCount(io.toFpDq.req.map(_.valid && io.toFpDq.canAccept)) +
    PopCount(io.toLsDq.req.map(_.valid && io.toLsDq.canAccept))
  XSError(enqFireCnt > renameFireCnt, "enqFireCnt should not be greater than renameFireCnt\n")

  val stall_rob = hasValidInstr && !io.enqRob.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept
  val stall_int_dq = hasValidInstr && io.enqRob.canAccept && !io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept
  val stall_fp_dq = hasValidInstr && io.enqRob.canAccept && io.toIntDq.canAccept && !io.toFpDq.canAccept && io.toLsDq.canAccept
  val stall_ls_dq = hasValidInstr && io.enqRob.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && !io.toLsDq.canAccept

  XSPerfAccumulate("in_valid_count", PopCount(io.fromRename.map(_.valid)))
  XSPerfAccumulate("in_fire_count", PopCount(io.fromRename.zip(io.recv).map { case (inst, ready) => inst.valid && ready }))
  XSPerfAccumulate("in_valid_not_ready_count", PopCount(io.fromRename.zip(io.recv).map { case (inst, ready) => inst.valid && !ready }))
  XSPerfAccumulate("wait_cycle", !io.fromRename.head.valid && allResourceReady)

  XSPerfAccumulate("stall_cycle_rob", stall_rob)
  XSPerfAccumulate("stall_cycle_int_dq", stall_int_dq)
  XSPerfAccumulate("stall_cycle_fp_dq", stall_fp_dq)
  XSPerfAccumulate("stall_cycle_ls_dq", stall_ls_dq)

  val notIssue = io.debugTopDown.fromRob.robHeadLsIssue
  val tlbReplay = io.debugTopDown.fromCore.fromMem.robHeadTlbReplay
  val tlbMiss = io.debugTopDown.fromCore.fromMem.robHeadTlbMiss
  val vioReplay = io.debugTopDown.fromCore.fromMem.robHeadLoadVio
  val mshrReplay = io.debugTopDown.fromCore.fromMem.robHeadLoadMSHR
  val l1Miss = io.debugTopDown.fromCore.fromMem.robHeadMissInDCache
  val l2Miss = io.debugTopDown.fromCore.l2MissMatch
  val l3Miss = io.debugTopDown.fromCore.l3MissMatch

  val ldReason = Mux(l3Miss, TopDownCounters.LoadMemStall.id.U,
  Mux(l2Miss, TopDownCounters.LoadL3Stall.id.U,
  Mux(l1Miss, TopDownCounters.LoadL2Stall.id.U,
  Mux(notIssue, TopDownCounters.MemNotReadyStall.id.U,
  Mux(tlbMiss, TopDownCounters.LoadTLBStall.id.U,
  Mux(tlbReplay, TopDownCounters.LoadTLBStall.id.U,
  Mux(mshrReplay, TopDownCounters.LoadMSHRReplayStall.id.U,
  Mux(vioReplay, TopDownCounters.LoadVioReplayStall.id.U,
  TopDownCounters.LoadL1Stall.id.U))))))))

  val decodeReason = RegNextN(io.stallReason.reason, 2)
  val renameReason = RegNext(io.stallReason.reason)

  val stallReason = Wire(chiselTypeOf(io.stallReason.reason))
  val realFired = io.recv.zip(io.fromRename.map(_.valid)).map(x => x._1 && x._2)
  io.stallReason.backReason.valid := !io.recv.head
  io.stallReason.backReason.bits := TopDownCounters.OtherCoreStall.id.U
  stallReason.zip(io.stallReason.reason).zip(io.recv).zip(realFired).zipWithIndex.map { case ((((update, in), recv), fire), idx) =>
    val headIsInt = FuType.isInt(io.robHead.fuType)  && io.robHeadNotReady
    val headIsFp  = FuType.isFArith(io.robHead.fuType)   && io.robHeadNotReady
    val headIsDiv = FuType.isDivSqrt(io.robHead.fuType) && io.robHeadNotReady
    val headIsLd  = io.robHead.fuType === FuType.ldu.U && io.robHeadNotReady || !io.lqCanAccept
    val headIsSt  = io.robHead.fuType === FuType.stu.U && io.robHeadNotReady || !io.sqCanAccept
    val headIsAmo = io.robHead.fuType === FuType.mou.U && io.robHeadNotReady
    val headIsLs  = headIsLd || headIsSt
    val robLsFull = io.robFull || !io.lqCanAccept || !io.sqCanAccept

    import TopDownCounters._
    update := MuxCase(OtherCoreStall.id.U, Seq(
      // fire
      (fire                                              ) -> NoStall.id.U          ,
      // dispatch not stall / core stall from decode or rename
      (in =/= OtherCoreStall.id.U && in =/= NoStall.id.U ) -> in                    ,
      // dispatch queue stall
      (!io.toIntDq.canAccept && !headIsInt && !io.robFull) -> IntDqStall.id.U       ,
      (!io.toFpDq.canAccept  && !headIsFp  && !io.robFull) -> FpDqStall.id.U        ,
      (!io.toLsDq.canAccept  && !headIsLs  && !robLsFull ) -> LsDqStall.id.U        ,
      // rob stall
      (headIsAmo                                         ) -> AtomicStall.id.U      ,
      (headIsSt                                          ) -> StoreStall.id.U       ,
      (headIsLd                                          ) -> ldReason              ,
      (headIsDiv                                         ) -> DivStall.id.U         ,
      (headIsInt                                         ) -> IntNotReadyStall.id.U ,
      (headIsFp                                          ) -> FPNotReadyStall.id.U  ,
      (renameReason(idx) =/= NoStall.id.U                ) -> renameReason(idx)     ,
      (decodeReason(idx) =/= NoStall.id.U                ) -> decodeReason(idx)     ,
    ))
  }

  TopDownCounters.values.foreach(ctr => XSPerfAccumulate(ctr.toString(), PopCount(stallReason.map(_ === ctr.id.U))))

  val robTrueCommit = io.debugTopDown.fromRob.robTrueCommit
  TopDownCounters.values.foreach(ctr => XSPerfRolling("td_"+ctr.toString(), PopCount(stallReason.map(_ === ctr.id.U)),
                                                      robTrueCommit, 1000, clock, reset))

  XSPerfHistogram("slots_fire", PopCount(thisActualOut), true.B, 0, RenameWidth+1, 1)
  // Explaination: when out(0) not fire, PopCount(valid) is not meaningfull
  XSPerfHistogram("slots_valid_pure", PopCount(io.enqRob.req.map(_.valid)), thisActualOut(0), 0, RenameWidth+1, 1)
  XSPerfHistogram("slots_valid_rough", PopCount(io.enqRob.req.map(_.valid)), true.B, 0, RenameWidth+1, 1)

  val perfEvents = Seq(
    ("dispatch_in",                 PopCount(io.fromRename.map(_.valid & io.fromRename(0).ready))                  ),
    ("dispatch_empty",              !hasValidInstr                                                                 ),
    ("dispatch_utili",              PopCount(io.fromRename.map(_.valid))                                           ),
    ("dispatch_waitinstr",          PopCount((0 until RenameWidth).map(i => io.fromRename(i).valid && !io.recv(i)))),
    ("dispatch_stall_cycle_lsq",    false.B                                                                        ),
    ("dispatch_stall_cycle_rob",    stall_rob                                                                      ),
    ("dispatch_stall_cycle_int_dq", stall_int_dq                                                                   ),
    ("dispatch_stall_cycle_fp_dq",  stall_fp_dq                                                                    ),
    ("dispatch_stall_cycle_ls_dq",  stall_ls_dq                                                                    )
  )
  generatePerfEvent()
}
