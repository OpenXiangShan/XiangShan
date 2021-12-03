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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import difftest._
import xiangshan.backend.fu.HasExceptionNO
import xiangshan.backend.rob.RobEnqIO
import xiangshan.mem.LsqEnqIO
import xiangshan.mem.mdp._

case class DispatchParameters
(
  IntDqSize: Int,
  FpDqSize: Int,
  LsDqSize: Int,
  IntDqDeqWidth: Int,
  FpDqDeqWidth: Int,
  LsDqDeqWidth: Int
)

// read rob and enqueue
class Dispatch(implicit p: Parameters) extends XSModule with HasExceptionNO {
  val io = IO(new Bundle() {
    val hartId = Input(UInt(8.W))
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val recv = Output(Vec(RenameWidth, Bool()))
    // enq Rob
    val enqRob = Flipped(new RobEnqIO)
    // enq Lsq
    val allocPregs = Vec(RenameWidth, Output(new ResetPregStateReq))
    // to dispatch queue
    val toIntDq = new Bundle {
      val canAccept = Input(Bool())
      val needAlloc = Vec(RenameWidth, Output(Bool()))
      val req = Vec(RenameWidth, ValidIO(new MicroOp))
    }
    val toFpDq = new Bundle {
      val canAccept = Input(Bool())
      val needAlloc = Vec(RenameWidth, Output(Bool()))
      val req = Vec(RenameWidth, ValidIO(new MicroOp))
    }
    val toLsDq = new Bundle {
      val canAccept = Input(Bool())
      val needAlloc = Vec(RenameWidth, Output(Bool()))
      val req = Vec(RenameWidth, ValidIO(new MicroOp))
    }
    val redirect = Flipped(ValidIO(new Redirect))
    // singleStep
    val singleStep = Input(Bool())
    // lfst
    val lfst = new DispatchLFSTIO
  })

  /**
    * Part 1: choose the target dispatch queue and the corresponding write ports
    */
  // valid bits for different dispatch queues
  val isInt    = VecInit(io.fromRename.map(req => FuType.isIntExu(req.bits.ctrl.fuType)))
  val isBranch = VecInit(io.fromRename.map(req =>
    // cover auipc (a fake branch)
    !req.bits.cf.pd.notCFI || FuType.isJumpExu(req.bits.ctrl.fuType)
  ))
  val isFp     = VecInit(io.fromRename.map(req => FuType.isFpExu (req.bits.ctrl.fuType)))
  val isMem    = VecInit(io.fromRename.map(req => FuType.isMemExu(req.bits.ctrl.fuType)))
  val isLs     = VecInit(io.fromRename.map(req => FuType.isLoadStore(req.bits.ctrl.fuType)))
  val isStore  = VecInit(io.fromRename.map(req => FuType.isStoreExu(req.bits.ctrl.fuType)))
  val isAMO    = VecInit(io.fromRename.map(req => FuType.isAMO(req.bits.ctrl.fuType)))
  val isBlockBackward = VecInit(io.fromRename.map(_.bits.ctrl.blockBackward))
  val isNoSpecExec    = VecInit(io.fromRename.map(_.bits.ctrl.noSpecExec))

  /**
    * Part 2:
    *   Update commitType, psrc(0), psrc(1), psrc(2), old_pdest, robIdx and singlestep for the uops
    */

  val singleStepStatus = RegInit(false.B)
  when (io.redirect.valid) {
    singleStepStatus := false.B
  }.elsewhen (io.singleStep && io.fromRename(0).fire()) {
    singleStepStatus := true.B
  }
  val updatedUop = Wire(Vec(RenameWidth, new MicroOp))
  val updatedCommitType = Wire(Vec(RenameWidth, CommitType()))
  val checkpoint_id = RegInit(0.U(64.W))
  checkpoint_id := checkpoint_id + PopCount((0 until RenameWidth).map(i => 
    io.fromRename(i).fire()
  ))

  for (i <- 0 until RenameWidth) {
    updatedCommitType(i) := Cat(isLs(i), (isStore(i) && !isAMO(i)) | isBranch(i))

    updatedUop(i) := io.fromRename(i).bits
    updatedUop(i).debugInfo.eliminatedMove := io.fromRename(i).bits.eliminatedMove
    // update commitType
    when (!CommitType.isFused(io.fromRename(i).bits.ctrl.commitType)) {
      updatedUop(i).ctrl.commitType := updatedCommitType(i)
    }.otherwise {
      XSError(io.fromRename(i).valid && updatedCommitType(i) =/= CommitType.NORMAL, "why fused?\n")
    }

    io.lfst.req(i).valid := io.fromRename(i).fire() && updatedUop(i).cf.storeSetHit
    io.lfst.req(i).bits.isstore := isStore(i)
    io.lfst.req(i).bits.ssid := updatedUop(i).cf.ssid
    io.lfst.req(i).bits.robIdx := updatedUop(i).robIdx // speculatively assigned in rename

    // override load delay ctrl signal with store set result
    if(StoreSetEnable) {
      updatedUop(i).cf.loadWaitBit := io.lfst.resp(i).bits.shouldWait
      updatedUop(i).cf.waitForRobIdx := io.lfst.resp(i).bits.robIdx
    } else {
      updatedUop(i).cf.loadWaitBit := isLs(i) && !isStore(i) && io.fromRename(i).bits.cf.loadWaitBit
    }

    // update singleStep
    updatedUop(i).ctrl.singleStep := io.singleStep && (if (i == 0) singleStepStatus else true.B)

    if (env.EnableDifftest) {
      // debug runahead hint
      val debug_runahead_checkpoint_id = Wire(checkpoint_id.cloneType)
      if(i == 0){
        debug_runahead_checkpoint_id := checkpoint_id
      } else {
        debug_runahead_checkpoint_id := checkpoint_id + PopCount((0 until i).map(i => 
          io.fromRename(i).fire()
        ))
      }

      val runahead = Module(new DifftestRunaheadEvent)
      runahead.io.clock         := clock
      runahead.io.coreid        := io.hartId
      runahead.io.index         := i.U
      runahead.io.valid         := io.fromRename(i).fire()
      runahead.io.branch        := isBranch(i) // setup checkpoint for branch
      runahead.io.may_replay    := isLs(i) && !isStore(i) // setup checkpoint for load, as load may replay
      runahead.io.pc            := updatedUop(i).cf.pc
      runahead.io.checkpoint_id := debug_runahead_checkpoint_id 

      // when(runahead.io.valid){
      //   printf("XS runahead " + i + " : %d: pc %x branch %x cpid %x\n",
      //     GTimer(),
      //     runahead.io.pc,
      //     runahead.io.branch,
      //     runahead.io.checkpoint_id
      //   );
      // }

      val mempred_check = Module(new DifftestRunaheadMemdepPred)
      mempred_check.io.clock     := clock
      mempred_check.io.coreid    := io.hartId
      mempred_check.io.index     := i.U
      mempred_check.io.valid     := io.fromRename(i).fire() && isLs(i)
      mempred_check.io.is_load   := !isStore(i) && isLs(i)
      mempred_check.io.need_wait := updatedUop(i).cf.loadWaitBit
      mempred_check.io.pc        := updatedUop(i).cf.pc 

      when(RegNext(mempred_check.io.valid)){
        XSDebug("mempred_check " + i + " : %d: pc %x ld %x need_wait %x oracle va %x\n",
          RegNext(GTimer()),
          RegNext(mempred_check.io.pc),
          RegNext(mempred_check.io.is_load),
          RegNext(mempred_check.io.need_wait),
          mempred_check.io.oracle_vaddr 
        );
      }
      updatedUop(i).debugInfo.runahead_checkpoint_id := debug_runahead_checkpoint_id
    }
  }

  // store set perf count
  XSPerfAccumulate("waittable_load_wait", PopCount((0 until RenameWidth).map(i =>
    io.fromRename(i).fire() && io.fromRename(i).bits.cf.loadWaitBit && !isStore(i) && isLs(i)
  )))
  XSPerfAccumulate("storeset_load_wait", PopCount((0 until RenameWidth).map(i =>
    io.fromRename(i).fire() && updatedUop(i).cf.loadWaitBit && !isStore(i) && isLs(i)
  )))
  XSPerfAccumulate("storeset_load_strict_wait", PopCount((0 until RenameWidth).map(i =>
    io.fromRename(i).fire() && updatedUop(i).cf.loadWaitBit && updatedUop(i).cf.loadWaitStrict && !isStore(i) && isLs(i)
  )))
  XSPerfAccumulate("storeset_store_wait", PopCount((0 until RenameWidth).map(i =>
    io.fromRename(i).fire() && updatedUop(i).cf.loadWaitBit && isStore(i)
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
  val hasException = VecInit(io.fromRename.map(r => selectFrontend(r.bits.cf.exceptionVec).asUInt.orR))
  val thisIsBlocked = VecInit((0 until RenameWidth).map(i => {
    // for i > 0, when Rob is empty but dispatch1 have valid instructions to enqueue, it's blocked
    if (i > 0) isNoSpecExec(i) && (!io.enqRob.isEmpty || Cat(io.fromRename.take(i).map(_.valid)).orR)
    else isNoSpecExec(i) && !io.enqRob.isEmpty
  }))
  val nextCanOut = VecInit((0 until RenameWidth).map(i =>
    (!isNoSpecExec(i) && !isBlockBackward(i)) || !io.fromRename(i).valid
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

  // input for ROB, LSQ, Dispatch Queue
  for (i <- 0 until RenameWidth) {
    io.enqRob.needAlloc(i) := io.fromRename(i).valid
    io.enqRob.req(i).valid := io.fromRename(i).valid && thisCanActualOut(i) && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept
    io.enqRob.req(i).bits := updatedUop(i)
    XSDebug(io.enqRob.req(i).valid, p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} receives nrob ${io.enqRob.resp(i)}\n")

    // send uops to dispatch queues
    // Note that if one of their previous instructions cannot enqueue, they should not enter dispatch queue.
    // We use notBlockedByPrevious here.
    io.toIntDq.needAlloc(i) := io.fromRename(i).valid && isInt(i) && !io.fromRename(i).bits.eliminatedMove
    io.toIntDq.req(i).valid := io.fromRename(i).valid && !hasException(i) && isInt(i) && thisCanActualOut(i) &&
                           io.enqRob.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept && !io.fromRename(i).bits.eliminatedMove
    io.toIntDq.req(i).bits  := updatedUop(i)

    io.toFpDq.needAlloc(i)  := io.fromRename(i).valid && isFp(i)
    io.toFpDq.req(i).bits   := updatedUop(i)
    io.toFpDq.req(i).valid  := io.fromRename(i).valid && !hasException(i) && isFp(i) && thisCanActualOut(i) &&
                           io.enqRob.canAccept && io.toIntDq.canAccept && io.toLsDq.canAccept

    io.toLsDq.needAlloc(i)  := io.fromRename(i).valid && isMem(i)
    io.toLsDq.req(i).bits   := updatedUop(i)
    io.toLsDq.req(i).valid  := io.fromRename(i).valid && !hasException(i) && isMem(i) && thisCanActualOut(i) &&
                           io.enqRob.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept

    XSDebug(io.toIntDq.req(i).valid, p"pc 0x${Hexadecimal(io.toIntDq.req(i).bits.cf.pc)} int index $i\n")
    XSDebug(io.toFpDq.req(i).valid , p"pc 0x${Hexadecimal(io.toFpDq.req(i).bits.cf.pc )} fp  index $i\n")
    XSDebug(io.toLsDq.req(i).valid , p"pc 0x${Hexadecimal(io.toLsDq.req(i).bits.cf.pc )} ls  index $i\n")
  }

  /**
    * Part 4: send response to rename when dispatch queue accepts the uop
    */
  val hasValidInstr = VecInit(io.fromRename.map(_.valid)).asUInt.orR
  val hasSpecialInstr = Cat((0 until RenameWidth).map(i => io.fromRename(i).valid && (isBlockBackward(i) || isNoSpecExec(i)))).orR
  for (i <- 0 until RenameWidth) {
    io.recv(i) := thisCanActualOut(i) && io.enqRob.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept
    io.fromRename(i).ready := !hasValidInstr || !hasSpecialInstr && io.enqRob.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept

    XSInfo(io.recv(i) && io.fromRename(i).valid,
      p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)}, type(${isInt(i)}, ${isFp(i)}, ${isLs(i)}), " +
      p"rob ${updatedUop(i).robIdx})\n"
    )

    io.allocPregs(i).isInt := io.fromRename(i).valid && io.fromRename(i).bits.ctrl.rfWen && (io.fromRename(i).bits.ctrl.ldest =/= 0.U) && !io.fromRename(i).bits.eliminatedMove
    io.allocPregs(i).isFp  := io.fromRename(i).valid && io.fromRename(i).bits.ctrl.fpWen
    io.allocPregs(i).preg  := io.fromRename(i).bits.pdest
  }
  val renameFireCnt = PopCount(io.recv)
  val enqFireCnt = PopCount(io.toIntDq.req.map(_.valid && io.toIntDq.canAccept)) +
    PopCount(io.toFpDq.req.map(_.valid && io.toFpDq.canAccept)) +
    PopCount(io.toLsDq.req.map(_.valid && io.toLsDq.canAccept))
  XSError(enqFireCnt > renameFireCnt, "enqFireCnt should not be greater than renameFireCnt\n")

  XSPerfAccumulate("in", Mux(RegNext(io.fromRename(0).ready), PopCount(io.fromRename.map(_.valid)), 0.U))
  XSPerfAccumulate("empty", !hasValidInstr)
  XSPerfAccumulate("utilization", PopCount(io.fromRename.map(_.valid)))
  XSPerfAccumulate("waitInstr", PopCount((0 until RenameWidth).map(i => io.fromRename(i).valid && !io.recv(i))))
  XSPerfAccumulate("stall_cycle_rob", hasValidInstr && !io.enqRob.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept)
  XSPerfAccumulate("stall_cycle_int_dq", hasValidInstr && io.enqRob.canAccept && !io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept)
  XSPerfAccumulate("stall_cycle_fp_dq", hasValidInstr && io.enqRob.canAccept && io.toIntDq.canAccept && !io.toFpDq.canAccept && io.toLsDq.canAccept)
  XSPerfAccumulate("stall_cycle_ls_dq", hasValidInstr && io.enqRob.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && !io.toLsDq.canAccept)

  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(9))
  })
  val perfEvents = Seq(
    ("dispatch_in                 ", PopCount(io.fromRename.map(_.valid & io.fromRename(0).ready))                                                                       ),
    ("dispatch_empty              ", !hasValidInstr                                                                                                                      ),
    ("dispatch_utili              ", PopCount(io.fromRename.map(_.valid))                                                                                                ),
    ("dispatch_waitinstr          ", PopCount((0 until RenameWidth).map(i => io.fromRename(i).valid && !io.recv(i)))                                                     ),
    ("dispatch_stall_cycle_lsq    ", false.B  ),
    ("dispatch_stall_cycle_rob    ", hasValidInstr && !io.enqRob.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept  ),
    ("dispatch_stall_cycle_int_dq ", hasValidInstr && io.enqRob.canAccept && !io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept  ),
    ("dispatch_stall_cycle_fp_dq  ", hasValidInstr && io.enqRob.canAccept && io.toIntDq.canAccept && !io.toFpDq.canAccept && io.toLsDq.canAccept  ),
    ("dispatch_stall_cycle_ls_dq  ", hasValidInstr && io.enqRob.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && !io.toLsDq.canAccept  ),
  )

  for (((perf_out,(perf_name,perf)),i) <- perfinfo.perfEvents.perf_events.zip(perfEvents).zipWithIndex) {
    perf_out.incr_step := RegNext(perf)
  }
}
