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
import xiangshan._
import utils._
import xiangshan.backend.roq.{RoqEnqIO, RoqPtr}
import xiangshan.backend.rename.RenameBypassInfo
import xiangshan.mem.LsqEnqIO
import xiangshan.backend.fu.HasExceptionNO
import xiangshan.backend.decode.{LFST, DispatchToLFST, LookupLFST}


class PreDispatchInfo(implicit p: Parameters) extends XSBundle {
  val lsqNeedAlloc = Vec(RenameWidth, UInt(2.W))
}

// read rob and enqueue
class Dispatch1(implicit p: Parameters) extends XSModule with HasExceptionNO {
  val io = IO(new Bundle() {
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val renameBypass = Input(new RenameBypassInfo)
    val preDpInfo = Input(new PreDispatchInfo)
    val recv = Output(Vec(RenameWidth, Bool()))
    // enq Roq
    val enqRoq = Flipped(new RoqEnqIO)
    // enq Lsq
    val enqLsq = Flipped(new LsqEnqIO)
    val allocPregs = Vec(RenameWidth, Output(new ReplayPregReq))
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
    // to store set LFST
    val lfst = Vec(RenameWidth, Valid(new DispatchToLFST))
    // flush or replay, for LFST
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    // LFST ctrl
    val csrCtrl = Input(new CustomCSRCtrlIO)
    // LFST state sync
    val storeIssue = Vec(StorePipelineWidth, Flipped(Valid(new ExuInput)))
    // singleStep
    val singleStep = Input(Bool())
  })


  /**
    * Store set LFST lookup
    */
  // store set LFST lookup may start from rename for better timing

  val lfst = Module(new LFST)
  lfst.io.redirect <> RegNext(io.redirect)
  lfst.io.flush <> RegNext(io.flush)
  lfst.io.storeIssue <> RegNext(io.storeIssue)
  lfst.io.csrCtrl <> RegNext(io.csrCtrl)
  lfst.io.dispatch := io.lfst

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
    *   Update commitType, psrc(0), psrc(1), psrc(2), old_pdest, roqIdx, lqIdx, sqIdx and singlestep for the uops
    */

  val singleStepStatus = RegInit(false.B)
  when (io.flush) {
    singleStepStatus := false.B
  }.elsewhen (io.singleStep && io.fromRename(0).fire()) {
    singleStepStatus := true.B
  }
  val updatedUop = Wire(Vec(RenameWidth, new MicroOp))
  val updatedCommitType = Wire(Vec(RenameWidth, CommitType()))
  val updatedPsrc1 = Wire(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
  val updatedPsrc2 = Wire(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
  val updatedPsrc3 = Wire(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
  val updatedOldPdest = Wire(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))

  for (i <- 0 until RenameWidth) {
    updatedCommitType(i) := Cat(isLs(i), (isStore(i) && !isAMO(i)) | isBranch(i))
    val pdestBypassedPsrc1 = io.fromRename.take(i).map(_.bits.pdest)
      .zip(if (i == 0) Seq() else io.renameBypass.lsrc1_bypass(i-1).asBools)
      .foldLeft(io.fromRename(i).bits.psrc(0)) {
        (z, next) => Mux(next._2, next._1, z)
      }
    val pdestBypassedPsrc2 = io.fromRename.take(i).map(_.bits.pdest)
      .zip(if (i == 0) Seq() else io.renameBypass.lsrc2_bypass(i-1).asBools)
      .foldLeft(io.fromRename(i).bits.psrc(1)) {
        (z, next) => Mux(next._2, next._1, z)
      }
    val pdestBypassedPsrc3 = io.fromRename.take(i).map(_.bits.pdest)
      .zip(if (i == 0) Seq() else io.renameBypass.lsrc3_bypass(i-1).asBools)
      .foldLeft(io.fromRename(i).bits.psrc(2)) {
        (z, next) => Mux(next._2, next._1, z)
      }
    val pdestBypassedOldPdest = io.fromRename.take(i).map(_.bits.pdest)
      .zip(if (i == 0) Seq() else io.renameBypass.ldest_bypass(i-1).asBools)
      .foldLeft(io.fromRename(i).bits.old_pdest) {
        (z, next) => Mux(next._2, next._1, z)
      }
    updatedPsrc1(i) := pdestBypassedPsrc1
    updatedPsrc2(i) := pdestBypassedPsrc2
    updatedPsrc3(i) := pdestBypassedPsrc3
    updatedOldPdest(i) := pdestBypassedOldPdest

    updatedUop(i) := io.fromRename(i).bits
    // update bypass psrc(0)/psrc(1)/psrc(2)/old_pdest
    updatedUop(i).psrc(0) := updatedPsrc1(i)
    updatedUop(i).psrc(1) := updatedPsrc2(i)
    updatedUop(i).psrc(2) := updatedPsrc3(i)
    updatedUop(i).old_pdest := updatedOldPdest(i)
    if (EnableIntMoveElim) {
      updatedUop(i).debugInfo.eliminatedMove := io.fromRename(i).bits.eliminatedMove
    } else {
      updatedUop(i).debugInfo.eliminatedMove := DontCare
    }
    // update commitType
    updatedUop(i).ctrl.commitType := updatedCommitType(i)
    // update roqIdx, lqIdx, sqIdx
    // updatedUop(i).roqIdx := io.enqRoq.resp(i)
//    XSError(io.fromRename(i).valid && updatedUop(i).roqIdx.asUInt =/= io.enqRoq.resp(i).asUInt, "they should equal")
    updatedUop(i).lqIdx  := io.enqLsq.resp(i).lqIdx
    updatedUop(i).sqIdx  := io.enqLsq.resp(i).sqIdx

    // lookup store set LFST
    lfst.io.lookup.raddr(i) := updatedUop(i).cf.ssid
    lfst.io.lookup.ren(i) := updatedUop(i).cf.storeSetHit

    // override load delay ctrl signal with store set result
    if(StoreSetEnable) {
      // updatedUop(i).cf.loadWaitBit := lfst.io.lookup.rdata(i) // classic store set
      updatedUop(i).cf.loadWaitBit := lfst.io.lookup.rdata(i) && !isStore(i) // store set lite
      // updatedUop(i).cf.loadWaitBit := lfst.io.lookup.rdata(i) && io.fromRename(i).bits.cf.loadWaitBit && !isStore(i) // 2-bit store set
    } else {
      updatedUop(i).cf.loadWaitBit := io.fromRename(i).bits.cf.loadWaitBit && !isStore(i) // wait table does not require store to be delayed
    }

    // update store set LFST
    io.lfst(i).valid := io.fromRename(i).valid && updatedUop(i).cf.storeSetHit && isStore(i)
    // or io.fromRename(i).ready && updatedUop(i).cf.storeSetHit && isStore(i), which is much slower
    io.lfst(i).bits.roqIdx := updatedUop(i).roqIdx
    io.lfst(i).bits.sqIdx := updatedUop(i).sqIdx
    io.lfst(i).bits.ssid := updatedUop(i).cf.ssid

    // update singleStep
    updatedUop(i).ctrl.singleStep := io.singleStep && (if (i == 0) singleStepStatus else true.B)
  }

  // store set perf count
  XSPerfAccumulate("waittable_load_wait", PopCount((0 until RenameWidth).map(i => 
    io.fromRename(i).fire() && io.fromRename(i).bits.cf.loadWaitBit && !isStore(i) && isLs(i)
  )))
  XSPerfAccumulate("storeset_load_wait", PopCount((0 until RenameWidth).map(i => 
    io.fromRename(i).fire() && updatedUop(i).cf.loadWaitBit && !isStore(i) && isLs(i)
  )))
  XSPerfAccumulate("storeset_store_wait", PopCount((0 until RenameWidth).map(i => 
    io.fromRename(i).fire() && updatedUop(i).cf.loadWaitBit && isStore(i)
  )))
  XSPerfAccumulate("loadwait_diffmat_sywy", PopCount((0 until RenameWidth).map(i => 
    io.fromRename(i).fire() && updatedUop(i).cf.loadWaitBit && io.fromRename(i).bits.cf.loadWaitBit &&
    !isStore(i) && isLs(i)
  )))
  XSPerfAccumulate("loadwait_diffmat_sywx", PopCount((0 until RenameWidth).map(i => 
    io.fromRename(i).fire() && updatedUop(i).cf.loadWaitBit && !io.fromRename(i).bits.cf.loadWaitBit &&
    !isStore(i) && isLs(i)
  )))
  XSPerfAccumulate("loadwait_diffmat_sxwy", PopCount((0 until RenameWidth).map(i => 
    io.fromRename(i).fire() && !updatedUop(i).cf.loadWaitBit && io.fromRename(i).bits.cf.loadWaitBit &&
    !isStore(i) && isLs(i)
  )))
  XSPerfAccumulate("loadwait_diffmat_sxwx", PopCount((0 until RenameWidth).map(i => 
    io.fromRename(i).fire() && !updatedUop(i).cf.loadWaitBit && !io.fromRename(i).bits.cf.loadWaitBit &&
    !isStore(i) && isLs(i)
  )))

  /**
    * Part 3:
    *   acquire ROQ (all), LSQ (load/store only) and dispatch queue slots
    *   only set valid when all of them provides enough entries
    */
  val allResourceReady = io.enqLsq.canAccept && io.enqRoq.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept

  // Instructions should enter dispatch queues in order.
  // thisIsBlocked: this instruction is blocked by itself (based on noSpecExec)
  // nextCanOut: next instructions can out (based on blockBackward)
  // notBlockedByPrevious: previous instructions can enqueue
  val hasException = VecInit(io.fromRename.map(r => selectFrontend(r.bits.cf.exceptionVec).asUInt.orR))
  val thisIsBlocked = VecInit((0 until RenameWidth).map(i => {
    // for i > 0, when Roq is empty but dispatch1 have valid instructions to enqueue, it's blocked
    if (i > 0) isNoSpecExec(i) && (!io.enqRoq.isEmpty || Cat(io.fromRename.take(i).map(_.valid)).orR)
    else isNoSpecExec(i) && !io.enqRoq.isEmpty
  }))
  val nextCanOut = VecInit((0 until RenameWidth).map(i =>
    (!isNoSpecExec(i) && !isBlockBackward(i)) || !io.fromRename(i).valid
  ))
  val notBlockedByPrevious = VecInit((0 until RenameWidth).map(i =>
    if (i == 0) true.B
    else Cat((0 until i).map(j => nextCanOut(j))).andR
  ))

  // for noSpecExec: (roqEmpty || !this.noSpecExec) && !previous.noSpecExec
  // For blockBackward:
  // this instruction can actually dequeue: 3 conditions
  // (1) resources are ready
  // (2) previous instructions are ready
  val thisCanActualOut = (0 until RenameWidth).map(i => !thisIsBlocked(i) && notBlockedByPrevious(i))

  // input for ROQ, LSQ, Dispatch Queue
  for (i <- 0 until RenameWidth) {
    io.enqRoq.needAlloc(i) := io.fromRename(i).valid
    io.enqRoq.req(i).valid := io.fromRename(i).valid && thisCanActualOut(i) && io.enqLsq.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept
    io.enqRoq.req(i).bits := updatedUop(i)
    XSDebug(io.enqRoq.req(i).valid, p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} receives nroq ${io.enqRoq.resp(i)}\n")

    io.enqLsq.needAlloc(i) := Mux(io.fromRename(i).valid, io.preDpInfo.lsqNeedAlloc(i), 0.U)
    io.enqLsq.req(i).valid := io.fromRename(i).valid && isLs(i) && thisCanActualOut(i) && io.enqRoq.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept
    io.enqLsq.req(i).bits := updatedUop(i)
    io.enqLsq.req(i).bits.roqIdx := io.enqRoq.resp(i)
    XSDebug(io.enqLsq.req(i).valid,
      p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} receives lq ${io.enqLsq.resp(i).lqIdx} sq ${io.enqLsq.resp(i).sqIdx}\n")

    // send uops to dispatch queues
    // Note that if one of their previous instructions cannot enqueue, they should not enter dispatch queue.
    // We use notBlockedByPrevious here.
    if (EnableIntMoveElim) {
      io.toIntDq.needAlloc(i) := io.fromRename(i).valid && isInt(i) && !io.fromRename(i).bits.eliminatedMove
      io.toIntDq.req(i).valid := io.fromRename(i).valid && !hasException(i) && isInt(i) && thisCanActualOut(i) &&
                             io.enqLsq.canAccept && io.enqRoq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept && !io.fromRename(i).bits.eliminatedMove
    } else {
      io.toIntDq.needAlloc(i) := io.fromRename(i).valid && isInt(i)
      io.toIntDq.req(i).valid := io.fromRename(i).valid && !hasException(i) && isInt(i) && thisCanActualOut(i) &&
                             io.enqLsq.canAccept && io.enqRoq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept
    }
    io.toIntDq.req(i).bits  := updatedUop(i)

    io.toFpDq.needAlloc(i)  := io.fromRename(i).valid && isFp(i)
    io.toFpDq.req(i).bits   := updatedUop(i)
    io.toFpDq.req(i).valid  := io.fromRename(i).valid && !hasException(i) && isFp(i) && thisCanActualOut(i) &&
                           io.enqLsq.canAccept && io.enqRoq.canAccept && io.toIntDq.canAccept && io.toLsDq.canAccept

    io.toLsDq.needAlloc(i)  := io.fromRename(i).valid && isMem(i)
    io.toLsDq.req(i).bits   := updatedUop(i)
    io.toLsDq.req(i).valid  := io.fromRename(i).valid && !hasException(i) && isMem(i) && thisCanActualOut(i) &&
                           io.enqLsq.canAccept && io.enqRoq.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept

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
    io.recv(i) := thisCanActualOut(i) && io.enqLsq.canAccept && io.enqRoq.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept
    io.fromRename(i).ready := !hasValidInstr || !hasSpecialInstr && io.enqLsq.canAccept && io.enqRoq.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept

    XSInfo(io.recv(i) && io.fromRename(i).valid,
      p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)}, type(${isInt(i)}, ${isFp(i)}, ${isLs(i)}), " +
      p"roq ${updatedUop(i).roqIdx}, lq ${updatedUop(i).lqIdx}, sq ${updatedUop(i).sqIdx})\n"
    )

    if (EnableIntMoveElim) {
      io.allocPregs(i).isInt := io.fromRename(i).valid && io.fromRename(i).bits.ctrl.rfWen && (io.fromRename(i).bits.ctrl.ldest =/= 0.U) && !io.fromRename(i).bits.eliminatedMove
    } else {
      io.allocPregs(i).isInt := io.fromRename(i).valid && io.fromRename(i).bits.ctrl.rfWen && (io.fromRename(i).bits.ctrl.ldest =/= 0.U)
    }
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
  XSPerfAccumulate("stall_cycle_lsq", hasValidInstr && !io.enqLsq.canAccept && io.enqRoq.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept)
  XSPerfAccumulate("stall_cycle_roq", hasValidInstr && io.enqLsq.canAccept && !io.enqRoq.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept)
  XSPerfAccumulate("stall_cycle_int_dq", hasValidInstr && io.enqLsq.canAccept && io.enqRoq.canAccept && !io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept)
  XSPerfAccumulate("stall_cycle_fp_dq", hasValidInstr && io.enqLsq.canAccept && io.enqRoq.canAccept && io.toIntDq.canAccept && !io.toFpDq.canAccept && io.toLsDq.canAccept)
  XSPerfAccumulate("stall_cycle_ls_dq", hasValidInstr && io.enqLsq.canAccept && io.enqRoq.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && !io.toLsDq.canAccept)
}
