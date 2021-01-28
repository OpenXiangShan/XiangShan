package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import chisel3.ExcitingUtils._
import xiangshan._
import utils._
import xiangshan.backend.roq.{RoqPtr, RoqEnqIO}
import xiangshan.backend.rename.RenameBypassInfo
import xiangshan.mem.LsqEnqIO
import xiangshan.backend.fu.HasExceptionNO

// read rob and enqueue
class Dispatch1 extends XSModule with HasExceptionNO {
  val io = IO(new Bundle() {
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val renameBypass = Input(new RenameBypassInfo)
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
  })


  /**
    * Part 1: choose the target dispatch queue and the corresponding write ports
    */
  // valid bits for different dispatch queues
  val isInt    = VecInit(io.fromRename.map(req => FuType.isIntExu(req.bits.ctrl.fuType)))
  val isBranch = VecInit(io.fromRename.map(req =>
    // cover auipc (a fake branch)
    !req.bits.cf.brUpdate.pd.notCFI || FuType.isJumpExu(req.bits.ctrl.fuType)
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
    *   Update commitType, psrc1, psrc2, psrc3, old_pdest, roqIdx, lqIdx, sqIdx for the uops
    */
  val updatedUop = Wire(Vec(RenameWidth, new MicroOp))
  val updatedCommitType = Wire(Vec(RenameWidth, CommitType()))
  val updatedPsrc1 = Wire(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
  val updatedPsrc2 = Wire(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
  val updatedPsrc3 = Wire(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
  val updatedOldPdest = Wire(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))

  for (i <- 0 until RenameWidth) {
    updatedCommitType(i) := Cat(isLs(i), (isStore(i) && !isAMO(i)) | isBranch(i))
    updatedPsrc1(i) := io.fromRename.take(i).map(_.bits.pdest)
      .zip(if (i == 0) Seq() else io.renameBypass.lsrc1_bypass(i-1).asBools)
      .foldLeft(io.fromRename(i).bits.psrc1) {
        (z, next) => Mux(next._2, next._1, z)
      }
    updatedPsrc2(i) := io.fromRename.take(i).map(_.bits.pdest)
      .zip(if (i == 0) Seq() else io.renameBypass.lsrc2_bypass(i-1).asBools)
      .foldLeft(io.fromRename(i).bits.psrc2) {
        (z, next) => Mux(next._2, next._1, z)
      }
    updatedPsrc3(i) := io.fromRename.take(i).map(_.bits.pdest)
      .zip(if (i == 0) Seq() else io.renameBypass.lsrc3_bypass(i-1).asBools)
      .foldLeft(io.fromRename(i).bits.psrc3) {
        (z, next) => Mux(next._2, next._1, z)
      }
    updatedOldPdest(i) := io.fromRename.take(i).map(_.bits.pdest)
      .zip(if (i == 0) Seq() else io.renameBypass.ldest_bypass(i-1).asBools)
      .foldLeft(io.fromRename(i).bits.old_pdest) {
        (z, next) => Mux(next._2, next._1, z)
      }

    updatedUop(i) := io.fromRename(i).bits
    // update bypass psrc1/psrc2/psrc3/old_pdest
    updatedUop(i).psrc1 := updatedPsrc1(i)
    updatedUop(i).psrc2 := updatedPsrc2(i)
    updatedUop(i).psrc3 := updatedPsrc3(i)
    updatedUop(i).old_pdest := updatedOldPdest(i)
    // update commitType
    updatedUop(i).ctrl.commitType := updatedCommitType(i)
    // update roqIdx, lqIdx, sqIdx
    // updatedUop(i).roqIdx := io.enqRoq.resp(i)
    XSError(io.fromRename(i).valid && updatedUop(i).roqIdx.asUInt =/= io.enqRoq.resp(i).asUInt, "they should equal")
    updatedUop(i).lqIdx  := io.enqLsq.resp(i).lqIdx
    updatedUop(i).sqIdx  := io.enqLsq.resp(i).sqIdx
  }


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

    io.enqLsq.needAlloc(i) := io.fromRename(i).valid && isLs(i)
    io.enqLsq.req(i).valid := io.fromRename(i).valid && isLs(i) && thisCanActualOut(i) && io.enqRoq.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept
    io.enqLsq.req(i).bits := updatedUop(i)
    io.enqLsq.req(i).bits.roqIdx := io.enqRoq.resp(i)
    XSDebug(io.enqLsq.req(i).valid,
      p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} receives lq ${io.enqLsq.resp(i).lqIdx} sq ${io.enqLsq.resp(i).sqIdx}\n")

    // send uops to dispatch queues
    // Note that if one of their previous instructions cannot enqueue, they should not enter dispatch queue.
    // We use notBlockedByPrevious here.
    io.toIntDq.needAlloc(i) := io.fromRename(i).valid && isInt(i)
    io.toIntDq.req(i).bits  := updatedUop(i)
    io.toIntDq.req(i).valid := io.fromRename(i).valid && !hasException(i) && isInt(i) && thisCanActualOut(i) &&
                           io.enqLsq.canAccept && io.enqRoq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept

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
  val hasSpecialInstr = Cat((0 until RenameWidth).map(i => io.fromRename(i).valid && (isBlockBackward(i) || isNoSpecExec(i)))).orR
  for (i <- 0 until RenameWidth) {
    io.recv(i) := thisCanActualOut(i) && io.enqLsq.canAccept && io.enqRoq.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept
    io.fromRename(i).ready := !hasSpecialInstr && io.enqLsq.canAccept && io.enqRoq.canAccept && io.toIntDq.canAccept && io.toFpDq.canAccept && io.toLsDq.canAccept

    XSInfo(io.recv(i) && io.fromRename(i).valid,
      p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)}, type(${isInt(i)}, ${isFp(i)}, ${isLs(i)}), " +
      p"roq ${updatedUop(i).roqIdx}, lq ${updatedUop(i).lqIdx}, sq ${updatedUop(i).sqIdx})\n"
    )

    io.allocPregs(i).isInt := io.fromRename(i).valid && io.fromRename(i).bits.ctrl.rfWen && (io.fromRename(i).bits.ctrl.ldest =/= 0.U)
    io.allocPregs(i).isFp  := io.fromRename(i).valid && io.fromRename(i).bits.ctrl.fpWen
    io.allocPregs(i).preg  := io.fromRename(i).bits.pdest
  }
  val renameFireCnt = PopCount(io.recv)
  val enqFireCnt = PopCount(io.toIntDq.req.map(_.valid && io.toIntDq.canAccept)) +
    PopCount(io.toFpDq.req.map(_.valid && io.toFpDq.canAccept)) +
    PopCount(io.toLsDq.req.map(_.valid && io.toLsDq.canAccept))
  XSError(enqFireCnt > renameFireCnt, "enqFireCnt should not be greater than renameFireCnt\n")

  XSPerf("utilization", PopCount(io.fromRename.map(_.valid)))
  XSPerf("waitInstr", PopCount((0 until RenameWidth).map(i => io.fromRename(i).valid && !io.recv(i))))
}
