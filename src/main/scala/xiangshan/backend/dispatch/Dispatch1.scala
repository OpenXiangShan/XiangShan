package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import chisel3.ExcitingUtils._
import xiangshan._
import utils.{XSDebug, XSError, XSInfo}
import xiangshan.backend.roq.RoqPtr
import xiangshan.backend.rename.RenameBypassInfo

// read rob and enqueue
class Dispatch1 extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val renameBypass = Input(new RenameBypassInfo)
    val recv = Output(Vec(RenameWidth, Bool()))
    // enq Roq
    val enqRoq = new Bundle {
      val canAccept = Input(Bool())
      val isEmpty = Input(Bool())
      // if set, Roq needs extra walk
      val extraWalk = Vec(RenameWidth, Output(Bool()))
      val req = Vec(RenameWidth, ValidIO(new MicroOp))
      val resp = Vec(RenameWidth, Input(new RoqPtr))
    }
    // enq Lsq
    val enqLsq = new Bundle() {
      val canAccept = Input(Bool())
      val req = Vec(RenameWidth, ValidIO(new MicroOp))
      val resp = Vec(RenameWidth, Input(new LSIdx))
    }
    val allocPregs = Vec(RenameWidth, Output(new ReplayPregReq))
    // to dispatch queue
    val toIntDqReady = Input(Bool())
    val toIntDq = Vec(dpParams.DqEnqWidth, ValidIO(new MicroOp))
    val toFpDqReady = Input(Bool())
    val toFpDq = Vec(dpParams.DqEnqWidth, ValidIO(new MicroOp))
    val toLsDqReady = Input(Bool())
    val toLsDq = Vec(dpParams.DqEnqWidth, ValidIO(new MicroOp))
  })


  /**
    * Part 1: choose the target dispatch queue and the corresponding write ports
    */
  // valid bits for different dispatch queues
  val isInt   = VecInit(io.fromRename.map(req => FuType.isIntExu(req.bits.ctrl.fuType)))
  val isFp    = VecInit(io.fromRename.map(req => FuType.isFpExu (req.bits.ctrl.fuType)))
  val isLs    = VecInit(io.fromRename.map(req => FuType.isMemExu(req.bits.ctrl.fuType)))
  val isStore = VecInit(io.fromRename.map(req => FuType.isStoreExu(req.bits.ctrl.fuType)))
  val isBlockBackward = VecInit(io.fromRename.map(_.bits.ctrl.blockBackward))
  val isNoSpecExec    = VecInit(io.fromRename.map(_.bits.ctrl.noSpecExec))

  // generate index mapping
  val intIndex = Module(new IndexMapping(RenameWidth, dpParams.DqEnqWidth, false))
  val fpIndex  = Module(new IndexMapping(RenameWidth, dpParams.DqEnqWidth, false))
  val lsIndex  = Module(new IndexMapping(RenameWidth, dpParams.DqEnqWidth, false))
  for (i <- 0 until RenameWidth) {
    intIndex.io.validBits(i) := isInt(i) && io.fromRename(i).valid
    fpIndex.io.validBits(i)  := isFp(i)  && io.fromRename(i).valid
    lsIndex.io.validBits(i)  := isLs(i)  && io.fromRename(i).valid
  }
  intIndex.io.priority := DontCare
  fpIndex.io.priority  := DontCare
  lsIndex.io.priority  := DontCare

  if (!env.FPGAPlatform) {
    val dispatchNotEmpty = Cat(io.fromRename.map(_.valid)).orR
    ExcitingUtils.addSource(!dispatchNotEmpty, "perfCntCondDp1Empty", Perf)
  }


  /**
    * Part 2:
    *   Update commitType, psrc1, psrc2, psrc3, old_pdest for the uops
    */
  val updatedUop = Wire(Vec(RenameWidth, new MicroOp))
  val updatedCommitType = Wire(Vec(RenameWidth, CommitType()))
  val updatedPsrc1 = Wire(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
  val updatedPsrc2 = Wire(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
  val updatedPsrc3 = Wire(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))
  val updatedOldPdest = Wire(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))

  for (i <- 0 until RenameWidth) {
    updatedCommitType(i) := Cat(isLs(i), isStore(i) | isFp(i))
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
    XSError(updatedUop(i).psrc1 =/= io.fromRename(i).bits.psrc1, "psrc1 bypass not working correctly\n")
    XSError(updatedUop(i).psrc2 =/= io.fromRename(i).bits.psrc2, "psrc2 bypass not working correctly\n")
    XSError(updatedUop(i).psrc3 =/= io.fromRename(i).bits.psrc3, "psrc3 bypass not working correctly\n")
    XSError(updatedUop(i).old_pdest =/= io.fromRename(i).bits.old_pdest, "old_pdest bypass not working correctly\n")
    // update commitType
    updatedUop(i).ctrl.commitType := updatedCommitType(i)
  }


  /**
    * Part 3:
    *   acquire ROQ (all), LSQ (load/store only) and dispatch queue slots
    *   only set valid when all of them provides enough entries
    */
  val redirectValid = io.redirect.valid && !io.redirect.bits.isReplay
  val allResourceReady = io.enqLsq.canAccept && io.enqRoq.canAccept && io.toIntDqReady && io.toFpDqReady && io.toLsDqReady

  // Instructions should enter dispatch queues in order.
  // When RenameWidth > DqEnqWidth, it's possible that some instructions cannot enter dispatch queue
  // because previous instructions cannot enter dispatch queue.
  // The reason is that although ROB and LSQ have enough empty slots, dispatch queue has limited enqueue ports.
  // Thus, for i >= dpParams.DqEnqWidth, we have to check whether it's previous instructions (and the instruction itself) can enqueue.
  // However, since, for instructions with indices less than dpParams.DqEnqWidth,
  // they can always enter dispatch queue when ROB and LSQ are ready, we don't need to check whether they can enqueue.
  // thisIsBlocked: this instruction is blocked by itself
  // thisCanOut: this instruction can enqueue
  // nextCanOut: next instructions can out
  // notBlockedByPrevious: previous instructions can enqueue
  val thisIsBlocked = VecInit((0 until RenameWidth).map(i =>
    isNoSpecExec(i) && !io.enqRoq.isEmpty
  ))
  val thisCanOut = VecInit((0 until RenameWidth).map(i => {
    // For i in [0, DqEnqWidth), they can always enqueue when ROB and LSQ are ready
    if (i < dpParams.DqEnqWidth) !thisIsBlocked(i)
    else Cat(Seq(intIndex, fpIndex, lsIndex).map(_.io.reverseMapping(i).valid)).orR && !thisIsBlocked(i)
  }))
  val nextCanOut = VecInit((0 until RenameWidth).map(i =>
    (thisCanOut(i) && !isBlockBackward(i)) || !io.fromRename(i).valid
  ))
  val notBlockedByPrevious = VecInit((0 until RenameWidth).map(i =>
    if (i == 0) true.B
    else Cat((0 until i).map(j => nextCanOut(j))).andR
  ))

  // this instruction can actually dequeue: 3 conditions
  // (1) resources are ready
  // (2) previous instructions are ready
  val thisCanActualOut = (0 until RenameWidth).map(i => allResourceReady && thisCanOut(i) && notBlockedByPrevious(i))

  // input for ROQ and LSQ
  // note that LSQ needs roqIdx
  for (i <- 0 until RenameWidth) {
    io.enqRoq.extraWalk(i) := io.fromRename(i).valid && !thisCanActualOut(i)
    io.enqRoq.req(i).valid := io.fromRename(i).valid && thisCanActualOut(i)
    io.enqRoq.req(i).bits := updatedUop(i)

    val shouldEnqLsq = isLs(i) && io.fromRename(i).bits.ctrl.fuType =/= FuType.mou
    io.enqLsq.req(i).valid := io.fromRename(i).valid && shouldEnqLsq && !redirectValid && thisCanActualOut(i)
    io.enqLsq.req(i).bits := updatedUop(i)
    io.enqLsq.req(i).bits.roqIdx := io.enqRoq.resp(i)

    XSDebug(io.enqLsq.req(i).valid,
      p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} receives lq ${io.enqLsq.resp(i).lqIdx} sq ${io.enqLsq.resp(i).sqIdx}\n")

    XSDebug(io.enqRoq.req(i).valid, p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} receives nroq ${io.enqRoq.resp(i)}\n")
  }


  /**
    * Part 4:
    *   append ROQ and LSQ indexed to uop, and send them to dispatch queue
    */
  val updateUopWithIndex = Wire(Vec(RenameWidth, new MicroOp))
  for (i <- 0 until RenameWidth) {
    updateUopWithIndex(i)        := updatedUop(i)
    updateUopWithIndex(i).roqIdx := io.enqRoq.resp(i)
    updateUopWithIndex(i).lqIdx  := io.enqLsq.resp(i).lqIdx
    updateUopWithIndex(i).sqIdx  := io.enqLsq.resp(i).sqIdx
  }

  // send uops with correct indexes to dispatch queues
  // Note that if one of their previous instructions cannot enqueue, they should not enter dispatch queue.
  // We use notBlockedByPrevious here since mapping(i).valid implies there's a valid instruction that can enqueue,
  // thus we don't need to check thisCanOut.
  for (i <- 0 until dpParams.DqEnqWidth) {
    io.toIntDq(i).bits  := updateUopWithIndex(intIndex.io.mapping(i).bits)
    io.toIntDq(i).valid := intIndex.io.mapping(i).valid && allResourceReady &&
                           !thisIsBlocked(intIndex.io.mapping(i).bits) && notBlockedByPrevious(intIndex.io.mapping(i).bits)

    // NOTE: floating point instructions are not noSpecExec currently
    // remove commit /**/ when fp instructions are possible to be noSpecExec
    io.toFpDq(i).bits   := updateUopWithIndex(fpIndex.io.mapping(i).bits)
    io.toFpDq(i).valid  := fpIndex.io.mapping(i).valid && allResourceReady &&
                           /*!thisIsBlocked(fpIndex.io.mapping(i).bits) && */notBlockedByPrevious(fpIndex.io.mapping(i).bits)

    io.toLsDq(i).bits   := updateUopWithIndex(lsIndex.io.mapping(i).bits)
    io.toLsDq(i).valid  := lsIndex.io.mapping(i).valid && allResourceReady &&
                           !thisIsBlocked(lsIndex.io.mapping(i).bits) && notBlockedByPrevious(lsIndex.io.mapping(i).bits)

    XSDebug(io.toIntDq(i).valid, p"pc 0x${Hexadecimal(io.toIntDq(i).bits.cf.pc)} int index $i\n")
    XSDebug(io.toFpDq(i).valid , p"pc 0x${Hexadecimal(io.toFpDq(i).bits.cf.pc )} fp  index $i\n")
    XSDebug(io.toLsDq(i).valid , p"pc 0x${Hexadecimal(io.toLsDq(i).bits.cf.pc )} ls  index $i\n")
  }

  /**
    * Part 3: send response to rename when dispatch queue accepts the uop
    */
  val readyVector = (0 until RenameWidth).map(i => !io.fromRename(i).valid || io.recv(i))
  for (i <- 0 until RenameWidth) {
    io.recv(i) := thisCanActualOut(i)
    io.fromRename(i).ready := Cat(readyVector).andR()

    XSInfo(io.recv(i),
      p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)}, type(${isInt(i)}, ${isFp(i)}, ${isLs(i)}), " +
      p"roq ${updateUopWithIndex(i).roqIdx}, lq ${updateUopWithIndex(i).lqIdx}, sq ${updateUopWithIndex(i).sqIdx}, " +
      p"(${intIndex.io.reverseMapping(i).bits}, ${fpIndex.io.reverseMapping(i).bits}, ${lsIndex.io.reverseMapping(i).bits})\n"
    )

    io.allocPregs(i).isInt := io.fromRename(i).valid && io.fromRename(i).bits.ctrl.rfWen && (io.fromRename(i).bits.ctrl.ldest =/= 0.U)
    io.allocPregs(i).isFp  := io.fromRename(i).valid && io.fromRename(i).bits.ctrl.fpWen
    io.allocPregs(i).preg  := io.fromRename(i).bits.pdest
  }
  val renameFireCnt = PopCount(io.recv)
  val enqFireCnt = PopCount(io.toIntDq.map(_.valid && io.toIntDqReady)) + PopCount(io.toFpDq.map(_.valid && io.toFpDqReady)) + PopCount(io.toLsDq.map(_.valid && io.toLsDqReady))
  XSError(enqFireCnt > renameFireCnt, "enqFireCnt should not be greater than renameFireCnt\n")
}
