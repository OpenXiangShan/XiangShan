package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import chisel3.ExcitingUtils._
import xiangshan._
import utils.{XSDebug, XSError, XSInfo}
import xiangshan.backend.roq.RoqPtr

// read rob and enqueue
class Dispatch1 extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
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
  // thisIsBlocked: this instruction is blocked by itself (based on noSpecExec)
  // thisCanOut: this instruction can enqueue (based on resource)
  // nextCanOut: next instructions can out (based on blockBackward and previous instructions)
  // notBlockedByPrevious: previous instructions can enqueue
  val thisIsBlocked = VecInit((0 until RenameWidth).map(i => {
    // for i > 0, when Roq is empty but dispatch1 have valid instructions to enqueue, it's blocked
    if (i > 0) isNoSpecExec(i) && (!io.enqRoq.isEmpty || Cat(io.fromRename.take(i).map(_.valid)).orR)
    else isNoSpecExec(i) && !io.enqRoq.isEmpty
  }))
  val thisCanOut = VecInit((0 until RenameWidth).map(i => {
    // For i in [0, DqEnqWidth), they can always enqueue when ROB and LSQ are ready
    if (i < dpParams.DqEnqWidth) true.B
    else Cat(Seq(intIndex, fpIndex, lsIndex).map(_.io.reverseMapping(i).valid)).orR
  }))
  val nextCanOut = VecInit((0 until RenameWidth).map(i =>
    (thisCanOut(i) && !isNoSpecExec(i) && !isBlockBackward(i)) || !io.fromRename(i).valid
  ))
  val notBlockedByPrevious = VecInit((0 until RenameWidth).map(i =>
    if (i == 0) true.B
    else Cat((0 until i).map(j => nextCanOut(j))).andR
  ))

  // this instruction can actually dequeue: 3 conditions
  // (1) resources are ready
  // (2) previous instructions are ready
  val thisCanActualOut = (0 until RenameWidth).map(i => allResourceReady && thisCanOut(i) && !thisIsBlocked(i) && notBlockedByPrevious(i))

  val uopWithIndex = Wire(Vec(RenameWidth, new MicroOp))

  for (i <- 0 until RenameWidth) {
    // input for ROQ and LSQ
    val commitType = Cat(isLs(i), isStore(i) | isFp(i))

    io.enqRoq.extraWalk(i) := io.fromRename(i).valid && !thisCanActualOut(i)
    io.enqRoq.req(i).valid := io.fromRename(i).valid && thisCanActualOut(i)
    io.enqRoq.req(i).bits := io.fromRename(i).bits
    io.enqRoq.req(i).bits.ctrl.commitType := commitType

    val shouldEnqLsq = isLs(i) && io.fromRename(i).bits.ctrl.fuType =/= FuType.mou
    io.enqLsq.req(i).valid := io.fromRename(i).valid && shouldEnqLsq && !redirectValid && thisCanActualOut(i)
    io.enqLsq.req(i).bits := io.fromRename(i).bits
    io.enqLsq.req(i).bits.ctrl.commitType := commitType
    io.enqLsq.req(i).bits.roqIdx := io.enqRoq.resp(i)

    // append ROQ and LSQ indexed to uop
    uopWithIndex(i) := io.fromRename(i).bits
    uopWithIndex(i).roqIdx := io.enqRoq.resp(i)
    uopWithIndex(i).lqIdx := io.enqLsq.resp(i).lqIdx
    uopWithIndex(i).sqIdx := io.enqLsq.resp(i).sqIdx

    XSDebug(io.enqLsq.req(i).valid,
      p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} receives lq ${io.enqLsq.resp(i).lqIdx} sq ${io.enqLsq.resp(i).sqIdx}\n")

    XSDebug(io.enqRoq.req(i).valid, p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} receives nroq ${io.enqRoq.resp(i)}\n")
  }

  // send uops with correct indexes to dispatch queues
  // Note that if one of their previous instructions cannot enqueue, they should not enter dispatch queue.
  // We use notBlockedByPrevious here since mapping(i).valid implies there's a valid instruction that can enqueue,
  // thus we don't need to check thisCanOut.
  for (i <- 0 until dpParams.DqEnqWidth) {
    io.toIntDq(i).bits  := uopWithIndex(intIndex.io.mapping(i).bits)
    io.toIntDq(i).valid := intIndex.io.mapping(i).valid && allResourceReady &&
                           !thisIsBlocked(intIndex.io.mapping(i).bits) && notBlockedByPrevious(intIndex.io.mapping(i).bits)

    // NOTE: floating point instructions are not noSpecExec currently
    // remove commit /**/ when fp instructions are possible to be noSpecExec
    io.toFpDq(i).bits   := uopWithIndex(fpIndex.io.mapping(i).bits)
    io.toFpDq(i).valid  := fpIndex.io.mapping(i).valid && allResourceReady &&
                           /*!thisIsBlocked(fpIndex.io.mapping(i).bits) && */notBlockedByPrevious(fpIndex.io.mapping(i).bits)

    io.toLsDq(i).bits   := uopWithIndex(lsIndex.io.mapping(i).bits)
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
      p"roq ${uopWithIndex(i).roqIdx}, lq ${uopWithIndex(i).lqIdx}, sq ${uopWithIndex(i).sqIdx}, " +
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
