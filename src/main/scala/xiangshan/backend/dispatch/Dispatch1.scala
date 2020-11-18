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
    val toRoq =  Vec(RenameWidth, DecoupledIO(new MicroOp))
    // get RoqIdx
    val roqIdxs = Input(Vec(RenameWidth, new RoqPtr))
    // enq Lsq
    val toLsq = Vec(RenameWidth, DecoupledIO(new MicroOp))
    // get LsIdx
    val lsIdx = Input(Vec(RenameWidth, new LSIdx))
    // to dispatch queue
    val toIntDq = Vec(dpParams.DqEnqWidth, DecoupledIO(new MicroOp))
    val toFpDq = Vec(dpParams.DqEnqWidth, DecoupledIO(new MicroOp))
    val toLsDq = Vec(dpParams.DqEnqWidth, DecoupledIO(new MicroOp))
  })
  /**
    * Part 1: choose the target dispatch queue and the corresponding write ports
    */
  // valid bits for different dispatch queues
  val isInt   = WireInit(VecInit(io.fromRename.map(uop => FuType.isIntExu(uop.bits.ctrl.fuType))))
  val isFp    = WireInit(VecInit(io.fromRename.map(uop => FuType.isFpExu (uop.bits.ctrl.fuType))))
  val isLs    = WireInit(VecInit(io.fromRename.map(uop => FuType.isMemExu(uop.bits.ctrl.fuType))))
  val isStore = WireInit(VecInit(io.fromRename.map(uop => FuType.isStoreExu(uop.bits.ctrl.fuType))))

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
    * Part 2: acquire ROQ (all) and LSQ (load/store only) indexes
    */
  val cancelled = WireInit(VecInit(Seq.fill(RenameWidth)(io.redirect.valid && !io.redirect.bits.isReplay)))

  val uopWithIndex = Wire(Vec(RenameWidth, new MicroOp))
  val roqIndexReg = Reg(Vec(RenameWidth, new RoqPtr))
  val roqIndexRegValid = RegInit(VecInit(Seq.fill(RenameWidth)(false.B)))
  val roqIndexAcquired = WireInit(VecInit(Seq.tabulate(RenameWidth)(i => io.toRoq(i).ready || roqIndexRegValid(i))))
  val lsIndexReg = Reg(Vec(RenameWidth, new LSIdx))
  val lsIndexRegValid = RegInit(VecInit(Seq.fill(RenameWidth)(false.B)))
  val lsqIndexAcquired = WireInit(VecInit(Seq.tabulate(RenameWidth)(i => io.toLsq(i).ready || lsIndexRegValid(i))))

  for (i <- 0 until RenameWidth) {
    // input for ROQ and LSQ
    val commitType = Cat(isLs(i), isStore(i) | isFp(i))

    io.toRoq(i).valid := io.fromRename(i).valid && !roqIndexRegValid(i)
    io.toRoq(i).bits := io.fromRename(i).bits
    io.toRoq(i).bits.ctrl.commitType := commitType

    io.toLsq(i).valid := io.fromRename(i).valid && !lsIndexRegValid(i) && isLs(i) && io.fromRename(i).bits.ctrl.fuType =/= FuType.mou && roqIndexAcquired(i) && !cancelled(i)
    io.toLsq(i).bits := io.fromRename(i).bits
    io.toLsq(i).bits.ctrl.commitType := commitType
    io.toLsq(i).bits.roqIdx := Mux(roqIndexRegValid(i), roqIndexReg(i), io.roqIdxs(i))

    // receive indexes from ROQ and LSQ
    when(io.toRoq(i).fire() && !io.recv(i)) {
      roqIndexReg(i) := io.roqIdxs(i)
      roqIndexRegValid(i) := true.B
    }.elsewhen(io.recv(i)) {
      roqIndexRegValid(i) := false.B
    }
    when(io.toLsq(i).fire() && !io.recv(i)) {
      lsIndexReg(i) := io.lsIdx(i)
      lsIndexRegValid(i) := true.B
    }.elsewhen(io.recv(i)) {
      lsIndexRegValid(i) := false.B
    }

    // append ROQ and LSQ indexed to uop
    uopWithIndex(i) := io.fromRename(i).bits
    uopWithIndex(i).roqIdx := Mux(roqIndexRegValid(i), roqIndexReg(i), io.roqIdxs(i))
    uopWithIndex(i).lqIdx := Mux(lsIndexRegValid(i), lsIndexReg(i), io.lsIdx(i)).lqIdx
    uopWithIndex(i).sqIdx := Mux(lsIndexRegValid(i), lsIndexReg(i), io.lsIdx(i)).sqIdx
    XSDebug(io.toLsq(i).fire(), p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} receives lq ${io.lsIdx(i).lqIdx} sq ${io.lsIdx(i).sqIdx}\n")

    XSDebug(io.toRoq(i).fire(), p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} receives nroq ${io.roqIdxs(i)}\n")
    if (i > 0) {
      XSError(io.toRoq(i).fire() && !io.toRoq(i - 1).ready && io.toRoq(i - 1).valid, p"roq handshake not continuous $i")
    }
  }

  /**
    * Part 3: send uop (should not be cancelled) with correct indexes to dispatch queues
    */
  val orderedEnqueue = Wire(Vec(RenameWidth, Bool()))
  val canEnqueue = Wire(Vec(RenameWidth, Bool()))
  var prevCanEnqueue = true.B
  for (i <- 0 until RenameWidth) {
    orderedEnqueue(i) := prevCanEnqueue
    canEnqueue(i) := !cancelled(i) && roqIndexAcquired(i) && (!isLs(i) || io.fromRename(i).bits.ctrl.fuType === FuType.mou || lsqIndexAcquired(i))
    val enqReady = (io.toIntDq(intIndex.io.reverseMapping(i).bits).ready && intIndex.io.reverseMapping(i).valid) ||
      (io.toFpDq(fpIndex.io.reverseMapping(i).bits).ready && fpIndex.io.reverseMapping(i).valid) ||
      (io.toLsDq(lsIndex.io.reverseMapping(i).bits).ready && lsIndex.io.reverseMapping(i).valid)
    prevCanEnqueue = prevCanEnqueue && (!io.fromRename(i).valid || (canEnqueue(i) && enqReady))
  }
  for (i <- 0 until dpParams.DqEnqWidth) {
    io.toIntDq(i).bits := uopWithIndex(intIndex.io.mapping(i).bits)
    io.toIntDq(i).valid := intIndex.io.mapping(i).valid &&
      canEnqueue(intIndex.io.mapping(i).bits) &&
      orderedEnqueue(intIndex.io.mapping(i).bits)

    io.toFpDq(i).bits := uopWithIndex(fpIndex.io.mapping(i).bits)
    io.toFpDq(i).valid := fpIndex.io.mapping(i).valid &&
      canEnqueue(fpIndex.io.mapping(i).bits) &&
      orderedEnqueue(fpIndex.io.mapping(i).bits)

    io.toLsDq(i).bits := uopWithIndex(lsIndex.io.mapping(i).bits)
    io.toLsDq(i).valid := lsIndex.io.mapping(i).valid &&
      canEnqueue(lsIndex.io.mapping(i).bits) &&
      orderedEnqueue(lsIndex.io.mapping(i).bits)

    // XSDebug(io.toIntDq(i).valid, p"pc 0x${Hexadecimal(io.toIntDq(i).bits.cf.pc)} int index $i\n")
    // XSDebug(io.toFpDq(i).valid , p"pc 0x${Hexadecimal(io.toFpDq(i).bits.cf.pc )} fp  index $i\n")
    // XSDebug(io.toLsDq(i).valid , p"pc 0x${Hexadecimal(io.toLsDq(i).bits.cf.pc )} ls  index $i\n")
  }

  /**
    * Part 4: send response to rename when dispatch queue accepts the uop
    */
  val readyVector = (0 until RenameWidth).map(i => !io.fromRename(i).valid || io.recv(i))
  for (i <- 0 until RenameWidth) {
    val enqFire = (io.toIntDq(intIndex.io.reverseMapping(i).bits).fire() && intIndex.io.reverseMapping(i).valid) ||
      (io.toFpDq(fpIndex.io.reverseMapping(i).bits).fire() && fpIndex.io.reverseMapping(i).valid) ||
      (io.toLsDq(lsIndex.io.reverseMapping(i).bits).fire() && lsIndex.io.reverseMapping(i).valid)
    io.recv(i) := enqFire || cancelled(i)
    io.fromRename(i).ready := Cat(readyVector).andR()

    // TODO: add print method for lsIdx
    XSInfo(io.recv(i) && !cancelled(i),
      p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} type(${isInt(i)}, ${isFp(i)}, ${isLs(i)}) " +
      p"roq ${uopWithIndex(i).roqIdx} lq ${uopWithIndex(i).lqIdx} sq ${uopWithIndex(i).sqIdx}" +
      p"(${intIndex.io.reverseMapping(i).bits}, ${fpIndex.io.reverseMapping(i).bits}, ${lsIndex.io.reverseMapping(i).bits})\n")

    XSInfo(io.recv(i) && cancelled(i),
      p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} with brTag ${io.fromRename(i).bits.brTag.value} cancelled\n")
    XSDebug(io.fromRename(i).valid, "v:%d r:%d pc 0x%x of type %b is in %d-th slot\n",
      io.fromRename(i).valid, io.fromRename(i).ready, io.fromRename(i).bits.cf.pc, io.fromRename(i).bits.ctrl.fuType, i.U)
  }
  val renameFireCnt = PopCount(io.recv)
  val enqFireCnt = PopCount(io.toIntDq.map(_.fire)) + PopCount(io.toFpDq.map(_.fire)) + PopCount(io.toLsDq.map(_.fire))
  XSError(enqFireCnt > renameFireCnt, "enqFireCnt should not be greater than renameFireCnt\n")
}
