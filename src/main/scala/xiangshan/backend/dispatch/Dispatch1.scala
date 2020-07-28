package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import utils.{XSDebug, XSInfo, XSWarn}

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
    val roqIdxs = Input(Vec(RenameWidth, UInt(RoqIdxWidth.W)))
    // enq Moq
    val toMoq =  Vec(RenameWidth, DecoupledIO(new MicroOp))
    // get MoqIdx
    val moqIdxs = Input(Vec(RenameWidth, UInt(MoqIdxWidth.W)))
    // to dispatch queue
    val toIntDq = Vec(dpParams.DqEnqWidth, DecoupledIO(new MicroOp))
    val toFpDq = Vec(dpParams.DqEnqWidth, DecoupledIO(new MicroOp))
    val toLsDq = Vec(dpParams.DqEnqWidth, DecoupledIO(new MicroOp))
  })
  /**
    * Part 1: choose the target dispatch queue and the corresponding write ports
    */
  // valid bits for different dispatch queues
  val isInt = WireInit(VecInit(io.fromRename.map(uop => FuType.isIntExu(uop.bits.ctrl.fuType))))
  val isFp  = WireInit(VecInit(io.fromRename.map(uop => FuType.isFpExu (uop.bits.ctrl.fuType))))
  val isLs  = WireInit(VecInit(io.fromRename.map(uop => FuType.isMemExu(uop.bits.ctrl.fuType))))

  // generate index mapping
  val intIndex = Module(new IndexMapping(RenameWidth, dpParams.DqEnqWidth))
  val fpIndex  = Module(new IndexMapping(RenameWidth, dpParams.DqEnqWidth))
  val lsIndex  = Module(new IndexMapping(RenameWidth, dpParams.DqEnqWidth))
  intIndex.io.validBits := isInt
  fpIndex.io.validBits := isFp
  lsIndex.io.validBits := isLs

  /**
    * Part 2: acquire ROQ (all) and LSROQ (load/store only) indexes
    */
  val uopWithIndex = Wire(Vec(RenameWidth, new MicroOp))
  val roqIndexReg = Reg(Vec(RenameWidth, UInt(RoqIdxWidth.W)))
  val roqIndexRegValid = RegInit(VecInit(Seq.fill(RenameWidth)(false.B)))
  val roqIndexAcquired = WireInit(VecInit(Seq.tabulate(RenameWidth)(i => io.toRoq(i).ready || roqIndexRegValid(i))))
  val lsroqIndexReg = Reg(Vec(RenameWidth, UInt(MoqIdxWidth.W)))
  val lsroqIndexRegValid = RegInit(VecInit(Seq.fill(RenameWidth)(false.B)))
  val lsroqIndexAcquired = WireInit(VecInit(Seq.tabulate(RenameWidth)(i => io.toMoq(i).ready || lsroqIndexRegValid(i) || !isLs(i))))

  for (i <- 0 until RenameWidth) {
    // input for ROQ and LSROQ
    io.toRoq(i).valid := io.fromRename(i).valid && !roqIndexRegValid(i)
    io.toMoq(i).valid := io.fromRename(i).valid && !lsroqIndexRegValid(i) && isLs(i)
    io.toRoq(i).bits := io.fromRename(i).bits
    io.toMoq(i).bits := io.fromRename(i).bits

    // receive indexes from ROQ and LSROQ
    when(io.toRoq(i).fire() && !io.recv(i)) {
      roqIndexReg(i) := io.roqIdxs(i)
      roqIndexRegValid(i) := true.B
    }.elsewhen(io.recv(i)) {
      roqIndexRegValid(i) := false.B
    }
    when(io.toMoq(i).fire() && !io.recv(i)) {
      lsroqIndexReg(i) := io.moqIdxs(i)
      lsroqIndexRegValid(i) := true.B
    }.elsewhen(io.recv(i)) {
      lsroqIndexRegValid(i) := false.B
    }

    // append ROQ and LSROQ indexed to uop
    uopWithIndex(i) := io.fromRename(i).bits
    uopWithIndex(i).roqIdx := Mux(roqIndexRegValid(i), roqIndexReg(i), io.roqIdxs(i))
    uopWithIndex(i).moqIdx := Mux(lsroqIndexRegValid(i), lsroqIndexReg(i), io.moqIdxs(i))

    XSDebug(io.toRoq(i).fire(), p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} receives nroq ${io.roqIdxs(i)}\n")
    XSDebug(io.toMoq(i).fire(), p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} receives mroq ${io.moqIdxs(i)}\n")
    if (i > 0) {
      XSWarn(io.toRoq(i).fire() && !io.toRoq(i - 1).ready && io.toRoq(i - 1).valid, p"roq handshake not continuous $i")
    }
  }

  /**
    * Part 3: send uop (should not be cancelled) with correct indexes to dispatch queues
    */
  val cancelled = WireInit(VecInit(Seq.fill(RenameWidth)(true.B)))
  for (i <- 0 until dpParams.DqEnqWidth) {
    io.toIntDq(i).bits := uopWithIndex(intIndex.io.mapping(i).bits)
    io.toIntDq(i).valid := intIndex.io.mapping(i).valid && roqIndexAcquired(intIndex.io.mapping(i).bits) &&
      lsroqIndexAcquired(intIndex.io.mapping(i).bits) && !cancelled(intIndex.io.mapping(i).bits)

    io.toFpDq(i).bits := uopWithIndex(fpIndex.io.mapping(i).bits)
    io.toFpDq(i).valid := fpIndex.io.mapping(i).valid && roqIndexAcquired(fpIndex.io.mapping(i).bits) &&
      lsroqIndexAcquired(fpIndex.io.mapping(i).bits) && !cancelled(fpIndex.io.mapping(i).bits)

    io.toLsDq(i).bits := uopWithIndex(lsIndex.io.mapping(i).bits)
    io.toLsDq(i).valid := lsIndex.io.mapping(i).valid && roqIndexAcquired(lsIndex.io.mapping(i).bits) &&
      lsroqIndexAcquired(lsIndex.io.mapping(i).bits) && !cancelled(lsIndex.io.mapping(i).bits)

    XSDebug(io.toIntDq(i).valid, p"pc 0x${Hexadecimal(io.toIntDq(i).bits.cf.pc)} int index $i\n")
    XSDebug(io.toFpDq(i).valid , p"pc 0x${Hexadecimal(io.toFpDq(i).bits.cf.pc )} fp  index $i\n")
    XSDebug(io.toLsDq(i).valid , p"pc 0x${Hexadecimal(io.toLsDq(i).bits.cf.pc )} ls  index $i\n")
  }

  /**
    * Part 4: send response to rename when dispatch queue accepts the uop
    */
  val readyVector = (0 until RenameWidth).map(i => !io.fromRename(i).valid || io.recv(i))
  val allReady = Cat(readyVector).andR()
  for (i <- 0 until RenameWidth) {
    val enqFire = (io.toIntDq(intIndex.io.reverseMapping(i).bits).fire() && isInt(i)) ||
      (io.toFpDq(fpIndex.io.reverseMapping(i).bits).fire() && isFp(i)) ||
      (io.toLsDq(lsIndex.io.reverseMapping(i).bits).fire() && isLs(i))
    io.recv(i) := enqFire || cancelled(i)
    io.fromRename(i).ready := allReady

    XSInfo(io.recv(i) && !cancelled(i),
      p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} type(${isInt(i)}, ${isFp(i)}, ${isLs(i)}) " +
        p"roq ${uopWithIndex(i).roqIdx} lsroq ${uopWithIndex(i).moqIdx} is accepted by dispatch queue\n")
    XSInfo(io.recv(i) && cancelled(i),
      p"pc 0x${Hexadecimal(io.fromRename(i).bits.cf.pc)} with brTag ${io.fromRename(i).bits.brTag.value} cancelled\n")
    XSDebug(io.fromRename(i).valid, "v:%d r:%d pc 0x%x of type %b is in %d-th slot\n",
      io.fromRename(i).valid, io.fromRename(i).ready, io.fromRename(i).bits.cf.pc, io.fromRename(i).bits.ctrl.fuType, i.U)
  }
}
