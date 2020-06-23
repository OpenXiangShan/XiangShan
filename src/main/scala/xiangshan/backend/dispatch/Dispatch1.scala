package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import utils.{GTimer}

case class DP1Config
(
  IntDqSize: Int,
  FpDqSize: Int,
  LsDqSize: Int
)

// read rob and enqueue
class Dispatch1 extends XSModule{
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val recv = Output(Vec(RenameWidth, Bool()))
    // enq Roq
    val toRoq =  Vec(RenameWidth, DecoupledIO(new MicroOp))
    // get RoqIdx
    val roqIdxs = Input(Vec(RenameWidth, UInt(RoqIdxWidth.W)))
    // to dispatch queue
    val toIntDq = Vec(RenameWidth, DecoupledIO(new MicroOp))
    val toFpDq = Vec(RenameWidth, DecoupledIO(new MicroOp))
    val toLsDq = Vec(RenameWidth, DecoupledIO(new MicroOp))
  })
  // check whether valid uops are canceled
  val cancelled = Wire(Vec(RenameWidth, Bool()))
  for (i <- 0 until RenameWidth) {
    cancelled(i) := ((io.fromRename(i).bits.brMask & UIntToOH(io.redirect.bits.brTag)) =/= 0.U) && io.redirect.valid
  }

  // enqueue handshake
  val enq_ready = Wire(Vec(RenameWidth, Bool()))
  val enq_valid = Wire(Vec(RenameWidth, Bool()))
  for (i <- 0 until RenameWidth) {
    enq_ready(i) := (io.toIntDq(i).ready && FuType.isIntExu(io.fromRename(i).bits.ctrl.fuType)) ||
                    (io.toFpDq(i).ready  && FuType.isFpExu(io.fromRename(i).bits.ctrl.fuType )) ||
                    (io.toLsDq(i).ready  && FuType.isMemExu(io.fromRename(i).bits.ctrl.fuType))
    enq_valid(i) := io.toIntDq(i).valid || io.toFpDq(i).valid || io.toLsDq(i).valid
    io.recv(i) := (enq_ready(i) && enq_valid(i)) || cancelled(i)
    when (io.recv(i) && !cancelled(i)) {
      printf("[Cycle:%d][Dispatch1] instruction 0x%x accepted by queue %x %x %x\n",
        GTimer(), io.fromRename(i).bits.cf.pc, io.toIntDq(i).valid, io.toFpDq(i).valid, io.toLsDq(i).valid)
    }
    when (io.recv(i) && cancelled(i)) {
      printf("[Cycle:%d][Dispatch1] instruction 0x%x with brMask %x brTag %x cancelled\n",
        GTimer(), io.fromRename(i).bits.cf.pc, io.fromRename(i).bits.brMask, io.redirect.bits.brTag)
    }
  }

  // latch indexes from roq in case of DQ not fire
  val roqIndexReg = Reg(Vec(RenameWidth, UInt((1 + RoqIdxWidth).W)))
  val roqIndexRegValid = Reg(Vec(RenameWidth, Bool()))
  for (i <- 0 until RenameWidth) {
    // dispatch queue does not accept the MicroOp
    // however, ROQ has fired
    when (io.toRoq(i).fire() && !io.recv(i)) {
      roqIndexReg(i) := io.roqIdxs(i)
      roqIndexRegValid(i) := true.B
      printf("[Cycle:%d][Dispatch1] instruction 0x%x receives nboq %x but not accepted by queue (and it waits)\n",
        GTimer(), io.fromRename(i).bits.cf.pc, io.roqIdxs(i))
    }
    .elsewhen (io.recv(i)) {
      roqIndexRegValid(i) := false.B
      printf("[Cycle:%d][Dispatch1] waiting instruction 0x%x is accepted by queue\n", GTimer(), io.fromRename(i).bits.cf.pc)
    }
  }

  // append nroq to uop
  val uop_nroq = Wire(Vec(RenameWidth, new MicroOp))
  for (i <- 0 until RenameWidth) {
    uop_nroq(i) := io.fromRename(i).bits
    uop_nroq(i).roqIdx := Mux(io.toRoq(i).ready, io.roqIdxs(i), roqIndexReg(i))
    when (io.toRoq(i).fire()) {
      printf("[Cycle:%d][Dispatch1] instruction 0x%x receives nroq %d\n", GTimer(), io.fromRename(i).bits.cf.pc, io.roqIdxs(i))
    }
  }

  // uop can enqueue when rename.valid and roq.valid
  val can_enqueue = Wire(Vec(RenameWidth, Bool()))
  for (i <- 0 until RenameWidth) {
    can_enqueue(i) := io.fromRename(i).valid && (io.toRoq(i).ready || roqIndexRegValid(i)) && !cancelled(i)
    io.toIntDq(i).valid := can_enqueue(i) && FuType.isIntExu(io.fromRename(i).bits.ctrl.fuType)
    io.toIntDq(i).bits := uop_nroq(i)
    io.toFpDq(i).valid := can_enqueue(i) && FuType.isFpExu(io.fromRename(i).bits.ctrl.fuType)
    io.toFpDq(i).bits := uop_nroq(i)
    io.toLsDq(i).valid := can_enqueue(i) && FuType.isMemExu(io.fromRename(i).bits.ctrl.fuType)
    io.toLsDq(i).bits := uop_nroq(i)
  }

  // ack roq and input (rename) when both roq and dispatch queue are ready
  val recv_vector =(0 until RenameWidth).map(i => !io.fromRename(i).valid || io.recv(i))
  val all_recv = recv_vector.reduce((x, y) => x && y).asBool()
  for (i <- 0 until RenameWidth) {
    io.toRoq(i).bits <> io.fromRename(i).bits
    io.toRoq(i).valid := io.fromRename(i).valid && !roqIndexRegValid(i)
    io.fromRename(i).ready := all_recv
    when (io.fromRename(i).valid) {
      printf("[Cycle:%d][Dispatch1] instruction 0x%x of type %b is in %d-th slot\n",
        GTimer(), io.fromRename(i).bits.cf.pc, io.fromRename(i).bits.ctrl.fuType, i.U)
    }
  }
}
