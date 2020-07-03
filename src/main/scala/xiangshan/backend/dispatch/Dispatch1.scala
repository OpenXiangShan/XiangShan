package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.utils.{XSDebug, XSInfo, XSWarn}

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
    XSDebug(io.redirect.valid, p"pc=${Hexadecimal(io.fromRename(i).bits.cf.pc)} brMask:${Binary(io.fromRename(i).bits.brMask)} brTag:${io.redirect.bits.brTag}\n")
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
    XSInfo(io.recv(i) && !cancelled(i), "pc 0x%x accepted by queue %x %x %x\n",
      io.fromRename(i).bits.cf.pc, io.toIntDq(i).valid, io.toFpDq(i).valid, io.toLsDq(i).valid)
    XSInfo(io.recv(i) && cancelled(i), "pc 0x%x with brMask %x brTag %x cancelled\n",
      io.fromRename(i).bits.cf.pc, io.fromRename(i).bits.brMask, io.redirect.bits.brTag)
  }

  // latch indexes from roq in case of DQ not fire
  val roqIndexReg = Reg(Vec(RenameWidth, UInt(RoqIdxWidth.W)))
  val roqIndexRegValid = RegInit(VecInit(Seq.fill(RenameWidth)(false.B)))
  for (i <- 0 until RenameWidth) {
    // dispatch queue does not accept the MicroOp
    // however, ROQ has fired
    when (io.toRoq(i).fire() && !io.recv(i)) {
      roqIndexReg(i) := io.roqIdxs(i)
      roqIndexRegValid(i) := true.B
    }
    .elsewhen (io.recv(i)) {
      roqIndexRegValid(i) := false.B
    }
    XSDebug(io.toRoq(i).fire() && !io.recv(i),
      "pc 0x%x receives nboq %x but not accepted by queue (and it waits)\n",
      io.fromRename(i).bits.cf.pc, io.roqIdxs(i))
  }

  // append nroq to uop
  val uop_nroq = Wire(Vec(RenameWidth, new MicroOp))
  for (i <- 0 until RenameWidth) {
    uop_nroq(i) := io.fromRename(i).bits
    uop_nroq(i).roqIdx := Mux(io.toRoq(i).ready, io.roqIdxs(i), roqIndexReg(i))
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
    io.toRoq(i).valid := io.fromRename(i).valid && !roqIndexRegValid(i)// && !cancelled(i)
    XSDebug(io.toRoq(i).fire(), "pc 0x%x receives nroq %d\n", io.fromRename(i).bits.cf.pc, io.roqIdxs(i))
    if (i > 0) {
      XSWarn(io.toRoq(i).fire() && !io.toRoq(i - 1).ready && io.toRoq(i - 1).valid,
        "roq handshake not continuous %d", i.U)
    }
    io.fromRename(i).ready := all_recv
    XSDebug(io.fromRename(i).valid, "pc 0x%x of type %b is in %d-th slot\n",
      io.fromRename(i).bits.cf.pc, io.fromRename(i).bits.ctrl.fuType, i.U)
  }
}
