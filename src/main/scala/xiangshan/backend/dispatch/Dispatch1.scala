package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.regfile.RfReadPort
import utils.{GTimer, PipelineConnect}

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
  // enqueue handshake
  val enq_ready = Wire(Vec(RenameWidth, Bool()))
  val enq_valid = Wire(Vec(RenameWidth, Bool()))
  for (i <- 0 until RenameWidth) {
    enq_ready(i) := (io.toIntDq(i).ready && FuType.isIntExu(io.fromRename(i).bits.ctrl.fuType)) ||
                    (io.toFpDq(i).ready  && FuType.isFpExu(io.fromRename(i).bits.ctrl.fuType )) ||
                    (io.toLsDq(i).ready  && FuType.isMemExu(io.fromRename(i).bits.ctrl.fuType))
    enq_valid(i) := io.toIntDq(i).valid || io.toFpDq(i).valid || io.toLsDq(i).valid
    io.recv(i) := enq_ready(i) && enq_valid(i)
    when (io.recv(i)) {
      printf("[Dispatch1:%d]: instruction 0x%x accepted by queue %x %x %x\n", GTimer(), io.fromRename(i).bits.cf.pc,
        io.toIntDq(i).valid, io.toFpDq(i).valid, io.toLsDq(i).valid)
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
      printf("[Dispatch1:%d]: instruction 0x%x receives nboq %x but not accepted by queue (and it waits)\n",
        GTimer(), io.fromRename(i).bits.cf.pc, io.roqIdxs(i))
    }
    .elsewhen (io.recv(i)) {
      roqIndexRegValid(i) := false.B
      printf("[Dispatch1:%d]: waiting instruction 0x%x is accepted by queue\n", GTimer(), io.fromRename(i).bits.cf.pc)
    }
  }

  // append nroq to uop
  val uop_nroq = Wire(Vec(RenameWidth, new MicroOp))
  for (i <- 0 until RenameWidth) {
    uop_nroq(i) := io.fromRename(i).bits
    uop_nroq(i).roqIdx := Mux(io.toRoq(i).ready, io.roqIdxs(i), roqIndexReg(i))
    when (io.toRoq(i).fire()) {
      printf("[Dispatch1:%d]: instruction 0x%x receives nroq %d\n", GTimer(), io.fromRename(i).bits.cf.pc, io.roqIdxs(i))
    }
  }

  // uop can enqueue when rename.valid and roq.valid
  val can_enqueue = Wire(Vec(RenameWidth, Bool()))
  for (i <- 0 until RenameWidth) {
    can_enqueue(i) := io.fromRename(i).valid && (io.toRoq(i).ready || roqIndexRegValid(i))
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
      printf("[Dispatch1:%d]: instruction 0x%x is in %d-th slot\n", GTimer(), io.fromRename(i).bits.cf.pc, i.U)
    }
  }
}

class Dispatch1Debug extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    // enq Roq
    val toRoq =  Vec(RenameWidth, DecoupledIO(new MicroOp))
    // get RoqIdx
    val roqIdxs = Input(Vec(RenameWidth, UInt(RoqIdxWidth.W)))
    val fromIntDq = Vec(IntDqDeqWidth, DecoupledIO(UInt(46.W)))
    val fromFpDq = Vec(FpDqDeqWidth, DecoupledIO(UInt(46.W)))
    val fromLsDq = Vec(LsDqDeqWidth, DecoupledIO(UInt(46.W)))
    // read regfile
    //    val readIntRf = Vec(NRReadPorts, Flipped(new RfReadPort))
    //    val readFpRf = Vec(NRReadPorts, Flipped(new RfReadPort))
    // ro reservation stations
    //    val enqIQCtrl = Vec(exuConfig.ExuCnt, DecoupledIO(new MicroOp))
    //    val enqIQData = Vec(exuConfig.ExuCnt, ValidIO(new   ExuInput))
  })
  // pipeline between rename and dispatch
  val dispatch1 = Module(new Dispatch1())
  for (i <- 0 until RenameWidth) {
//    dispatch1.io.fromRename(i) <> Queue(io.fromRename(i))
    PipelineConnect(io.fromRename(i), dispatch1.io.fromRename(i), dispatch1.io.recv(i), false.B)
  }

//  dispatch1.io.fromRename <> DontCare
//  io.fromRename.foreach( x => x.ready <> DontCare)

  val intDq = Module(new DispatchQueue(new MicroOp, dp1Config.IntDqSize, RenameWidth, IntDqDeqWidth))
  val fpDq = Module(new DispatchQueue(new MicroOp, dp1Config.FpDqSize, RenameWidth, FpDqDeqWidth))
  val lsDq = Module(new DispatchQueue(new MicroOp, dp1Config.LsDqSize, RenameWidth, LsDqDeqWidth))
//  val dispatch2 = new Dispatch2()

  dispatch1.io.redirect <> io.redirect
  dispatch1.io.toRoq <> io.toRoq
  dispatch1.io.roqIdxs <> io.roqIdxs
  dispatch1.io.toIntDq <> intDq.io.enq
  dispatch1.io.toFpDq <> fpDq.io.enq
  dispatch1.io.toLsDq <> lsDq.io.enq

  for (i <- 0 until IntDqDeqWidth) {
    intDq.io.deq(i).ready := io.fromIntDq(i).ready
    io.fromIntDq(i).valid := intDq.io.deq(i).valid
    io.fromIntDq(i).bits := Cat(intDq.io.deq(i).bits.roqIdx, intDq.io.deq(i).bits.cf.pc)
    when (io.fromIntDq(i).fire()) {
      printf("[Dispatch1:%d]: instruction 0x%x leaves Int dispatch queue with nroq %d\n",
        GTimer(), io.fromIntDq(i).bits(38, 0), io.fromIntDq(i).bits(45, 39))
    }
  }
  for (i <- 0 until FpDqDeqWidth) {
    fpDq.io.deq(i).ready := io.fromFpDq(i).ready
    io.fromFpDq(i).valid := fpDq.io.deq(i).valid
    io.fromFpDq(i).bits := Cat(fpDq.io.deq(i).bits.roqIdx, fpDq.io.deq(i).bits.cf.pc)
    when (io.fromFpDq(i).fire()) {
      printf("[Dispatch1:%d]: instruction 0x%x leaves FP dispatch queue with nroq %d\n",
        GTimer(), io.fromFpDq(i).bits(38, 0), io.fromIntDq(i).bits(45, 39))
    }
  }
  for (i <- 0 until LsDqDeqWidth) {
    lsDq.io.deq(i).ready := io.fromLsDq(i).ready
    io.fromLsDq(i).valid := lsDq.io.deq(i).valid
    io.fromLsDq(i).bits := Cat(lsDq.io.deq(i).bits.roqIdx, lsDq.io.deq(i).bits.cf.pc)
    when (io.fromLsDq(i).fire()) {
      printf("[Dispatch1:%d]: instruction 0x%x leaves LS dispatch queue with nroq %d\n",
        GTimer(), io.fromLsDq(i).bits(38, 0), io.fromIntDq(i).bits(45, 39))
    }
  }
}

object Dispatch1Top extends App {
  Driver.execute(args, () => new Dispatch1Debug())
}