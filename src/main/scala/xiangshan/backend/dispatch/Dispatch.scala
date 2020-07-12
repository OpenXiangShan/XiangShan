package xiangshan.backend.dispatch

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.exu.ExuConfig
import xiangshan.utils._
import xiangshan.backend.regfile.RfReadPort

class Dispatch(exuCfg: Array[ExuConfig]) extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    // from rename
    val fromRename = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    // enq Roq
    val toRoq =  Vec(RenameWidth, DecoupledIO(new MicroOp))
    // get RoqIdx
    val roqIdxs = Input(Vec(RenameWidth, UInt(RoqIdxWidth.W)))
    // check if roq is empty for 'noSpecExec' inst
    val roqIsEmpty = Input(Bool())
    // the 'noSpecExec' inst is commiting?
    val isNoSpecExecCommit =Input(Bool())
    // read regfile
    val readIntRf = Vec(NRReadPorts, Flipped(new RfReadPort))
    val readFpRf = Vec(NRReadPorts, Flipped(new RfReadPort))
    // read reg status (busy/ready)
    val intPregRdy = Vec(NRReadPorts, Input(Bool()))
    val fpPregRdy = Vec(NRReadPorts, Input(Bool()))
    // to reservation stations
    val numExist = Input(Vec(exuParameters.ExuCnt, UInt(log2Ceil(IssQueSize).W)))
    val enqIQCtrl = Vec(exuParameters.ExuCnt, DecoupledIO(new MicroOp))
    val enqIQData = Vec(exuParameters.ExuCnt, ValidIO(new ExuInput))
  })
  // pipeline between rename and dispatch
  val dispatch1 = Module(new Dispatch1)

  val s_idle :: s_waitRoqEmpty :: s_waitCommit :: Nil = Enum(3)

  val state = RegInit(s_idle)

  val noSpecVec = io.fromRename.map(x => x.valid && x.bits.ctrl.noSpecExec)
  assert(PopCount(noSpecVec)<=1.U, "Error: multi noSpecExec inst in Dispatch\n")

  val hasNoSpecInst = ParallelOR(noSpecVec).asBool()

  switch(state){
    is(s_idle){
      when(hasNoSpecInst){
        state := s_waitRoqEmpty
      }
    }
    is(s_waitRoqEmpty){
      when(io.roqIsEmpty){
        state := s_waitCommit
      }
    }
    is(s_waitCommit){
      when(io.isNoSpecExecCommit){
        state := s_idle
      }
    }
  }
  when(io.redirect.valid){ state := s_idle }

  XSDebug(p"state=$state roqEmpty:${io.roqIsEmpty} noSpecCmt:${io.isNoSpecExecCommit}\n")

  XSDebug(
    (state===s_idle) && hasNoSpecInst,
    p"a noSpec inst in\n"
  )

  XSDebug(
    (state===s_waitRoqEmpty) && io.roqIsEmpty,
    p"roq is empty, switch state to waitCommit\n"
  )

  XSDebug(
    (state===s_waitCommit) && io.isNoSpecExecCommit,
    p"the noSpec inst commited, switch state to idle\n"
  )

  for (i <- 0 until RenameWidth) {
    val valid = RegInit(false.B)
    when(dispatch1.io.recv(i)){ valid := false.B  }
    when(io.fromRename(i).fire()){ valid := true.B }
    dispatch1.io.fromRename(i).valid := Mux(state===s_idle,
      valid,
      valid && state===s_waitCommit
    )
    io.fromRename(i).ready := dispatch1.io.fromRename(i).ready && state===s_idle
    dispatch1.io.fromRename(i).bits <> RegEnable(io.fromRename(i).bits, io.fromRename(i).fire())
  }
  val intDq = Module(new DispatchQueue(dp1Paremeters.IntDqSize, RenameWidth, IntDqDeqWidth, "IntDpQ"))
  val fpDq = Module(new DispatchQueue(dp1Paremeters.FpDqSize, RenameWidth, FpDqDeqWidth, "FpDpQ"))
  val lsDq = Module(new DispatchQueue(dp1Paremeters.LsDqSize, RenameWidth, LsDqDeqWidth, "LsDpQ"))
  val dispatch2 = Module(new Dispatch2(exuCfg))

  dispatch1.io.redirect <> io.redirect
  dispatch1.io.toRoq <> io.toRoq
  dispatch1.io.roqIdxs <> io.roqIdxs
  dispatch1.io.toIntDq <> intDq.io.enq
  dispatch1.io.toFpDq <> fpDq.io.enq
  dispatch1.io.toLsDq <> lsDq.io.enq

  // dispatch queue cancels the uops
  intDq.io.redirect <> io.redirect
  fpDq.io.redirect <> io.redirect
  lsDq.io.redirect <> io.redirect

  // dispatch2 only receives valid uops from dispatch queue
  dispatch2.io.fromIntDq <> intDq.io.deq
  dispatch2.io.fromFpDq <> fpDq.io.deq
  dispatch2.io.fromLsDq <> lsDq.io.deq
  dispatch2.io.readIntRf <> io.readIntRf
  dispatch2.io.readFpRf <> io.readFpRf
  dispatch2.io.intPregRdy <> io.intPregRdy
  dispatch2.io.fpPregRdy <> io.fpPregRdy
  dispatch2.io.enqIQCtrl <> io.enqIQCtrl
  dispatch2.io.enqIQData <> io.enqIQData
  dispatch2.io.numExist <> io.numExist
}
