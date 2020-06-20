package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._

class Rename extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val roqCommits = Vec(CommitWidth, Flipped(ValidIO(new RoqCommit)))
    val wbIntResults = Vec(NRWritePorts, Flipped(ValidIO(new ExuOutput)))
    val wbFpResults = Vec(NRWritePorts, Flipped(ValidIO(new ExuOutput)))
    val intPregRdy = Vec(NRReadPorts, Output(Bool()))
    val fpPregRdy = Vec(NRReadPorts, Output(Bool()))
    // from decode buffer
    val in = Vec(RenameWidth, Flipped(DecoupledIO(new CfCtrl)))
    // to dispatch1
    val out = Vec(RenameWidth, DecoupledIO(new MicroOp))
  })
}
