package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import xiangshan._

class Rename extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val roqCommits = Vec(CommitWidth, Flipped(ValidIO(new RoqCommit)))
    val in = Vec(RenameWidth, Flipped(DecoupledIO(new CfCtrl)))
    val out = Vec(RenameWidth, DecoupledIO(new MicroOp))
  })
}
