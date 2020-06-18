package xiangshan.backend.roq

import chisel3._
import chisel3.util._
import xiangshan._


class Roq extends XSModule with NeedImpl {
  val io = IO(new Bundle() {
    val brqRedirect = Input(Valid(new Redirect))
    val dp1Req = Vec(RenameWidth, Flipped(DecoupledIO(new MicroOp)))
    val roqIdxs = Output(Vec(RenameWidth, UInt(RoqIdxWidth.W)))
    val redirect = Output(Valid(new Redirect))
    val exeWbResults = Vec(exuConfig.ExuCnt, Flipped(DecoupledIO(new ExuOutput)))
    val commits = Vec(CommitWidth, Valid(new RoqCommit))
  })
}
