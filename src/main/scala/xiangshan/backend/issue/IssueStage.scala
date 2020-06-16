package xiangshan.backend.issue

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.exu.HasExuHelper

class IssueStage extends XSModule with HasExuHelper with NeedImpl{
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val in = Vec(exuConfig.ExuCnt, Flipped(DecoupledIO(new ExuInput)))
    val out = Vec(exuConfig.ExuCnt, DecoupledIO(new ExuInput))
    val exeWbResults = Vec(exuConfig.ExuCnt, Flipped(ValidIO(new ExuOutput)))
  })
}
