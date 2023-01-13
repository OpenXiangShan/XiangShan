package xiangshan.v2backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utility.SelectOne
import xiangshan.XSModule

class DeqPolicyIO(implicit p: IssueQueueParams) extends Bundle {
  val validVec = Input(UInt(p.numEntries.W))
  val deqSelOHVec = Vec(p.numDeq, ValidIO(UInt(p.numEntries.W)))
}

class DeqPolicy(implicit p: Parameters, iqP: IssueQueueParams) extends XSModule {
  val io = IO(new DeqPolicyIO)

  val emptyVec = io.validVec.asBools.map(!_)
  // Todo: support more policies
  val selVec = io.deqSelOHVec.indices.map(i => SelectOne("circ", emptyVec, iqP.numDeq).getNthOH(i + 1))

  io.deqSelOHVec.zip(selVec).foreach { case (deqOH, (selValid, selOH)) =>
    deqOH.valid := selValid
    deqOH.bits := selOH
  }
}
