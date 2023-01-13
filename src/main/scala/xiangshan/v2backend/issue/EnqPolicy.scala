package xiangshan.v2backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utility.SelectOne
import xiangshan.XSModule

class EnqPolicyIO(implicit p: IssueQueueParams) extends Bundle {
  val validVec = Input(UInt(p.numEntries.W))
  val enqSelOHVec = Vec(p.numEnq, ValidIO(UInt(p.numEntries.W)))
}

class EnqPolicy(implicit p: Parameters, iqP: IssueQueueParams) extends XSModule {
  val io = IO(new EnqPolicyIO)

  val emptyVec = io.validVec.asBools.map(!_)
  // Todo: support more policies
  val selVec = io.enqSelOHVec.indices.map(i => SelectOne("center", emptyVec, iqP.numEnq).getNthOH(i + 1))

  io.enqSelOHVec.zip(selVec).foreach { case (enqOH, (selValid, selOH)) =>
    enqOH.valid := selValid
    enqOH.bits := selOH
  }
}
