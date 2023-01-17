package xiangshan.v2backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utility.SelectOne
import xiangshan.XSModule

class EnqPolicyIO(implicit p: IssueQueueParams) extends Bundle {
  val valid = Input(UInt(p.numEntries.W))
  val enqSelOHVec = Vec(p.numEnq, ValidIO(UInt(p.numEntries.W)))
}

class EnqPolicy(implicit p: Parameters, iqP: IssueQueueParams) extends XSModule {
  val io = IO(new EnqPolicyIO)

  val emptyVec = io.valid.asBools.map(!_)
  // Todo: support more policies
  val selVec: Seq[(Bool, Vec[Bool])] = io.enqSelOHVec.indices.map(i => SelectOne("center", emptyVec, iqP.numEnq).getNthOH(i + 1))

  io.enqSelOHVec.zip(selVec).foreach { case (enqOH, (selValid, selOH)) =>
    enqOH.valid := selValid
    enqOH.bits := selOH.asUInt
  }
}
