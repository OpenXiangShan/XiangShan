package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.SelectOne
import xiangshan.XSModule

class DeqPolicyIO(implicit p: IssueBlockParams) extends Bundle {
  val request = Input(UInt(p.numEntries.W))
  val deqSelOHVec = Vec(p.numDeq, ValidIO(UInt(p.numEntries.W)))
}

class DeqPolicy(implicit p: Parameters, iqP: IssueBlockParams) extends XSModule {
  val io = IO(new DeqPolicyIO)

  private val requestVec = VecInit(io.request.asBools)
  // Todo: support more policies
  private val selVec: Seq[(Bool, Vec[Bool])] = io.deqSelOHVec.indices.map(i => SelectOne("circ", requestVec, iqP.numDeq).getNthOH(i + 1))

  io.deqSelOHVec.zip(selVec).foreach { case (deqOH, (selValid, selOH)) =>
    deqOH.valid := selValid
    deqOH.bits := selOH.asUInt
  }
}
