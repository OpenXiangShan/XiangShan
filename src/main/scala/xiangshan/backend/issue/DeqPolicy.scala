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

/** Combinationally select distinct requests for identical-FU dequeue ports.
  * SelectOne's circ policy alternates low-end and high-end priority: with two
  * ports, grant 0 is the lowest request and grant 1 is the highest, if distinct.
  */
class DeqPolicy(implicit p: Parameters, iqP: IssueBlockParams) extends XSModule {
  val io = IO(new DeqPolicyIO)

  private val requestVec = VecInit(io.request.asBools)
  // Return each grant's validity separately when fewer than numDeq requests exist.
  private val selVec: Seq[(Bool, Vec[Bool])] = io.deqSelOHVec.indices.map(i => SelectOne("circ", requestVec, iqP.numDeq).getNthOH(i + 1))

  io.deqSelOHVec.zip(selVec).foreach { case (deqOH, (selValid, selOH)) =>
    deqOH.valid := selValid
    deqOH.bits := selOH.asUInt
  }
}
