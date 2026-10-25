package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.SelectOne
import xiangshan.XSModule

class EnqPolicyIO(implicit p: IssueBlockParams) extends Bundle {
  val canEnq = Input(UInt((p.numEntries-p.numEnq).W))
  val enqSelOHVec = Vec(p.numEnq, ValidIO(UInt((p.numEntries-p.numEnq).W)))
}

/** Combinationally select available destinations using SelectOne's circ policy.
  * With two ports, select the lowest and highest free slot. The caller checks
  * grant validity and group capacity before accepting the one-hot destinations.
  */
class EnqPolicy(implicit p: Parameters, iqP: IssueBlockParams) extends XSModule {
  val io = IO(new EnqPolicyIO)

  val canEnqVec = io.canEnq.asBools
  // Valid outputs name distinct free slots; no history is stored by this module.
  val selVec: Seq[(Bool, Vec[Bool])] = io.enqSelOHVec.indices.map(i => SelectOne("circ", canEnqVec, iqP.numEnq).getNthOH(i + 1))

  io.enqSelOHVec.zip(selVec).foreach { case (enqOH, (selValid, selOH)) =>
    enqOH.valid := selValid
    enqOH.bits := selOH.asUInt
  }
}
