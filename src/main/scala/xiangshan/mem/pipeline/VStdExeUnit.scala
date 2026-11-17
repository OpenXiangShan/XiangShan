package xiangshan.mem.pipeline

import chisel3._
import chisel3.util.{DecoupledIO, ValidIO}
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSModule
import xiangshan.backend.Bundles
import xiangshan.backend.Bundles.{ExuOutput, MemExuOutput, NewExuOutput}
import xiangshan.backend.exu.ExeUnitParams
import xiangshan.mem.StoreQueueDataWrite

class VStdExeUnit(val param: ExeUnitParams)(implicit p: Parameters) extends XSModule {
  import VStdExeUnit._

  val in = IO(new In()(param, p))
  val out = IO(new Out()(param, p))

  out.wbToSQ match {
    case wb =>
      wb.valid := in.uop.valid
      wb.bits match {
        case bits =>
          bits.fuType := in.uop.bits.fuType
          bits.fuOpType := in.uop.bits.fuOpType
          bits.data := in.uop.bits.src(0)
          bits.sqIdx := in.uop.bits.sqIdx.get
          bits.vecDebug.foreach(_.start := 0.U)
          bits.vecDebug.foreach(_.offset := 0.U)
      }
  }

  out.wbToBackend match {
    case wb: NewExuOutput =>
      wb := 0.U.asTypeOf(wb)
      wb.toRob.valid := in.uop.valid
      wb.toRob.bits.robIdx := in.uop.bits.robIdx
      wb.toRob.bits.sqIdx.get := in.uop.bits.sqIdx.get
      wb.perfDebugInfo.foreach(_ := in.uop.bits.perfDebugInfo.get)
      wb.debug_seqNum.foreach(_ := in.uop.bits.debug_seqNum.get)
      in.uop.ready := true.B
  }
}

object VStdExeUnit {
  class In(implicit val param: ExeUnitParams, p: Parameters) extends Bundle {
    val uop: DecoupledIO[Bundles.ExuInput] = Flipped(DecoupledIO(param.genExuInputBundle))
  }

  class Out(implicit val param: ExeUnitParams, p: Parameters) extends Bundle {
    val wbToSQ = ValidIO(new StoreQueueDataWrite)
    val wbToBackend: NewExuOutput = new NewExuOutput(param)
  }
}
