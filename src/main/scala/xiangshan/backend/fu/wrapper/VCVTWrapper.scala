package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.vector.fu.Func._
import xiangshan.backend.vector.fu.{VecFixLatFunc, VecFuConfig}

// TODO: yunsuan API mismatch (VectorConvert not in our yunsuan at 4fbbab3)
// Golden commit 8f050e76e7 needs yunsuan update to 78609d85+ before full impl
class VCVTWrapper(cfg: VecFuConfig)(implicit p: Parameters) extends VecFixLatFunc(cfg) {
  out.ex.foreach { lane =>
    lane.valid := false.B
    lane.bits := DontCare
  }
}
