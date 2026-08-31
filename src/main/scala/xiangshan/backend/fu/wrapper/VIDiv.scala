package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.vector.{Mgu, VecNonPipedFuncUnit}
import xiangshan.backend.rob.RobPtr
import xiangshan.ExceptionNO
import yunsuan.vector.VectorIdiv
import xiangshan.backend.decode.opcode.Opcode.VIDivOpcodes
import xiangshan.backend.vector.fu.VecNoFixLatFunc
import xiangshan.backend.vector.fu.VecFuConfig

class VIDiv(cfg: VecFuConfig)(implicit p: Parameters) extends VecNoFixLatFunc(cfg) {
  out.ex := 0.U.asTypeOf(out.ex)
}
