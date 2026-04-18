package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu, Utils, VecPipedFuncUnit, VecSrcTypeModule}
import xiangshan.ExceptionNO
import yunsuan.VialuFixType
import yunsuan.encoding.{VdType, Vs1IntType, Vs2IntType}
import yunsuan.vector.mac.VIMac64b
import xiangshan.backend.decode.opcode.Opcode.VIMacOpcodes
import xiangshan.backend.vector.fu.VecFixLatFunc
import xiangshan.backend.vector.fu.VecFuConfig

class VIMacU(cfg: VecFuConfig)(implicit p: Parameters) extends VecFixLatFunc(cfg) {
  // Stub: VIMacU requires VecFixLatFunc/VecFuConfig infrastructure not yet ported
  out.ex := 0.U.asTypeOf(out.ex)
}