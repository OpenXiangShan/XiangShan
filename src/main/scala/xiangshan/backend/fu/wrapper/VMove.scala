package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSError
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.Bundles.VLmul
import xiangshan.backend.fu.vector.Utils.VecDataToMaskDataVec
import xiangshan.backend.fu.vector.{Mgu, VecPipedFuncUnit}
import xiangshan.backend.fu.fpu.FPU
import yunsuan.encoding.Opcode.Opcodes.VMoveOpcode
import yunsuan.vector.VectorMove.VectorMove
import xiangshan.backend.vector.fu._
import yunsuan.vector.Common._

class VMove(cfg: VecFuConfig)(implicit p: Parameters) extends VecFixLatFunc(cfg) {
  // XSError(io.in.valid && !VMoveOpcode.isLegal(io.in.bits.ctrl.fuOpType), "VMove OpType not supported")

  private implicit val opcode: UInt = fuOpType
  private val elemWidth = VMoveOpcode.getElemWidth
  // param alias
  private val dataWidth = cfg.destDataBits
  private val vlenb = VLEN / 8

  private val vMove = Module(new VectorMove)

  vMove match {
    case mod =>
      mod.io.in.valid := ex(0).valid
      mod.io.in.bits.opcode := fuOpType.take(VMoveOpcode.getWidth)
      mod.io.in.bits.info.vm := ex0vm
      mod.io.in.bits.info.vsew := elemWidth
      mod.io.in.bits.vs2 := ex0vs2
      mod.io.in.bits.vs1 := ex0vs1
      mod.io.in.bits.mask := ex(0).data.v0.get // Todo: use 16b mask instead
  }

  private val intResult = vMove.io.out.bits.vd
  private val fpResult = Mux1H(Seq(
    (elemWidth === VSew.e16) -> FPU.box(intResult, FPU.f16),
    (elemWidth === VSew.e32) -> FPU.box(intResult, FPU.f32),
    (elemWidth === VSew.e64) -> FPU.box(intResult, FPU.f64),
  ))

  out.ex(0).data.int.foreach(_ := intResult)
  out.ex(0).data.fp.foreach(_ := fpResult)
  out.ex(0).data.vec.foreach(_.normal := vMove.io.out.bits.vd)
  out.ex(0).data.vec.foreach(_.narrow := 0.U)
  out.ex(0).data.vec.foreach(_.maskE8 := 0.U)
  out.ex(0).data.vec.foreach(_.maskE16 := 0.U)
  out.ex(0).data.vec.foreach(_.maskE32 := 0.U)
  out.ex(0).data.vec.foreach(_.maskE64 := 0.U)
}
