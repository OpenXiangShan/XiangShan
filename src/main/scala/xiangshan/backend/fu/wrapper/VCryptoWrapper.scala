package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.decode.opcode.Opcode.VCryptoOpcodes
import xiangshan.backend.vector.fu.{VecFixLatFunc, VecFuConfig}
import yunsuan.vector.VectorALU.VCrypto

class VectorCryptoWrapper(cfg: VecFuConfig)(implicit p: Parameters) extends VecFixLatFunc(cfg) {
  require(cfg.latency == VectorCryptoWrapper.Latency)

  private val vicrypto = Module(new VCrypto)

  private val cryptoOpcode = MuxLookup(
    fuOpType,
    VCrypto.Opcode.vclmul
  )(Seq(
    VCryptoOpcodes.vclmulh.encode.value.U -> VCrypto.Opcode.vclmulh
  ))

  vicrypto.io.in.valid             := in.ex.head.valid
  vicrypto.io.in.bits.opcode.op    := cryptoOpcode
  vicrypto.io.in.bits.vs1          := ex0vs1
  vicrypto.io.in.bits.vs2          := ex0vs2
  vicrypto.io.in.bits.old_vd       := ex0oldVd

  out.ex.zipWithIndex.foreach { case (outStage, stage) =>
    outStage.bits.data.vec.foreach { vecData =>
      vecData.normal := (if (stage == VectorCryptoWrapper.Latency) vicrypto.io.out.bits.vd else 0.U)
      vecData.narrow := 0.U
      vecData.maskE8 := 0.U
      vecData.maskE16 := 0.U
      vecData.maskE32 := 0.U
      vecData.maskE64 := 0.U
      vecData.isWiden.foreach(_ := false.B)
      vecData.isNarrow.foreach(_ := false.B)
      vecData.vxsatE8.foreach(_ := false.B)
      vecData.narrowVxsatE8.foreach(_ := false.B)
    }
  }

  private val expectedOutValid =
    in.ex(VectorCryptoWrapper.Latency).valid &&
      in.ex(VectorCryptoWrapper.Latency).bits.ctrl.latency === VectorCryptoWrapper.Latency.U

  out.ex(VectorCryptoWrapper.Latency).valid := expectedOutValid && vicrypto.io.out.valid

  assert(!expectedOutValid || vicrypto.io.out.valid, "VCrypto result was not ready at its configured latency")
}

object VectorCryptoWrapper {
  val Latency = 2
}
