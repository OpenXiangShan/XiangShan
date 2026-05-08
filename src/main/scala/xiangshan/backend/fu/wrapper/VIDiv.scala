package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import yunsuan.vector.VectorIdiv
import xiangshan.backend.decode.opcode.Opcode.VIDivOpcodes
import xiangshan.backend.vector.fu.VecNonFixedLatFunc
import xiangshan.backend.vector.fu.VecFuConfig
import xiangshan.backend.vector.fu.Func.makePipeReg

class VIDiv(cfg: VecFuConfig)(implicit p: Parameters) extends VecNonFixedLatFunc(cfg) {
  private val vidiv = Module(new VectorIdiv)

  private val outIsDiv = RegInit(false.B)
  private val divInFire = ex(0).valid && vidiv.io.div_in_ready

  private val ex0NextOpcode = ex0Next.bits.ctrl.opcode
  private val sew = makePipeReg(VIDivOpcodes.getDataWidth(ex0NextOpcode), pipeRegValids)
  private val isDiv = makePipeReg(VIDivOpcodes.isDiv(ex0NextOpcode), pipeRegValids)
  private val isSigned = makePipeReg(VIDivOpcodes.isSigned(ex0NextOpcode), pipeRegValids)

  when(divInFire) {
    outIsDiv := isDiv.ex0
  }
  latchNonFixedLatOutCtrl(divInFire)

  private val divFlush = nonFixedLatOutCtrl.robIdx.needFlush(in.flush)

  vidiv.io.div_in_valid := ex(0).valid
  vidiv.io.sew := sew.ex0
  vidiv.io.sign := isSigned.ex0
  vidiv.io.dividend_v := ex0vs2
  vidiv.io.divisor_v := ex0vs1
  vidiv.io.flush := divFlush

  private val resultData = Mux(outIsDiv, vidiv.io.div_out_q_v, vidiv.io.div_out_rem_v)

  out.ex(0).valid := vidiv.io.div_out_valid

  out.ex(0).bits.data.vec.foreach { vecData =>
    vecData := 0.U.asTypeOf(vecData)
    vecData.normal := resultData
  }

  outFuLat.valid := vidiv.io.div_latency.valid
  outFuLat.bits := vidiv.io.div_latency.bits
}
