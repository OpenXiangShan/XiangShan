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
  private val divInFire = ex(0).valid && vidiv.in.ex0.ready

  private val ex0NextOpcode = ex0Next.bits.ctrl.opcode
  private val sew = makePipeReg(VIDivOpcodes.getDataWidth(ex0NextOpcode), pipeRegValids)
  private val isDiv = makePipeReg(VIDivOpcodes.isDiv(ex0NextOpcode), pipeRegValids)
  private val isSigned = makePipeReg(VIDivOpcodes.isSigned(ex0NextOpcode), pipeRegValids)

  when(divInFire) {
    outIsDiv := isDiv.ex0
  }
  latchNonFixedLatOutCtrl(divInFire)

  private val divFlush = nonFixedLatOutCtrl.robIdx.needFlush(in.flush)

  vidiv.in.ex0.valid := ex(0).valid
  vidiv.in.ex0.bits.ctrl.sign := isSigned.ex0
  vidiv.in.ex0.bits.ctrl.flush := divFlush
  vidiv.in.ex0.bits.ctrl.sel8 := sew.ex0 === 0.U
  vidiv.in.ex0.bits.ctrl.sel16 := sew.ex0 === 1.U
  vidiv.in.ex0.bits.ctrl.sel32 := sew.ex0 === 2.U
  vidiv.in.ex0.bits.ctrl.sel64 := sew.ex0 === 3.U
  vidiv.in.ex0.bits.data.dividend_v := ex0vs2
  vidiv.in.ex0.bits.data.divisor_v := ex0vs1

  vidiv.out.ex0.ready := true.B

  private val resultData = Mux(outIsDiv, vidiv.out.ex0.bits.q_v, vidiv.out.ex0.bits.rem_v)

  out.ex(0).valid := vidiv.out.ex0.valid

  out.ex(0).bits.data.vec.foreach { vecData =>
    vecData := 0.U.asTypeOf(vecData)
    vecData.normal := resultData
  }

  outFuLat.valid := vidiv.out.ex0.bits.div_latency.valid
  outFuLat.bits := vidiv.out.ex0.bits.div_latency.bits
  connectNonFixedLatWakeUp(vidiv.out.ex0.bits.div_latency, divFlush, vidiv.out.ex0.valid)
}
