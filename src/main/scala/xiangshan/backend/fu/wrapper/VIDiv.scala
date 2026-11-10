package xiangshan.backend.fu.wrapper

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.vector.fu.VecNonFixedLatFunc
import xiangshan.backend.vector.fu.VecFuConfig
import xiangshan.backend.vector.fu.Func.makePipeReg
import yunsuan.encoding.Opcode.Opcodes.VIDivOpcode
import yunsuan.vector.VectorIdiv

class VIDiv(cfg: VecFuConfig)(implicit p: Parameters) extends VecNonFixedLatFunc(cfg) {
  private val vidiv = Module(new VectorIdiv)

  private val outIsDiv = RegInit(false.B)
  private val divInFire = ex(0).valid && vidiv.in.ex0.ready

  private val ex0NextOpcode = ex0Next.bits.ctrl.opcode

  private val sel8 = makePipeReg(VIDivOpcode.isSourceE8(ex0NextOpcode), pipeRegValids)
  private val sel16 = makePipeReg(VIDivOpcode.isSourceE16(ex0NextOpcode), pipeRegValids)
  private val sel32 = makePipeReg(VIDivOpcode.isSourceE32(ex0NextOpcode), pipeRegValids)
  private val sel64 = makePipeReg(VIDivOpcode.isSourceE64(ex0NextOpcode), pipeRegValids)
  private val isDiv = makePipeReg(VIDivOpcode.isDiv(ex0NextOpcode), pipeRegValids)
  private val isSigned = makePipeReg(VIDivOpcode.isSigned(ex0NextOpcode), pipeRegValids)

  when(divInFire) {
    outIsDiv := isDiv.ex0
  }
  latchNonFixedLatOutCtrl(divInFire)

  private val divFlush = nonFixedLatOutCtrl.robIdx.needFlush(in.flush)

  vidiv.in.ex0.valid := ex(0).valid
  vidiv.in.ex0.bits.ctrl.sign := isSigned.ex0
  vidiv.in.ex0.bits.ctrl.flush := divFlush
  vidiv.in.ex0.bits.ctrl.sel8 := sel8.ex0
  vidiv.in.ex0.bits.ctrl.sel16 := sel16.ex0
  vidiv.in.ex0.bits.ctrl.sel32 := sel32.ex0
  vidiv.in.ex0.bits.ctrl.sel64 := sel64.ex0
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
