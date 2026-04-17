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
import xiangshan.backend.vector.fu.util.VecNoFixLatFunc
import xiangshan.backend.vector.fu.util.VecFuConfig

class VIDiv(cfg: VecFuConfig)(implicit p: Parameters) extends VecNoFixLatFunc(cfg) {
  // XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VidivType.dummy, "Vfdiv OpType not supported")

  // params alias
  private val dataWidth = cfg.destDataBits

  // modules
  private val vidiv = Module(new VectorIdiv)

  private val thisRobIdx = Wire(new RobPtr)
//  when(vidiv.io.div_in_ready) { //note
//    thisRobIdx := io.in(0).bits.ctrl.robIdx
//  }.otherwise {
//    thisRobIdx := outCtrl.robIdx
//  }
//
//  /**
//   * [[vidiv]]'s in connection
//   */
//  vidiv.io match {
//    case subIO =>
//      subIO.div_in_valid := io.in(0).valid
//      subIO.div_out_ready := io.out(0).ready & io.out(0).valid
//      subIO.sew := vsew
//      subIO.sign := VIDivOpcodes.isSigned(fuOpType)
//      subIO.dividend_v := ex0vs2
//      subIO.divisor_v := ex0vs1
//      subIO.flush := thisRobIdx.needFlush(io.flush)
//  }
//
//  // io.in.ready  := vidiv.io.div_in_ready note
//  io.out(0).valid := vidiv.io.div_out_valid
//
//  private val outFuOpType = outCtrl.fuOpType
//  private val outIsDiv = VIDivOpcodes.isDiv(outFuOpType)
//  private val resultData = Mux(outIsDiv, vidiv.io.div_out_q_v, vidiv.io.div_out_rem_v)
//  private val notModifyVd = outVl === 0.U
//
//  io.out(0).bits.res.data := Mux(notModifyVd, outOldVd, mgu.io.out.vd)
//  io.out(0).bits.ctrl.exceptionVec(ExceptionNO.illegalInstr) := mgu.io.out.illegal
}
