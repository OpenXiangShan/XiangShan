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
import yunsuan.VidivType
import yunsuan.vector.VectorIdiv

class VIDiv(cfg: FuConfig)(implicit p: Parameters) extends VecNonPipedFuncUnit(cfg) {
  XSError(io.in.valid && io.in.bits.ctrl.fuOpType === VidivType.dummy, "Vfdiv OpType not supported")

  // params alias
  private val dataWidth = cfg.destDataBits

  // modules
  private val vidiv = Module(new VectorIdiv)
  private val mgu = Module(new Mgu(dataWidth))

  private val thisRobIdx = Wire(new RobPtr)
  when(io.in.ready){
    thisRobIdx := io.in.bits.ctrl.robIdx
  }.otherwise{
    thisRobIdx := outCtrl.robIdx
  }

  /**
    * [[vidiv]]'s in connection
    */
  vidiv.io match {
    case subIO =>
      subIO.div_in_valid  := io.in.valid
      subIO.div_out_ready := io.out.ready & io.out.valid
      subIO.sew           := vsew
      subIO.sign          := VidivType.isSigned(fuOpType)
      subIO.dividend_v    := vs2
      subIO.divisor_v     := vs1
      subIO.flush         := thisRobIdx.needFlush(io.flush)
  }

  io.in.ready  := vidiv.io.div_in_ready
  io.out.valid := vidiv.io.div_out_valid

  private val outFuOpType = outCtrl.fuOpType
  private val outIsDiv = VidivType.isDiv(outFuOpType)
  private val resultData = Mux(outIsDiv, vidiv.io.div_out_q_v, vidiv.io.div_out_rem_v)
  private val notModifyVd = outVl === 0.U

  mgu.io.in.vd := resultData
  mgu.io.in.oldVd := outOldVd
  mgu.io.in.mask := outSrcMask
  mgu.io.in.info.ta := outVecCtrl.vta
  mgu.io.in.info.ma := outVecCtrl.vma
  mgu.io.in.info.vl := outVl
  mgu.io.in.info.vlmul := outVecCtrl.vlmul
  mgu.io.in.info.valid := io.out.valid
  mgu.io.in.info.vstart := outVecCtrl.vstart
  mgu.io.in.info.eew := outVecCtrl.vsew
  mgu.io.in.info.vsew := outVecCtrl.vsew
  mgu.io.in.info.vdIdx := outVecCtrl.vuopIdx
  mgu.io.in.info.narrow := outVecCtrl.isNarrow
  mgu.io.in.info.dstMask := outVecCtrl.isDstMask
  mgu.io.in.isIndexedVls := false.B
  io.out.bits.res.data := Mux(notModifyVd, outOldVd, mgu.io.out.vd)
  io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := mgu.io.out.illegal
}
