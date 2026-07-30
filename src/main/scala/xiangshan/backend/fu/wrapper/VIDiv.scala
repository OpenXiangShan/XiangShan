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
import yunsuan.encoding.Opcode.Opcodes.VIDivOpcode
import yunsuan.vector.VectorIdiv

class VIDiv(cfg: FuConfig)(implicit p: Parameters) extends VecNonPipedFuncUnit(cfg) {
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
  vidiv.in.ex0.valid  := io.in.valid
  vidiv.out.ex0.ready := io.out.ready & io.out.valid
  vidiv.in.ex0.bits.ctrl.sel64 := true.B // FIXME: proper SEW routing
  vidiv.in.ex0.bits.ctrl.sign  := VIDivOpcode.isSigned(fuOpType)
  vidiv.in.ex0.bits.data.dividend_v := vs2
  vidiv.in.ex0.bits.data.divisor_v  := vs1
  vidiv.in.ex0.bits.ctrl.flush := thisRobIdx.needFlush(io.flush)

  io.in.ready  := vidiv.in.ex0.ready
  io.out.valid := vidiv.out.ex0.valid

  private val outFuOpType = outCtrl.fuOpType
  private val outIsDiv = VIDivOpcode.isDiv(outFuOpType)
  private val resultData = Mux(outIsDiv, vidiv.out.ex0.bits.q_v, vidiv.out.ex0.bits.rem_v)
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
  io.out.bits.ctrl.exceptionVec(ExceptionNO.illegalInstr) := mgu.io.out.illegal
}
