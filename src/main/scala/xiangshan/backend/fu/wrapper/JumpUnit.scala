package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import fudian.SignExt
import xiangshan.RedirectLevel
import xiangshan.backend.fu.{FuConfig, FuncUnit, JumpDataModule, PipedFuncUnit}
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.frontend.tracertl.TraceRTLChoose


class JumpUnit(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  private val jumpDataModule = Module(new JumpDataModule)

  private val flushed = io.in.bits.ctrl.robIdx.needFlush(io.flush)

  // associated with AddrData's position of JmpCfg.srcData
  private val src = io.in.bits.data.src(0)
  private val pc = SignExt(io.in.bits.data.pc.get, cfg.destDataBits)
  private val immMin = io.in.bits.data.imm
  private val func = io.in.bits.ctrl.fuOpType
  private val isRVC = io.in.bits.ctrl.preDecode.get.isRVC

  jumpDataModule.io.src := src
  jumpDataModule.io.pc := pc
  jumpDataModule.io.immMin := immMin
  jumpDataModule.io.func := func
  jumpDataModule.io.isRVC := isRVC

  val jmpTarget = io.in.bits.ctrl.predictInfo.get.target
  val predTaken = io.in.bits.ctrl.predictInfo.get.taken

  val redirect = io.out.bits.res.redirect.get.bits
  val redirectValid = io.out.bits.res.redirect.get.valid
  redirectValid := io.in.valid && !jumpDataModule.io.isAuipc
  redirect := 0.U.asTypeOf(redirect)
  redirect.level := RedirectLevel.flushAfter
  redirect.robIdx := io.in.bits.ctrl.robIdx
  redirect.ftqIdx := io.in.bits.ctrl.ftqIdx.get
  redirect.ftqOffset := io.in.bits.ctrl.ftqOffset.get
  redirect.cfiUpdate.predTaken := true.B
  redirect.cfiUpdate.taken := true.B
  redirect.cfiUpdate.target := TraceRTLChoose(jumpDataModule.io.target, io.in.bits.ctrl.traceInfo.target)
  redirect.cfiUpdate.pc := io.in.bits.data.pc.get
  val targetPredWrong =  jumpDataModule.io.target(VAddrData().dataWidth - 1, 0) =/= jmpTarget
  val predTakenFixed = WireInit(predTaken)
  val targetPredWrongFixed = WireInit(targetPredWrong)
  if (env.TraceRTLMode) {
    dontTouch(io.in.bits.ctrl.traceInfo)
    val inst = io.in.bits.ctrl.traceInfo.inst
    val isJal = Mux(isRVC(inst),
      (inst(1,0) === "b01".U) && (inst(15,13) === "b001".U),
      (inst(4,2) === "b011".U) && (inst(6,5) === "b11".U)
    )
    // NOTE:
    // 1. jal's target already redirected. should not be redirected again.
    // 2. jump should always be predicted to be true(when false, preCheck by IFU's predChecker)
    predTakenFixed := true.B
    when (isJal)  {
      targetPredWrongFixed := false.B
    }
  }
  redirect.cfiUpdate.isMisPred := targetPredWrongFixed || !predTakenFixed
//  redirect.debug_runahead_checkpoint_id := uop.debugInfo.runahead_checkpoint_id // Todo: assign it

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.res.data := jumpDataModule.io.result
  connect0LatencyCtrlSingal
}
