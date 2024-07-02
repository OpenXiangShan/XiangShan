package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import fudian.SignExt
import xiangshan.RedirectLevel
import xiangshan.backend.fu.{FuConfig, FuncUnit, JumpDataModule, PipedFuncUnit}
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.frontend.tracertl.TraceRTLChoose
import xiangshan.frontend.tracertl.ChiselRecordForField._


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
  jumpDataModule.io.traceInfo := io.in.bits.ctrl.traceInfo

  val jmpTarget = io.in.bits.ctrl.predictInfo.get.target
  val predTaken = io.in.bits.ctrl.predictInfo.get.taken

  val redirect = io.out.bits.res.redirect.get.bits
  val redirectValid = io.out.bits.res.redirect.get.valid
  redirectValid := io.in.valid && !jumpDataModule.io.isAuipc
  redirect := 0.U.asTypeOf(redirect)
  redirect.specifyField(
    _.level := RedirectLevel.flushAfter,
    _.robIdx := io.in.bits.ctrl.robIdx,
    _.ftqIdx := io.in.bits.ctrl.ftqIdx.get,
    _.ftqOffset := io.in.bits.ctrl.ftqOffset.get,
    _.cfiUpdate.predTaken := true.B,
    _.cfiUpdate.taken := true.B,
    _.cfiUpdate.target := TraceRTLChoose(jumpDataModule.io.target, io.in.bits.ctrl.traceInfo.target),
    _.cfiUpdate.isMisPred := jumpDataModule.io.target(VAddrData().dataWidth - 1, 0) =/= jmpTarget || !predTaken,
    _.cfiUpdate.pc := io.in.bits.data.pc.get,
    _.traceInfo := io.in.bits.ctrl.traceInfo,
  )
//  redirect.debug_runahead_checkpoint_id := uop.debugInfo.runahead_checkpoint_id // Todo: assign it

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.res.data := jumpDataModule.io.result
  connect0LatencyCtrlSingal
}
