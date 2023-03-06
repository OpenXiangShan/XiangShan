package xiangshan.v2backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import fudian.SignExt
import xiangshan.RedirectLevel
import xiangshan.backend.fu.JumpDataModule
import xiangshan.v2backend.{FuConfig, VAddrData}


class JumpUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  private val jumpDataModule = Module(new JumpDataModule)

  private val flushed = io.in.bits.robIdx.needFlush(io.flush)

  // associated with AddrData's position of JmpCfg.srcData
  private val src = io.in.bits.src(0)
  private val pc = SignExt(io.in.bits.pc.get, cfg.dataBits)
  private val immMin = io.in.bits.imm
  private val func = io.in.bits.fuOpType
  private val isRVC = io.in.bits.preDecode.get.isRVC

  jumpDataModule.io.src := src
  jumpDataModule.io.pc := pc
  jumpDataModule.io.immMin := immMin
  jumpDataModule.io.func := func
  jumpDataModule.io.isRVC := isRVC

  val jmpTarget = io.in.bits.predictInfo.get.target
  val predTaken = io.in.bits.predictInfo.get.taken

  val redirect = io.out.bits.redirect.get.bits
  val redirectValid = io.out.bits.redirect.get.valid
  redirectValid := io.in.valid && !jumpDataModule.io.isAuipc
  redirect := 0.U.asTypeOf(redirect)
  redirect.level := RedirectLevel.flushAfter
  redirect.robIdx := io.in.bits.robIdx
  redirect.ftqIdx := io.in.bits.ftqIdx.get
  redirect.ftqOffset := io.in.bits.ftqOffset.get
  redirect.cfiUpdate.predTaken := true.B
  redirect.cfiUpdate.taken := true.B
  redirect.cfiUpdate.target := jumpDataModule.io.target
  redirect.cfiUpdate.isMisPred := jumpDataModule.io.target(VAddrData().dataWidth - 1, 0) =/= jmpTarget || !predTaken
//  redirect.debug_runahead_checkpoint_id := uop.debugInfo.runahead_checkpoint_id // Todo: assign it

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.data := jumpDataModule.io.result
  connectCtrlSingal
}
