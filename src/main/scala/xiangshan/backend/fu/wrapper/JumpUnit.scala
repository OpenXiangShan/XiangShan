package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{SignExt, ZeroExt}
import xiangshan.RedirectLevel
import xiangshan.backend.fu.{FuConfig, FuncUnit, JumpDataModule, PipedFuncUnit}
import xiangshan.JumpOpType
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.BranchAttribute

class JumpUnit(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  private val jumpDataModule = Module(new JumpDataModule)

  private val flushed = io.in.bits.ctrl.robIdx.needFlush(io.flush)

  // associated with AddrData's position of JmpCfg.srcData
  private val src = io.in.bits.data.src(0)
  private val pc = Mux(io.instrAddrTransType.get.shouldBeSext,
    SignExt(io.in.bits.data.pc.get, cfg.destDataBits),
    ZeroExt(io.in.bits.data.pc.get, cfg.destDataBits)
  )
  private val imm = io.in.bits.data.imm
  private val func = io.in.bits.ctrl.fuOpType
  private val isRVC = io.in.bits.ctrl.isRVC.get
  private val rasAction = io.in.bits.ctrl.rasAction.get

  jumpDataModule.io.src := src
  jumpDataModule.io.pc := pc
  jumpDataModule.io.imm := imm
  jumpDataModule.io.nextPcOffset := io.in.bits.data.nextPcOffset.get
  jumpDataModule.io.func := func
  jumpDataModule.io.isRVC := isRVC

  val fixedTaken = io.in.bits.ctrl.predictInfo.get.fixedTaken
  val predTaken  = io.in.bits.ctrl.predictInfo.get.predTaken
  val jmpPredictTarget = io.in.bits.ctrl.predictInfo.get.target
  val jumpRealTarget = jumpDataModule.io.target(VAddrData().dataWidth - 1, 0)

  val targetWrong = jumpRealTarget =/= jmpPredictTarget
  val needRedirect = !fixedTaken || targetWrong
  val needTrain = !predTaken || targetWrong

  val redirect = io.out.bits.res.redirect.get.bits
  val redirectValid = io.out.bits.res.redirect.get.valid
  redirectValid := io.in.valid && !jumpDataModule.io.isAuipc
  redirect := 0.U.asTypeOf(redirect)
  redirect.level := RedirectLevel.flushAfter
  redirect.robIdx := io.in.bits.ctrl.robIdx
  redirect.ftqIdx := io.in.bits.ctrl.ftqIdx.get
  redirect.ftqOffset := io.in.bits.ctrl.ftqOffset.get
  redirect.fullTarget := jumpDataModule.io.target
  redirect.taken := true.B
  redirect.target := jumpDataModule.io.target
  redirect.pc := io.in.bits.data.pc.get
  redirect.isMisPred := needRedirect
  redirect.backendIAF := io.instrAddrTransType.get.checkAccessFault(jumpDataModule.io.target)
  redirect.backendIPF := io.instrAddrTransType.get.checkPageFault(jumpDataModule.io.target)
  redirect.backendIGPF := io.instrAddrTransType.get.checkGuestPageFault(jumpDataModule.io.target)
  redirect.attribute := io.toFrontendBJUResolve.get.bits.attribute
//  redirect.debug_runahead_checkpoint_id := uop.debugInfo.runahead_checkpoint_id // Todo: assign it

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.res.data := jumpDataModule.io.result
  io.toFrontendBJUResolve.get.valid := io.out.valid && !JumpOpType.jumpOpisAuipc(func)
  io.toFrontendBJUResolve.get.bits.ftqIdx := io.in.bits.ctrl.ftqIdx.get
  io.toFrontendBJUResolve.get.bits.ftqOffset := io.in.bits.ctrl.ftqOffset.get
  io.toFrontendBJUResolve.get.bits.pc := PrunedAddrInit(pc)
  io.toFrontendBJUResolve.get.bits.target := PrunedAddrInit(jumpDataModule.io.target)
  io.toFrontendBJUResolve.get.bits.taken := true.B
  io.toFrontendBJUResolve.get.bits.mispredict := needTrain
  io.toFrontendBJUResolve.get.bits.attribute.branchType := MuxCase(
    BranchAttribute.BranchType.None,
    Seq(
      (func === JumpOpType.jal)  -> BranchAttribute.BranchType.Direct,
      (func === JumpOpType.jalr) -> BranchAttribute.BranchType.Indirect
    )
  )
  io.toFrontendBJUResolve.get.bits.attribute.rasAction := rasAction
  if (io.toFrontendBJUResolve.get.bits.debug_isRVC.isDefined) {
    io.toFrontendBJUResolve.get.bits.debug_isRVC.get := io.in.bits.ctrl.isRVC.get
  }
  connect0LatencyCtrlSingal
}
