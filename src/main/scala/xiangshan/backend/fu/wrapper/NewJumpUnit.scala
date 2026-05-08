package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{SignExt, XSError, ZeroExt}
import xiangshan.{NewJumpOpType, Redirect, RedirectLevel}
import xiangshan.backend.fu.{FuConfig, PipedFuncUnit}
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.BranchAttribute.BranchType

class NewJumpUnit(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  // associated with AddrData's position of JmpCfg.srcData
  private val src = io.in.bits.data.src(0)
  private val pc = io.instrAddrTransType.get.extend(io.in.bits.data.pc.get, cfg.destDataBits)
  private val imm = io.in.bits.data.imm
  private val func = io.in.bits.ctrl.fuOpType
  // XSError(func =/= NewJumpOpType.j && func =/= NewJumpOpType.jr, p"func ${Binary(func)} is not new_jump type uop")

  // j:  pc       + imm -> pc
  // ja: GPR[rs1] + imm -> pc
  private val isJr = NewJumpOpType.jumpUopisjr(func)
  private val jumpTarget = Mux(isJr, src, pc) + SignExt(imm, XLEN)

  private val fixedTaken = io.in.bits.ctrl.predictInfo.get.fixedTaken
  private val predTaken = io.in.bits.ctrl.predictInfo.get.predTaken
  private val jmpPredictTarget = io.in.bits.ctrl.predictInfo.get.target
  private val jumpRealTarget = jumpTarget(VAddrData().dataWidth - 1, 0)

  private val targetWrong = jumpRealTarget =/= jmpPredictTarget
  private val needRedirect = !fixedTaken || targetWrong
  private val needTrain = !predTaken || targetWrong

  val redirect: Redirect = io.out.bits.res.redirect.get.bits
  val redirectValid: Bool = io.out.bits.res.redirect.get.valid
  redirectValid := io.in.valid && (needRedirect || redirect.hasBackendFault)
  redirect := 0.U.asTypeOf(redirect)
  redirect.level := RedirectLevel.flushAfter
  redirect.robIdx := io.in.bits.ctrl.robIdx
  redirect.ftqIdx := io.in.bits.ctrl.ftqIdx.get
  redirect.ftqOffset := io.in.bits.ctrl.ftqOffset.get
  redirect.fullTarget := jumpTarget
  redirect.taken := true.B
  redirect.target := jumpTarget
  redirect.pc := io.in.bits.data.pc.get
  redirect.isMisPred := needRedirect
  redirect.backendIAF := io.instrAddrTransType.get.checkAccessFault(jumpTarget)
  redirect.backendIPF := io.instrAddrTransType.get.checkPageFault(jumpTarget)
  redirect.backendIGPF := io.instrAddrTransType.get.checkGuestPageFault(jumpTarget)
  redirect.attribute := io.toFrontendBJUResolve.get.bits.attribute
  //  redirect.debug_runahead_checkpoint_id := uop.debugInfo.runahead_checkpoint_id // Todo: assign it

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.res.data := 0.U
  io.toFrontendBJUResolve.get.valid := io.out.valid
  io.toFrontendBJUResolve.get.bits.ftqIdx := io.in.bits.ctrl.ftqIdx.get
  io.toFrontendBJUResolve.get.bits.ftqOffset := io.in.bits.ctrl.ftqOffset.get
  io.toFrontendBJUResolve.get.bits.pc := PrunedAddrInit(pc)
  io.toFrontendBJUResolve.get.bits.target := PrunedAddrInit(jumpTarget)
  io.toFrontendBJUResolve.get.bits.taken := true.B
  io.toFrontendBJUResolve.get.bits.mispredict := needTrain
  io.toFrontendBJUResolve.get.bits.attribute.branchType := Mux(isJr, BranchType.Indirect, BranchType.Direct) // jr / j
  io.toFrontendBJUResolve.get.bits.attribute.rasAction := io.in.bits.ctrl.rasAction.get
  io.toFrontendBJUResolve.get.bits.debug_isRVC.foreach(_ := io.in.bits.ctrl.isRVC.get)
  connect0LatencyCtrlSingal
}
