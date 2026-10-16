package xiangshan.backend.fu.wrapper

import chisel3._
import org.chipsalliance.cde.config.Parameters
import utility.SignExt
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.backend.fu.{FuConfig, PipedFuncUnit}
import xiangshan.frontend.{PcInit, PrunedAddrInit}
import xiangshan.frontend.bpu.BranchAttribute.BranchType
import xiangshan.{ExceptionNO, JumpOpType, Redirect, RedirectLevel}

class JumpUnit(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  // associated with AddrData's position of NJmpCfg.srcData
  // LPAD 阅读：对有效 LPAD，src 是 Datapath/Bypass 已取出的 x7 物理版本数据，不再是编号。
  // LPAD 的 imm 走指令立即数通路：Decode 的 20 位标签，经 Imm_U 重建为 {标签,12'b0}。
  private val src = io.in.bits.data.src(0)
  private val pc = io.instrAddrTransType.get.extend(io.in.bits.data.pc.get, cfg.destDataBits)
  private val imm = io.in.bits.data.imm
  private val func = io.in.bits.ctrl.fuOpType
  // 使用当前 uop 从 Decode 携带来的检查标志，不读取实时 ELP；它此时可能早已清零。
  private val isLPAD = io.in.bits.ctrl.ZicfilpInfos.map(_.ZicfilpLPADValid).getOrElse(false.B)
  // XSError(func =/= NewJumpOpType.j && func =/= NewJumpOpType.jr, p"func ${Binary(func)} is not new_jump type uop")

  // j:  pc       + imm -> pc
  // ja: GPR[rs1] + imm -> pc
  private val isJr = JumpOpType.jumpUopisjr(func)
  private val jumpTarget = Mux(isJr, src, pc) + SignExt(imm, XLEN)

  private val predTaken = io.in.bits.ctrl.predictInfo.get.predTaken
  private val jmpPredictTarget = io.in.bits.ctrl.predictInfo.get.target
  private val jumpRealTarget = jumpTarget(VAddrData().dataWidth - 1, 0)

  private val targetWrong = jumpRealTarget =/= jmpPredictTarget
  private val needRedirect = !predTaken || targetWrong
  private val needTrain = !predTaken || targetWrong

  val redirect: Redirect = io.out.bits.res.redirect.get.bits
  val redirectValid: Bool = io.out.bits.res.redirect.get.valid
  // LPAD 虽复用 j 操作类型，内部仍会算 pc+imm，但不应拿这个数当目标重定向。
  // 检查失败通过异常回写进入 ROB，由 ROB/CSR 在精确边界处理 trap。
  redirectValid := io.in.valid && !isLPAD && (needRedirect || redirect.hasBackendFault)
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
  // 受保护 JALR 误预测时，CtrlBlock 据此把推测 ELP 恢复为 1，重新检查正确目标。
  redirect.ZicfilpJalr.foreach(_ := io.in.bits.ctrl.ZicfilpInfos.get.ZicfilpJalr)
  //  redirect.debug_runahead_checkpoint_id := uop.debugInfo.runahead_checkpoint_id // Todo: assign it

  if (HasZicfilp) {
    // A zero LPAD label matches any x7 value.
    val lpadLabel = imm(31, 12)
    // 组合硬件：20 位标签 OR 归约(非零) + 20 位不等比较 + isLPAD 门控。
    // 只比较 x7[31:12]，RV64 高 32 位和低 12 位均不参与。
    // 例如 LPL=0x12345、x7=0x12345000 则成功；LPL=0 时任意 x7 都成功。
    // 零标签这里只屏蔽异常，无法消除 Decode/Issue 已建立的 x7 源依赖。
    io.out.bits.ctrl.exceptionVec(ExceptionNO.softwareCheck) :=
      isLPAD && lpadLabel.orR && src(31, 12) =/= lpadLabel
  }

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.res.data := 0.U
  // 同样关闭 LPAD 的有效分支 resolve/预测器训练；执行完成 valid 仍照常产生。
  io.toFrontendBJUResolve.get.valid := io.out.valid && !isLPAD
  io.toFrontendBJUResolve.get.bits.ftqIdx := io.in.bits.ctrl.ftqIdx.get
  io.toFrontendBJUResolve.get.bits.ftqOffset := io.in.bits.ctrl.ftqOffset.get
  io.toFrontendBJUResolve.get.bits.pc := PcInit(pc(VAddrBits - 1, 0))
  io.toFrontendBJUResolve.get.bits.target := PcInit(jumpTarget(VAddrBits - 1, 0))
  io.toFrontendBJUResolve.get.bits.taken := true.B
  io.toFrontendBJUResolve.get.bits.mispredict := needTrain
  io.toFrontendBJUResolve.get.bits.attribute.branchType := Mux(isJr, BranchType.Indirect, BranchType.Direct) // jr / j
  io.toFrontendBJUResolve.get.bits.attribute.rasAction := io.in.bits.ctrl.rasAction.get
  io.toFrontendBJUResolve.get.bits.debug_isRVC.foreach(_ := io.in.bits.ctrl.isRVC.get)
  connect0LatencyCtrlSingal
}
