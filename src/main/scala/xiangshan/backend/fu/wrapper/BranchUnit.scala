package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utility.{SignExt, XSError, XSPerfAccumulate}
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.fu.{BranchModule, FuConfig, FuncUnit}
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.frontend.tracertl.TraceRTLChoose
import xiangshan.{RedirectLevel, XSModule}
import xiangshan.frontend.tracertl.ChiselRecordForField._

class AddrAddModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val pc = Input(UInt(VAddrBits.W))
    val taken = Input(Bool())
    val isRVC = Input(Bool())
    val offset = Input(UInt(12.W)) // branch inst only support 12 bits immediate num
    val target = Output(UInt(XLEN.W))
  })
  val pcExtend = SignExt(io.pc, VAddrBits + 1)
  io.target := SignExt(Mux(io.taken,
  pcExtend + SignExt(ImmUnion.B.toImm32(io.offset), VAddrBits + 1),
  pcExtend + Mux(io.isRVC, 2.U, 4.U)
  ), XLEN)
}

class BranchUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  val dataModule = Module(new BranchModule)
  val addModule = Module(new AddrAddModule)
  dataModule.io.src(0) := io.in.bits.data.src(0) // rs1
  dataModule.io.src(1) := io.in.bits.data.src(1) // rs2
  dataModule.io.func := io.in.bits.ctrl.fuOpType
  dataModule.io.pred_taken := io.in.bits.ctrl.predictInfo.get.taken

  addModule.io.pc := io.in.bits.data.pc.get // pc
  addModule.io.offset := io.in.bits.data.imm // imm
  addModule.io.taken := TraceRTLChoose(dataModule.io.taken, io.in.bits.ctrl.traceInfo.branchTaken(0))
  addModule.io.isRVC := io.in.bits.ctrl.preDecode.get.isRVC

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  // when taken, return traceInfo.target
  // when not taken, return sequential target
  val addModuleTarget = TraceRTLChoose(addModule.io.target,
    Mux(io.in.bits.ctrl.traceInfo.branchTaken(0), SignExt(io.in.bits.ctrl.traceInfo.target, XLEN),
    addModule.io.target))

  io.out.bits.res.data := 0.U
  io.out.bits.res.redirect.get match {
    case redirect =>
      redirect.valid := io.out.valid && redirect.bits.cfiUpdate.isMisPred && TraceRTLChoose(true.B, !io.in.bits.ctrl.traceInfo.isWrongPath && !io.in.bits.ctrl.traceInfo.hasTriggeredExuRedirect)
      redirect.bits := 0.U.asTypeOf(io.out.bits.res.redirect.get.bits)
      redirect.bits.specifyField(
        _.level := RedirectLevel.flushAfter,
        _.robIdx := io.in.bits.ctrl.robIdx,
        _.ftqIdx := io.in.bits.ctrl.ftqIdx.get,
        _.ftqOffset := io.in.bits.ctrl.ftqOffset.get,
        _.fullTarget := addModuleTarget,
        _.cfiUpdate.isMisPred := TraceRTLChoose(dataModule.io.mispredict,
          io.in.bits.ctrl.traceInfo.branchTaken(0) =/= io.in.bits.ctrl.predictInfo.get.taken,
        ),
        _.cfiUpdate.taken := TraceRTLChoose(dataModule.io.taken, io.in.bits.ctrl.traceInfo.branchTaken(0)),
        _.cfiUpdate.predTaken := dataModule.io.pred_taken,
        _.cfiUpdate.target := addModuleTarget,
        _.cfiUpdate.pc := io.in.bits.data.pc.get,
        _.cfiUpdate.backendIAF := io.instrAddrTransType.get.checkAccessFault(addModuleTarget),
        _.cfiUpdate.backendIPF := io.instrAddrTransType.get.checkPageFault(addModuleTarget),
        _.cfiUpdate.backendIGPF := io.instrAddrTransType.get.checkGuestPageFault(addModuleTarget),
        _.traceInfo := io.in.bits.ctrl.traceInfo,
      )
  }
  if (env.TraceRTLMode) {
    dontTouch(io.in.bits.ctrl.traceInfo)
    // XSError(io.in.valid && (io.in.bits.ctrl.traceInfo.branchType === 0.U), "Instruction at BranchUnit is not branch instruction in trace\n")
  }

  val filterWP = TraceRTLChoose(true.B, !io.in.bits.ctrl.traceInfo.isWrongPath)
  XSPerfAccumulate("BranchNum", io.out.fire && filterWP)
  XSPerfAccumulate("BranchNumMisPred", io.out.fire &&
    io.out.bits.res.redirect.get.valid &&
    io.out.bits.res.redirect.get.bits.cfiUpdate.isMisPred &&
    filterWP)

  XSPerfAccumulate("BranchTakenNum", io.out.fire && addModule.io.taken && filterWP)
  XSPerfAccumulate("BranchTakenNumMisPred", io.out.fire && addModule.io.taken &&
    io.out.bits.res.redirect.get.valid &&
    io.out.bits.res.redirect.get.bits.cfiUpdate.isMisPred &&
    filterWP)

  XSPerfAccumulate("BranchNonTakenNum", io.out.fire && !addModule.io.taken && filterWP)
  XSPerfAccumulate("BranchNonTakenNumMisPred", io.out.fire && !addModule.io.taken &&
    io.out.bits.res.redirect.get.valid &&
    io.out.bits.res.redirect.get.bits.cfiUpdate.isMisPred &&
    filterWP)

  connect0LatencyCtrlSingal
}
