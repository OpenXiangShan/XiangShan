package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utility.SignExt
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.fu.{BranchModule, FuConfig, FuncUnit}
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.frontend.tracertl.TraceRTLChoose
import xiangshan.{RedirectLevel, XSModule}
import utils.XSError

class AddrAddModule(len: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val pc = Input(UInt(len.W))
    val taken = Input(Bool())
    val isRVC = Input(Bool())
    val offset = Input(UInt(12.W)) // branch inst only support 12 bits immediate num
    val target = Output(UInt(len.W))
  })
  io.target := io.pc + Mux(io.taken,
    SignExt(ImmUnion.B.toImm32(io.offset), len),
    Mux(io.isRVC, 2.U, 4.U)
  )
}

class BranchUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  val dataModule = Module(new BranchModule)
  val addModule = Module(new AddrAddModule(VAddrData().dataWidth))
  dataModule.io.src(0) := io.in.bits.data.src(0) // rs1
  dataModule.io.src(1) := io.in.bits.data.src(1) // rs2
  dataModule.io.func := io.in.bits.ctrl.fuOpType
  dataModule.io.pred_taken := io.in.bits.ctrl.predictInfo.get.taken

  addModule.io.pc := io.in.bits.data.pc.get // pc
  addModule.io.offset := io.in.bits.data.imm // imm
  addModule.io.taken := dataModule.io.taken
  addModule.io.isRVC := io.in.bits.ctrl.preDecode.get.isRVC

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  io.out.bits.res.data := 0.U
  io.out.bits.res.redirect.get match {
    case redirect =>
      redirect.valid := io.out.valid && dataModule.io.mispredict
      redirect.bits := 0.U.asTypeOf(io.out.bits.res.redirect.get.bits)
      redirect.bits.level := RedirectLevel.flushAfter
      redirect.bits.robIdx := io.in.bits.ctrl.robIdx
      redirect.bits.ftqIdx := io.in.bits.ctrl.ftqIdx.get
      redirect.bits.ftqOffset := io.in.bits.ctrl.ftqOffset.get
      redirect.bits.cfiUpdate.isMisPred := TraceRTLChoose(dataModule.io.mispredict,
        io.in.bits.ctrl.traceInfo.branchTaken(0) =/= dataModule.io.taken,
      )
      redirect.bits.cfiUpdate.taken := TraceRTLChoose(dataModule.io.taken, io.in.bits.ctrl.traceInfo.branchTaken(0))
      redirect.bits.cfiUpdate.predTaken := dataModule.io.pred_taken
      redirect.bits.cfiUpdate.target := TraceRTLChoose(addModule.io.target, io.in.bits.ctrl.traceInfo.target)
      redirect.bits.cfiUpdate.pc := io.in.bits.data.pc.get
  }
  if (env.TraceRTLMode) {
    dontTouch(io.in.bits.ctrl.traceInfo)
    XSError(io.in.valid && (io.in.bits.ctrl.traceInfo.branchType === 0.U), "Trace \n")
  }

  connect0LatencyCtrlSingal
}
