package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utility.SignExt
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.fu.{BranchModule, FuConfig, FuncUnit}
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.{RedirectLevel, XSModule}

class AddrAddModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val pc = Input(UInt(VAddrBits.W))
    val taken = Input(Bool())
    val isRVC = Input(Bool())
    val offset = Input(UInt(12.W)) // branch inst only support 12 bits immediate num
    val target = Output(UInt(XLEN.W))
  })
  io.target := SignExt(SignExt(io.pc, VAddrBits + 1) + Mux(io.taken,
    SignExt(ImmUnion.B.toImm32(io.offset), VAddrBits + 1),
    Mux(io.isRVC, 2.U, 4.U)
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
      redirect.bits.fullTarget := addModule.io.target
      redirect.bits.cfiUpdate.isMisPred := dataModule.io.mispredict
      redirect.bits.cfiUpdate.taken := dataModule.io.taken
      redirect.bits.cfiUpdate.predTaken := dataModule.io.pred_taken
      redirect.bits.cfiUpdate.target := addModule.io.target
      redirect.bits.cfiUpdate.pc := io.in.bits.data.pc.get
      redirect.bits.cfiUpdate.backendIAF := io.instrAddrTransType.get.checkAccessFault(addModule.io.target)
      redirect.bits.cfiUpdate.backendIPF := io.instrAddrTransType.get.checkPageFault(addModule.io.target)
      redirect.bits.cfiUpdate.backendIGPF := io.instrAddrTransType.get.checkGuestPageFault(addModule.io.target)
  }
  connect0LatencyCtrlSingal
}
