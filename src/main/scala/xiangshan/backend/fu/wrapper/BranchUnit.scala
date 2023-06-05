package xiangshan.backend.fu.wrapper

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import utility.SignExt
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.fu.{BranchModule, FuConfig, PipedFuncUnit}
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.{RedirectLevel, XSModule}

class AddrAddModule(len: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val pc = Input(UInt(len.W))
    val offset = Input(UInt(12.W)) // branch inst only support 12 bits immediate num
    val target = Output(UInt(len.W))
  })
  io.target := io.pc + SignExt(ImmUnion.B.toImm32(io.offset), len)
}

class BranchUnit(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  val dataModule = Module(new BranchModule)
  val addModule = Module(new AddrAddModule(VAddrData().dataWidth))
  dataModule.io.src(0) := io.in.bits.data.src(0) // rs1
  dataModule.io.src(1) := io.in.bits.data.src(1) // rs2
  dataModule.io.func := io.in.bits.ctrl.fuOpType
  dataModule.io.pred_taken := io.in.bits.ctrl.predictInfo.get.taken

  addModule.io.pc := io.in.bits.data.pc.get // pc
  addModule.io.offset := io.in.bits.data.imm // imm

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
      redirect.bits.cfiUpdate.isMisPred := dataModule.io.mispredict
      redirect.bits.cfiUpdate.taken := dataModule.io.taken
      redirect.bits.cfiUpdate.predTaken := dataModule.io.pred_taken
      redirect.bits.cfiUpdate.target := addModule.io.target
  }
  connectNonPipedCtrlSingal
}
