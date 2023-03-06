package xiangshan.v2backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import utility.SignExt
import xiangshan.{RedirectLevel, XSModule}
import xiangshan.backend.fu.BranchModule
import xiangshan.v2backend.{FuConfig, IntData, VAddrData}

class AddrAddModule(len: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val pc = Input(UInt(len.W))
    val offset = Input(UInt(12.W)) // branch inst only support 12 bits immediate num
    val target = Output(UInt(len.W))
  })
  io.target := io.pc + SignExt(io.offset, len)
}

class BranchUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  val dataModule = Module(new BranchModule)
  val addModule = Module(new AddrAddModule(VAddrData().dataWidth))
  dataModule.io.src(0) := io.in.bits.src(0) // rs1
  dataModule.io.src(1) := io.in.bits.src(1) // rs2
  dataModule.io.func := io.in.bits.fuOpType
  dataModule.io.pred_taken := io.in.bits.predictInfo.get.taken

  addModule.io.pc := io.in.bits.pc.get // pc
  addModule.io.offset := io.in.bits.imm // imm

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  io.out.bits.data := SignExt(addModule.io.target, IntData().dataWidth)

  io.out.bits.redirect.get.valid := io.out.valid && dataModule.io.mispredict
  io.out.bits.redirect.get.bits := 0.U.asTypeOf(io.out.bits.redirect.get.bits)
  io.out.bits.redirect.get.bits.level := RedirectLevel.flushAfter
  io.out.bits.redirect.get.bits.robIdx := io.in.bits.robIdx
  io.out.bits.redirect.get.bits.ftqIdx := io.in.bits.ftqIdx.get
  io.out.bits.redirect.get.bits.ftqOffset := io.in.bits.ftqOffset.get
  io.out.bits.redirect.get.bits.cfiUpdate.isMisPred := dataModule.io.mispredict
  io.out.bits.redirect.get.bits.cfiUpdate.taken := dataModule.io.taken
  io.out.bits.redirect.get.bits.cfiUpdate.predTaken := dataModule.io.pred_taken
  connectCtrlSingal
}
