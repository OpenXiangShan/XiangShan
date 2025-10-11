package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.log2Up
import utility.{SignExt, ZeroExt}
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.fu.{BranchModule, FuConfig, FuncUnit}
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.{RedirectLevel, SelImm, XSModule}
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.BranchAttribute

class AddrAddModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val pcExtend = Input(UInt((VAddrBits + 1).W))
    val taken = Input(Bool())
    val isRVC = Input(Bool())
    val imm = Input(UInt(32.W)) // branch inst only support 12 bits immediate num
    val target = Output(UInt(XLEN.W))
    val nextPcOffset = Input(UInt((FetchBlockInstOffsetWidth + 2).W))
  })
  val immMinWidth = FuConfig.BrhCfg.immType.map(x => SelImm.getImmUnion(x).len).max
  print(s"[Branch]: immMinWidth = $immMinWidth\n")
  io.target := SignExt(Mux(io.taken,
    io.pcExtend + SignExt(io.imm(immMinWidth + 2, 0), VAddrBits + 1),
    io.pcExtend + io.nextPcOffset
  ), XLEN)
}

class BranchUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  val dataModule = Module(new BranchModule)
  val addModule = Module(new AddrAddModule)
  dataModule.io.src(0) := io.in.bits.data.src(0) // rs1
  dataModule.io.src(1) := io.in.bits.data.src(1) // rs2
  dataModule.io.func := io.in.bits.ctrl.fuOpType
  dataModule.io.fixedTaken := io.in.bits.ctrl.predictInfo.get.fixedTaken

  val pcExtend = Mux(io.instrAddrTransType.get.shouldBeSext,
    SignExt(io.in.bits.data.pc.get, VAddrBits + 1),
    ZeroExt(io.in.bits.data.pc.get, VAddrBits + 1)
  )
  addModule.io.pcExtend := pcExtend
  addModule.io.imm := io.in.bits.data.imm // imm
  addModule.io.taken := dataModule.io.taken
  addModule.io.isRVC := io.in.bits.ctrl.preDecode.get.isRVC
  addModule.io.nextPcOffset := io.in.bits.data.nextPcOffset.get

  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  val brhPredictTarget = io.in.bits.ctrl.predictInfo.get.target
  val brhRealTarget = addModule.io.target
  val targetWrong = dataModule.io.fixedTaken && dataModule.io.taken && (brhRealTarget =/= brhPredictTarget)
  val isMisPred = dataModule.io.mispredict || targetWrong
  io.out.bits.res.data := 0.U
  io.out.bits.res.redirect.get match {
    case redirect =>
      redirect.valid := io.out.valid
      redirect.bits := 0.U.asTypeOf(io.out.bits.res.redirect.get.bits)
      redirect.bits.level := RedirectLevel.flushAfter
      redirect.bits.robIdx := io.in.bits.ctrl.robIdx
      redirect.bits.ftqIdx := io.in.bits.ctrl.ftqIdx.get
      redirect.bits.ftqOffset := io.in.bits.ctrl.ftqOffset.get
      redirect.bits.fullTarget := addModule.io.target
      redirect.bits.isMisPred := isMisPred
      redirect.bits.taken := dataModule.io.taken
      redirect.bits.target := addModule.io.target
      redirect.bits.pc := io.in.bits.data.pc.get
      redirect.bits.backendIAF := io.instrAddrTransType.get.checkAccessFault(addModule.io.target)
      redirect.bits.backendIPF := io.instrAddrTransType.get.checkPageFault(addModule.io.target)
      redirect.bits.backendIGPF := io.instrAddrTransType.get.checkGuestPageFault(addModule.io.target)
      redirect.bits.attribute := io.toFrontendBJUResolve.get.bits.attribute
  }
  io.toFrontendBJUResolve.get.valid := io.out.valid
  io.toFrontendBJUResolve.get.bits.ftqIdx := io.in.bits.ctrl.ftqIdx.get
  io.toFrontendBJUResolve.get.bits.ftqOffset := io.in.bits.ctrl.ftqOffset.get
  io.toFrontendBJUResolve.get.bits.pc := PrunedAddrInit(pcExtend)
  io.toFrontendBJUResolve.get.bits.target := PrunedAddrInit(addModule.io.target)
  io.toFrontendBJUResolve.get.bits.taken := dataModule.io.taken
  io.toFrontendBJUResolve.get.bits.mispredict := isMisPred
  io.toFrontendBJUResolve.get.bits.attribute.branchType := io.in.bits.ctrl.preDecode.get.brType
  io.toFrontendBJUResolve.get.bits.attribute.rasAction := Mux1H(
    Seq(io.in.bits.ctrl.preDecode.get.isCall, io.in.bits.ctrl.preDecode.get.isRet),
    Seq(BranchAttribute.RasAction.Push, BranchAttribute.RasAction.Pop)
  )
  connect0LatencyCtrlSingal
}
