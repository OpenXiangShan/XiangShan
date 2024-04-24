package xiangshan.backend.fu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.DataHoldBypass
import utils.OptionWrapper
import xiangshan._
import xiangshan.backend.Bundles.VPUCtrlSignals
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend.{FtqPtr, PreDecodeInfo}
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.fu.vector.Bundles.Vxsat
import xiangshan.ExceptionNO.illegalInstr
import xiangshan.backend.fu.vector.Bundles.VType

class FuncUnitCtrlInput(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val fuOpType    = FuOpType()
  val robIdx      = new RobPtr
  val pdest       = UInt(PhyRegIdxWidth.W)
  val rfWen       = OptionWrapper(cfg.needIntWen, Bool())
  val fpWen       = OptionWrapper(cfg.needFpWen,  Bool())
  val vecWen      = OptionWrapper(cfg.needVecWen, Bool())
  val flushPipe   = OptionWrapper(cfg.flushPipe,  Bool())
  val preDecode   = OptionWrapper(cfg.hasPredecode, new PreDecodeInfo)
  val ftqIdx      = OptionWrapper(cfg.needPc || cfg.replayInst || cfg.isSta, new FtqPtr)
  val ftqOffset   = OptionWrapper(cfg.needPc || cfg.replayInst || cfg.isSta, UInt(log2Up(PredictWidth).W))
  val predictInfo = OptionWrapper(cfg.hasRedirect, new Bundle {
    val target    = UInt(VAddrData().dataWidth.W)
    val taken     = Bool()
  })
  val fpu         = OptionWrapper(cfg.writeFflags, new FPUCtrlSignals)
  val vpu         = OptionWrapper(cfg.needVecCtrl, new VPUCtrlSignals)
}

class FuncUnitCtrlOutput(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val robIdx        = new RobPtr
  val pdest         = UInt(PhyRegIdxWidth.W) // Todo: use maximum of pregIdxWidth of different pregs
  val rfWen         = OptionWrapper(cfg.needIntWen, Bool())
  val fpWen         = OptionWrapper(cfg.needFpWen,  Bool())
  val vecWen        = OptionWrapper(cfg.needVecWen, Bool())
  val exceptionVec  = OptionWrapper(cfg.exceptionOut.nonEmpty, ExceptionVec())
  val flushPipe     = OptionWrapper(cfg.flushPipe,  Bool())
  val replay        = OptionWrapper(cfg.replayInst, Bool())
  val preDecode     = OptionWrapper(cfg.hasPredecode, new PreDecodeInfo)
  val fpu           = OptionWrapper(cfg.writeFflags, new FPUCtrlSignals)
  val vpu           = OptionWrapper(cfg.needVecCtrl, new VPUCtrlSignals)
}

class FuncUnitDataInput(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val src       = MixedVec(cfg.genSrcDataVec)
  val imm       = UInt(cfg.dataBits.W)
  val pc        = OptionWrapper(cfg.needPc, UInt(VAddrData().dataWidth.W))

  def getSrcVConfig : UInt = src(cfg.vconfigIdx)
  def getSrcMask    : UInt = src(cfg.maskSrcIdx)
}

class FuncUnitDataOutput(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val data      = UInt(cfg.dataBits.W)
  val fflags    = OptionWrapper(cfg.writeFflags, UInt(5.W))
  val vxsat     = OptionWrapper(cfg.writeVxsat, Vxsat())
  val pc        = OptionWrapper(cfg.isFence, UInt(VAddrData().dataWidth.W))
  val redirect  = OptionWrapper(cfg.hasRedirect, ValidIO(new Redirect))
}

class FuncUnitInput(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val ctrl = new FuncUnitCtrlInput(cfg)
  val data = new FuncUnitDataInput(cfg)
  val perfDebugInfo = new PerfDebugInfo()
}

class FuncUnitOutput(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val ctrl = new FuncUnitCtrlOutput(cfg)
  val res = new FuncUnitDataOutput(cfg)
  val perfDebugInfo = new PerfDebugInfo()
}

class FuncUnitIO(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect))
  val in = Flipped(DecoupledIO(new FuncUnitInput(cfg)))
  val out = DecoupledIO(new FuncUnitOutput(cfg))
  val csrio = OptionWrapper(cfg.isCsr, new CSRFileIO)
  val fenceio = OptionWrapper(cfg.isFence, new FenceIO)
  val frm = OptionWrapper(cfg.needSrcFrm, Input(UInt(3.W)))
  val vxrm = OptionWrapper(cfg.needSrcVxrm, Input(UInt(2.W)))
  val vtype = OptionWrapper(cfg.writeVType, new VType)
}

abstract class FuncUnit(val cfg: FuConfig)(implicit p: Parameters) extends XSModule {
  val io = IO(new FuncUnitIO(cfg))

  // should only be used in non-piped fu
  def connectNonPipedCtrlSingal: Unit = {
    io.out.bits.ctrl.robIdx := RegEnable(io.in.bits.ctrl.robIdx, io.in.fire)
    io.out.bits.ctrl.pdest  := RegEnable(io.in.bits.ctrl.pdest, io.in.fire)
    io.out.bits.ctrl.rfWen  .foreach(_ := RegEnable(io.in.bits.ctrl.rfWen.get, io.in.fire))
    io.out.bits.ctrl.fpWen  .foreach(_ := RegEnable(io.in.bits.ctrl.fpWen.get, io.in.fire))
    io.out.bits.ctrl.vecWen .foreach(_ := RegEnable(io.in.bits.ctrl.vecWen.get, io.in.fire))
    // io.out.bits.ctrl.flushPipe should be connected in fu
    io.out.bits.ctrl.preDecode.foreach(_ := RegEnable(io.in.bits.ctrl.preDecode.get, io.in.fire))
    io.out.bits.ctrl.fpu      .foreach(_ := RegEnable(io.in.bits.ctrl.fpu.get, io.in.fire))
    io.out.bits.ctrl.vpu      .foreach(_ := RegEnable(io.in.bits.ctrl.vpu.get, io.in.fire))
    io.out.bits.perfDebugInfo := RegEnable(io.in.bits.perfDebugInfo, io.in.fire)
  }

  def connect0LatencyCtrlSingal: Unit = {
    io.out.bits.ctrl.robIdx := io.in.bits.ctrl.robIdx
    io.out.bits.ctrl.pdest := io.in.bits.ctrl.pdest
    io.out.bits.ctrl.rfWen.foreach(_ := io.in.bits.ctrl.rfWen.get)
    io.out.bits.ctrl.fpWen.foreach(_ := io.in.bits.ctrl.fpWen.get)
    io.out.bits.ctrl.vecWen.foreach(_ := io.in.bits.ctrl.vecWen.get)
    // io.out.bits.ctrl.flushPipe should be connected in fu
    io.out.bits.ctrl.preDecode.foreach(_ := io.in.bits.ctrl.preDecode.get)
    io.out.bits.ctrl.fpu.foreach(_ := io.in.bits.ctrl.fpu.get)
    io.out.bits.ctrl.vpu.foreach(_ := io.in.bits.ctrl.vpu.get)
    io.out.bits.perfDebugInfo := io.in.bits.perfDebugInfo
  }
}

/**
  * @author LinJiaWei, Yinan Xu
  */
trait HasPipelineReg { this: FuncUnit =>
  def latency: Int 

  val latdiff :Int = cfg.latency.extraLatencyVal.getOrElse(0)
  val preLat :Int = latency - latdiff
  require(latency >= 0 && latdiff >=0)

  def pipelineReg(init: FuncUnitInput , valid:Bool, ready: Bool,latency: Int, flush:ValidIO[Redirect]): (Seq[FuncUnitInput],Seq[Bool],Seq[Bool])={
    val rdyVec = Seq.fill(latency)(Wire(Bool())) :+ ready
    val validVec = valid +: Seq.fill(latency)(RegInit(false.B))
    val ctrlVec = init.ctrl +: Seq.fill(latency)(Reg(chiselTypeOf(io.in.bits.ctrl)))
    val dataVec = init.data +: Seq.fill(latency)(Reg(chiselTypeOf(io.in.bits.data)))
    val perfVec = init.perfDebugInfo +: Seq.fill(latency)(Reg(chiselTypeOf(io.in.bits.perfDebugInfo)))



    val robIdxVec = ctrlVec.map(_.robIdx)

    // if flush(0), valid 0 will not given, so set flushVec(0) to false.B
    val flushVec = validVec.zip(robIdxVec).map(x => x._1 && x._2.needFlush(flush))

    for (i <- 0 until latency) {
      rdyVec(i) := !validVec(i + 1) || rdyVec(i + 1).asTypeOf(Bool())
    }
    for (i <- 1 to latency) {
      when(rdyVec(i - 1) && validVec(i - 1) && !flushVec(i - 1)) {
        validVec(i) := validVec(i - 1)
        ctrlVec(i) := ctrlVec(i - 1)
        dataVec(i) := dataVec(i - 1)
        perfVec(i) := perfVec(i - 1)
      }.elsewhen(flushVec(i) || rdyVec(i)) {
        validVec(i) := false.B
      }
    }

    (ctrlVec.zip(dataVec).zip(perfVec).map{
      case(( ctrl,data), perf) => {
        val out = Wire(new FuncUnitInput(cfg))
        out.ctrl := ctrl
        out.data := data
        out.perfDebugInfo := perf
        out
      }
    },validVec, rdyVec)
  }
  val (pipeReg : Seq[FuncUnitInput],validVec ,rdyVec ) = pipelineReg(io.in.bits, io.in.valid,io.out.ready,preLat, io.flush)
  val ctrlVec = pipeReg.map(_.ctrl)
  val dataVec = pipeReg.map(_.data)
  val perfVec = pipeReg.map(_.perfDebugInfo)
  val robIdxVec = ctrlVec.map(_.robIdx)
  val pipeflushVec = validVec.zip(robIdxVec).map(x => x._1 && x._2.needFlush(io.flush))


  val fixtiminginit = Wire(new FuncUnitInput(cfg))
  fixtiminginit.ctrl := ctrlVec.last
  fixtiminginit.data := dataVec.last
  fixtiminginit.perfDebugInfo := perfVec.last

  // fixtiming pipelinereg
  val (fixpipeReg : Seq[FuncUnitInput], fixValidVec, fixRdyVec) = pipelineReg(fixtiminginit, validVec.last,rdyVec.head ,latdiff, io.flush)
  val fixCtrlVec = fixpipeReg.map(_.ctrl)
  val fixDataVec = fixpipeReg.map(_.data)
  val fixPerfVec = fixpipeReg.map(_.perfDebugInfo)
  val fixrobIdxVec = ctrlVec.map(_.robIdx)
  val fixflushVec = fixValidVec.zip(fixrobIdxVec).map(x => x._1 && x._2.needFlush(io.flush))
  val flushVec = pipeflushVec ++ fixflushVec
  val pcVec = fixDataVec.map(_.pc)

  io.in.ready := fixRdyVec.head
  io.out.valid := fixValidVec.last
  io.out.bits.res.pc.zip(pcVec.last).foreach { case (l, r) => l := r }

  io.out.bits.ctrl.robIdx := fixCtrlVec.last.robIdx
  io.out.bits.ctrl.pdest := fixCtrlVec.last.pdest
  io.out.bits.ctrl.rfWen.foreach(_ := fixCtrlVec.last.rfWen.get)
  io.out.bits.ctrl.fpWen.foreach(_ := fixCtrlVec.last.fpWen.get)
  io.out.bits.ctrl.vecWen.foreach(_ := fixCtrlVec.last.vecWen.get)
  io.out.bits.ctrl.fpu.foreach(_ := fixCtrlVec.last.fpu.get)
  io.out.bits.ctrl.vpu.foreach(_ := fixCtrlVec.last.vpu.get)
  io.out.bits.perfDebugInfo := fixPerfVec.last

  // vstart illegal
  if (cfg.exceptionOut.nonEmpty) {
    val outVstart = fixCtrlVec.last.vpu.get.vstart
    val vstartIllegal = outVstart =/= 0.U
    io.out.bits.ctrl.exceptionVec.get := 0.U.asTypeOf(io.out.bits.ctrl.exceptionVec.get)
    io.out.bits.ctrl.exceptionVec.get(illegalInstr) := vstartIllegal
  }

  def regEnable(i: Int): Bool = validVec(i - 1) && rdyVec(i - 1) && !flushVec(i - 1)

  def PipelineReg[TT <: Data](i: Int)(next: TT) = {
    val lat = preLat min i
    RegEnable(
      next,
      regEnable(lat)
    )
  }

  def SNReg[TT <: Data](in: TT, n: Int): TT ={
    val lat = preLat min n
    var next = in
    for (i <- 1 to lat) {
      next = PipelineReg[TT](i)(next)
    }
    next
  }

  def S1Reg[TT <: Data](next: TT): TT = PipelineReg[TT](1)(next)

  def S2Reg[TT <: Data](next: TT): TT = PipelineReg[TT](2)(next)

  def S3Reg[TT <: Data](next: TT): TT = PipelineReg[TT](3)(next)

  def S4Reg[TT <: Data](next: TT): TT = PipelineReg[TT](4)(next)

  def S5Reg[TT <: Data](next: TT): TT = PipelineReg[TT](5)(next)

}

abstract class PipedFuncUnit(override val cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg)
  with HasPipelineReg {
  override def latency: Int = cfg.latency.latencyVal.get
}
