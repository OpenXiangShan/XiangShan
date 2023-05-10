package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utility.DataHoldBypass
import utils.OptionWrapper
import xiangshan._
import xiangshan.backend.Bundles.VPUCtrlSignals
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend.{FtqPtr, PreDecodeInfo}
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.fu.fpu.Bundles.Fflags
import xiangshan.backend.fu.vector.Bundles.Vxsat

class FuncUnitCtrlInput(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val fuOpType    = FuOpType()
  val robIdx      = new RobPtr
  val pdest       = UInt(PhyRegIdxWidth.W)
  val rfWen       = OptionWrapper(cfg.writeIntRf, Bool())
  val fpWen       = OptionWrapper(cfg.writeFpRf,  Bool())
  val vecWen      = OptionWrapper(cfg.writeVecRf, Bool())
  val flushPipe   = OptionWrapper(cfg.flushPipe,  Bool())
  val preDecode   = OptionWrapper(cfg.hasPredecode, new PreDecodeInfo)
  val ftqIdx      = OptionWrapper(cfg.needPc || cfg.replayInst, new FtqPtr)
  val ftqOffset   = OptionWrapper(cfg.needPc || cfg.replayInst, UInt(log2Up(PredictWidth).W))
  val predictInfo = OptionWrapper(cfg.hasRedirect, new Bundle {
    val target    = UInt(VAddrData().dataWidth.W)
    val taken     = Bool()
  })
  val fpu         = OptionWrapper(cfg.needFPUCtrl, new FPUCtrlSignals)
  val vpu         = OptionWrapper(cfg.needVecCtrl, new VPUCtrlSignals)
}

class FuncUnitCtrlOutput(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val robIdx        = new RobPtr
  val pdest         = UInt(PhyRegIdxWidth.W) // Todo: use maximum of pregIdxWidth of different pregs
  val rfWen         = OptionWrapper(cfg.writeIntRf, Bool())
  val fpWen         = OptionWrapper(cfg.writeFpRf,  Bool())
  val vecWen        = OptionWrapper(cfg.writeVecRf, Bool())
  val exceptionVec  = OptionWrapper(cfg.exceptionOut.nonEmpty, ExceptionVec())
  val flushPipe     = OptionWrapper(cfg.flushPipe,  Bool())
  val replay        = OptionWrapper(cfg.replayInst, Bool())
  val preDecode     = OptionWrapper(cfg.hasPredecode, new PreDecodeInfo)
  val fpu           = OptionWrapper(cfg.needFPUCtrl, new FPUCtrlSignals) // only used in FMA
  val vpu           = OptionWrapper(cfg.needVecCtrl, new VPUCtrlSignals)
}

class FuncUnitDataInput(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val src       = MixedVec(cfg.genSrcDataVec)
  val imm       = UInt(cfg.dataBits.W)
  val pc        = OptionWrapper(cfg.needPc, UInt(VAddrData().dataWidth.W))
}

class FuncUnitDataOutput(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val data      = UInt(cfg.dataBits.W)
  val fflags    = OptionWrapper(cfg.writeFflags, Fflags())
  val vxsat     = OptionWrapper(cfg.writeVxsat, Vxsat())
  val pc        = OptionWrapper(cfg.isFence, UInt(VAddrData().dataWidth.W))
  val redirect  = OptionWrapper(cfg.hasRedirect, ValidIO(new Redirect))
}

class FuncUnitInput(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val ctrl = new FuncUnitCtrlInput(cfg)
  val data = new FuncUnitDataInput(cfg)
}

class FuncUnitOutput(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val ctrl = new FuncUnitCtrlOutput(cfg)
  val res = new FuncUnitDataOutput(cfg)
}

class FuncUnitIO(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect))
  val in = Flipped(DecoupledIO(new FuncUnitInput(cfg)))
  val out = DecoupledIO(new FuncUnitOutput(cfg))
  val csrio = if (cfg.isCsr) Some(new CSRFileIO) else None
  val fenceio = if (cfg.isFence) Some(new FenceIO) else None
  val frm = if (cfg.needSrcFrm) Some(Input(UInt(3.W))) else None
}

abstract class FuncUnit(val cfg: FuConfig)(implicit p: Parameters) extends XSModule {
  val io = IO(new FuncUnitIO(cfg))

  // should only be used in non-piped fu
  def connectNonPipedCtrlSingal: Unit = {
    io.out.bits.ctrl.robIdx   := DataHoldBypass(io.in.bits.ctrl.robIdx, io.in.fire)
    io.out.bits.ctrl.pdest    := DataHoldBypass(io.in.bits.ctrl.pdest, io.in.fire)
    io.out.bits.ctrl.preDecode.foreach(_ := DataHoldBypass(io.in.bits.ctrl.preDecode.get, io.in.fire))
    io.out.bits.ctrl.fpu      .foreach(_ := DataHoldBypass(io.in.bits.ctrl.fpu.get,    io.in.fire))
  }
}

/**
  * @author LinJiaWei, Yinan Xu
  */
trait HasPipelineReg { this: FuncUnit =>
  def latency: Int

  require(latency > 0)

  val validVec = io.in.valid +: Seq.fill(latency)(RegInit(false.B))
  val rdyVec = Seq.fill(latency)(Wire(Bool())) :+ io.out.ready
//  val ctrlVec = io.in.bits.ctrl +: Seq.fill(latency)(Reg(chiselTypeOf(io.in.bits.ctrl)))
//  val dataVec = io.in.bits.data +: Seq.fill(latency)(Reg(chiselTypeOf(io.in.bits.data)))

  val robIdxVec = io.in.bits.ctrl.robIdx +: Array.fill(latency)(Reg(chiselTypeOf(io.in.bits.ctrl.robIdx)))
  val pdestVec = io.in.bits.ctrl.pdest +: Array.fill(latency)(Reg(chiselTypeOf(io.in.bits.ctrl.pdest)))

  val pcVec = io.in.bits.data.pc.map(x => x) +: Array.fill(latency)( io.in.bits.data.pc.map(x => Reg(chiselTypeOf(x)))) // Reg(chiselTypeOf(io.in.bits.pc.get))
  val preDecodeVec = io.in.bits.ctrl.preDecode.map(x => x) +: Array.fill(latency)(io.in.bits.ctrl.preDecode.map(x => Reg(chiselTypeOf(x))))
  val fpuVec = io.in.bits.ctrl.fpu.map(x => x) +: Array.fill(latency)(io.in.bits.ctrl.fpu.map(x => Reg(chiselTypeOf(x))))

  // if flush(0), valid 0 will not given, so set flushVec(0) to false.B
  val flushVec = validVec.zip(robIdxVec).map(x => x._1 && x._2.needFlush(io.flush))

  for (i <- 0 until latency) {
    rdyVec(i) := !validVec(i + 1) || rdyVec(i + 1)
  }

  for (i <- 1 until latency) {
    when(rdyVec(i - 1) && validVec(i - 1) && !flushVec(i - 1)){
      validVec(i) := validVec(i - 1)
      robIdxVec(i) := robIdxVec(i - 1)
      pdestVec(i) := pdestVec(i - 1)
      pcVec(i).zip(pcVec(i - 1)).foreach{case (l,r) => l := r}
      preDecodeVec(i).zip(preDecodeVec(i - 1)).foreach{case (l,r) => l := r}
      fpuVec(i).zip(fpuVec(i - 1)).foreach{case (l,r) => l := r}
    }.elsewhen(flushVec(i) || rdyVec(i)){
      validVec(i) := false.B
    }
  }

  io.in.ready := rdyVec(0)
  io.out.valid := validVec.last
  io.out.bits.ctrl.robIdx := robIdxVec.last
  io.out.bits.ctrl.pdest := pdestVec.last
  io.out.bits.res.pc.zip(pcVec.last).foreach{case (l,r) => l := r}
  io.out.bits.ctrl.preDecode.zip(preDecodeVec.last).foreach{case (l,r) => l := r}
  io.out.bits.ctrl.fpu.zip(fpuVec.last).foreach{case (l,r) => l := r}

  def regEnable(i: Int): Bool = validVec(i - 1) && rdyVec(i - 1) && !flushVec(i - 1)

  def PipelineReg[TT <: Data](i: Int)(next: TT) = RegEnable(
    next,
    regEnable(i)
  )

  def S1Reg[TT <: Data](next: TT): TT = PipelineReg[TT](1)(next)

  def S2Reg[TT <: Data](next: TT): TT = PipelineReg[TT](2)(next)

  def S3Reg[TT <: Data](next: TT): TT = PipelineReg[TT](3)(next)

  def S4Reg[TT <: Data](next: TT): TT = PipelineReg[TT](4)(next)

  def S5Reg[TT <: Data](next: TT): TT = PipelineReg[TT](5)(next)

}


