package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utility.DataHoldBypass
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend.{FtqPtr, PreDecodeInfo}
import xiangshan.backend.datapath.DataConfig._

class FuncUnitInput(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val fuOpType  = FuOpType()
  val src       = MixedVec(cfg.genSrcDataVec)
  val imm       = UInt(cfg.dataBits.W)
  val robIdx    = new RobPtr
  val pdest     = UInt(PhyRegIdxWidth.W)
  val rfWen     = if (cfg.writeIntRf)   Some(Bool())                        else None
  val fpWen     = if (cfg.writeFpRf)    Some(Bool())                        else None
  val vecWen    = if (cfg.writeVecRf)   Some(Bool())                        else None
  val fpu       = if (cfg.needFPUCtrl)  Some(new FPUCtrlSignals)            else None

  val flushPipe = if (cfg.flushPipe)    Some(Bool())                        else None
  val pc        = if (cfg.needPc)       Some(UInt(VAddrData().dataWidth.W)) else None
  val preDecode = if (cfg.hasPredecode) Some(new PreDecodeInfo)             else None
  val ftqIdx    = if (cfg.needPc || cfg.replayInst)
                                        Some(new FtqPtr)                    else None
  val ftqOffset = if (cfg.needPc || cfg.replayInst)
                                        Some(UInt(log2Up(PredictWidth).W))  else None
  val predictInfo = if (cfg.hasRedirect)Some(new Bundle {
    val target = UInt(VAddrData().dataWidth.W)
    val taken = Bool()
  }) else None
}

class FuncUnitOutput(cfg: FuConfig)(implicit p: Parameters) extends XSBundle {
  val data         = UInt(cfg.dataBits.W)
  val pdest        = UInt(PhyRegIdxWidth.W) // Todo: use maximum of pregIdxWidth of different pregs
  val robIdx       = new RobPtr
  val redirect     = if (cfg.hasRedirect) Some(ValidIO(new Redirect))        else None
  val fflags       = if (cfg.writeFflags) Some(UInt(5.W))                    else None
  val exceptionVec = if (cfg.exceptionOut.nonEmpty) Some(ExceptionVec())     else None
  val flushPipe    = if (cfg.flushPipe)   Some(Bool())                       else None
  val replay       = if (cfg.replayInst)  Some(Bool())                       else None
  val pc           = if (cfg.isFence)     Some(UInt(VAddrData().dataWidth.W))else None
  val fpu          = if (cfg.needFPUCtrl) Some(new FPUCtrlSignals)           else None // only used in FMA
  val preDecode    = if (cfg.hasPredecode)Some(new PreDecodeInfo)            else None
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
    io.out.bits.robIdx := DataHoldBypass(io.in.bits.robIdx, io.in.fire)
    io.out.bits.pdest := DataHoldBypass(io.in.bits.pdest, io.in.fire)
    io.out.bits.pc.foreach(_ := io.in.bits.pc.get)
    io.out.bits.preDecode.foreach(_ := io.in.bits.preDecode.get)
    io.out.bits.fpu.foreach(_ := DataHoldBypass(io.in.bits.fpu.get, io.in.fire))
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
  val robIdxVec = io.in.bits.robIdx +: Array.fill(latency)(Reg(chiselTypeOf(io.in.bits.robIdx)))

  // if flush(0), valid 0 will not given, so set flushVec(0) to false.B
  val flushVec = validVec.zip(robIdxVec).map(x => x._1 && x._2.needFlush(io.flush))

  for (i <- 0 until latency) {
    rdyVec(i) := !validVec(i + 1) || rdyVec(i + 1)
  }

  for (i <- 1 to latency) {
    when(rdyVec(i - 1) && validVec(i - 1) && !flushVec(i - 1)){
      validVec(i) := validVec(i - 1)
      robIdxVec(i) := robIdxVec(i - 1)
    }.elsewhen(flushVec(i) || rdyVec(i)){
      validVec(i) := false.B
    }
  }

  io.in.ready := rdyVec(0)
  io.out.valid := validVec.last
  io.out.bits.robIdx := robIdxVec.last

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


