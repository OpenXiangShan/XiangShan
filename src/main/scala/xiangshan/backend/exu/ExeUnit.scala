package xiangshan.backend.exu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.DelayN
import utils._
import xiangshan.backend.fu.{CSRFileIO, FenceIO, FuncUnitInput}
import xiangshan.backend.Bundles.{ExuInput, ExuOutput, MemExuInput, MemExuOutput}
import xiangshan.{Redirect, XSBundle, XSModule}

class ExeUnitIO(params: ExeUnitParams)(implicit p: Parameters) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect()))
  val in = Flipped(DecoupledIO(new ExuInput(params)))
  val out = DecoupledIO(new ExuOutput(params))
  val csrio = if (params.hasCSR) Some(new CSRFileIO) else None
  val fenceio = if (params.hasFence) Some(new FenceIO) else None
  val frm = if (params.needSrcFrm) Some(Input(UInt(3.W))) else None
}

class ExeUnit(exuParams: ExeUnitParams)(implicit p: Parameters) extends LazyModule {
  lazy val module = new ExeUnitImp(this)(p, exuParams)
}

class ExeUnitImp(
  override val wrapper: ExeUnit
)(implicit
  p: Parameters, exuParams: ExeUnitParams
) extends LazyModuleImp(wrapper) {
  private val fuCfgs = exuParams.fuConfigs

  val io = IO(new ExeUnitIO(exuParams))

  val funcUnits = fuCfgs.map(cfg => {
    assert(cfg.fuGen != null, cfg.name + "Cfg'fuGen is null !!!")
    val module = cfg.fuGen(p, cfg)
    module
  })

  val busy = RegInit(false.B)
  val robIdx = RegEnable(io.in.bits.robIdx, io.in.fire)
  when (io.in.fire && io.in.bits.robIdx.needFlush(io.flush)) {
    busy := false.B
  }.elsewhen(busy && robIdx.needFlush(io.flush)){
    busy := false.B
  }.elsewhen(io.out.fire) {
    busy := false.B
  }.elsewhen(io.in.fire) {
    busy := true.B
  }
  if(exuParams.latencyValMax.nonEmpty){
    busy := false.B
  }

  // rob flush --> funcUnits
  funcUnits.zipWithIndex.foreach { case (fu, i) =>
    fu.io.flush <> io.flush
  }

  def acceptCond(input: ExuInput): Seq[Bool] = {
    input.params.fuConfigs.map(_.fuSel(input))
  }

  val in1ToN = Module(new Dispatcher(new ExuInput(exuParams), funcUnits.length, acceptCond))

  // ExeUnit.in <---> Dispatcher.in
  in1ToN.io.in.valid := io.in.fire()
  in1ToN.io.in.bits := io.in.bits
  io.in.ready := !busy

  // Dispatcher.out <---> FunctionUnits
  in1ToN.io.out.zip(funcUnits.map(_.io.in)).foreach {
    case (source: DecoupledIO[ExuInput], sink: DecoupledIO[FuncUnitInput]) =>
      sink.valid := source.valid
      source.ready := sink.ready

      sink.bits.data.src.zip(source.bits.src).foreach { case(fuSrc, exuSrc) => fuSrc := exuSrc }
      sink.bits.data.pc          .foreach(x => x := source.bits.pc.get)
      sink.bits.data.imm         := source.bits.imm
      sink.bits.ctrl.fuOpType    := source.bits.fuOpType
      sink.bits.ctrl.robIdx      := source.bits.robIdx
      sink.bits.ctrl.pdest       := source.bits.pdest
      sink.bits.ctrl.rfWen       .foreach(x => x := source.bits.rfWen.get)
      sink.bits.ctrl.fpWen       .foreach(x => x := source.bits.fpWen.get)
      sink.bits.ctrl.vecWen      .foreach(x => x := source.bits.vecWen.get)
      sink.bits.ctrl.flushPipe   .foreach(x => x := source.bits.flushPipe.get)
      sink.bits.ctrl.preDecode   .foreach(x => x := source.bits.preDecode.get)
      sink.bits.ctrl.ftqIdx      .foreach(x => x := source.bits.ftqIdx.get)
      sink.bits.ctrl.ftqOffset   .foreach(x => x := source.bits.ftqOffset.get)
      sink.bits.ctrl.predictInfo .foreach(x => x := source.bits.predictInfo.get)
      sink.bits.ctrl.fpu         .foreach(x => x := source.bits.fpu.get)
      sink.bits.ctrl.vpu         .foreach(x => x := source.bits.vpu.get)
  }

  private val fuOutValidOH = funcUnits.map(_.io.out.valid)
  XSError(PopCount(fuOutValidOH) > 1.U, p"fuOutValidOH ${Binary(VecInit(fuOutValidOH).asUInt)} should be one-hot)\n")
  private val fuOutBitsVec = funcUnits.map(_.io.out.bits)
  private val fuRedirectVec: Seq[Option[ValidIO[Redirect]]] = funcUnits.map(_.io.out.bits.res.redirect)

  // Assume that one fu can only write int or fp or vec,
  // otherwise, wenVec should be assigned to wen in fu.
  private val fuIntWenVec = funcUnits.map(x => x.cfg.writeIntRf.B && x.io.out.bits.ctrl.rfWen.getOrElse(false.B))
  private val fuFpWenVec  = funcUnits.map(x => x.cfg.writeFpRf.B  && x.io.out.bits.ctrl.fpWen.getOrElse(false.B))
  private val fuVecWenVec = funcUnits.map(x => x.cfg.writeVecRf.B && x.io.out.bits.ctrl.vecWen.getOrElse(false.B))
  // FunctionUnits <---> ExeUnit.out
  io.out.valid := Cat(fuOutValidOH).orR
  funcUnits.foreach(fu => fu.io.out.ready := io.out.ready)

  // select one fu's result
  io.out.bits.data := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.res.data))
  io.out.bits.robIdx := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.robIdx))
  io.out.bits.pdest := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.pdest))
  io.out.bits.intWen.foreach(x => x := Mux1H(fuOutValidOH, fuIntWenVec))
  io.out.bits.fpWen.foreach(x => x := Mux1H(fuOutValidOH, fuFpWenVec))
  io.out.bits.vecWen.foreach(x => x := Mux1H(fuOutValidOH, fuVecWenVec))
  io.out.bits.redirect.foreach(x => x := Mux1H((fuOutValidOH zip fuRedirectVec).filter(_._2.isDefined).map(x => (x._1, x._2.get))))
  io.out.bits.fflags.foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.res.fflags.getOrElse(0.U.asTypeOf(io.out.bits.fflags.get)))))
  io.out.bits.exceptionVec.foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.exceptionVec.getOrElse(0.U.asTypeOf(io.out.bits.exceptionVec.get)))))
  io.out.bits.flushPipe.foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.flushPipe.getOrElse(0.U.asTypeOf(io.out.bits.flushPipe.get)))))
  io.out.bits.replay.foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.replay.getOrElse(0.U.asTypeOf(io.out.bits.replay.get)))))
  io.out.bits.predecodeInfo.foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.preDecode.getOrElse(0.U.asTypeOf(io.out.bits.predecodeInfo.get)))))

  io.csrio.foreach(exuio => funcUnits.foreach(fu => fu.io.csrio.foreach{
    fuio =>
      exuio <> fuio
      fuio.exception := DelayN(exuio.exception, 2)
  }))
  io.fenceio.foreach(exuio => funcUnits.foreach(fu => fu.io.fenceio.foreach(fuio => fuio <> exuio)))
  io.frm.foreach(exuio => funcUnits.foreach(fu => fu.io.frm.foreach(fuio => fuio <> exuio)))

  // debug info
  io.out.bits.debug     := 0.U.asTypeOf(io.out.bits.debug)
  io.out.bits.debugInfo := 0.U.asTypeOf(io.out.bits.debugInfo)
}

class DispatcherIO[T <: Data](private val gen: T, n: Int) extends Bundle {
  val in = Flipped(DecoupledIO(gen))

  val out = Vec(n, DecoupledIO(gen))
}

class Dispatcher[T <: Data](private val gen: T, n: Int, acceptCond: T => Seq[Bool])
  (implicit p: Parameters)
  extends Module {

  val io = IO(new DispatcherIO(gen, n))

  private val acceptVec: Vec[Bool] = VecInit(acceptCond(io.in.bits))

  XSError(io.in.valid && PopCount(acceptVec) > 1.U, s"s[ExeUnit] accept vec should no more than 1, ${Binary(acceptVec.asUInt)} ")
  XSError(io.in.valid && PopCount(acceptVec) === 0.U, "[ExeUnit] there is a inst not dispatched to any fu")

  io.out.zipWithIndex.foreach { case (out, i) =>
    out.valid := acceptVec(i) && io.in.valid && out.ready
    out.bits := io.in.bits
  }

  io.in.ready := Cat(io.out.map(_.ready)).orR
}

class MemExeUnitIO (implicit p: Parameters) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect()))
  val in = Flipped(DecoupledIO(new MemExuInput()))
  val out = DecoupledIO(new MemExuOutput())
}

class MemExeUnit(exuParams: ExeUnitParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new MemExeUnitIO)
  val fu = exuParams.fuConfigs.head.fuGen(p, exuParams.fuConfigs.head)
  fu.io.flush             := io.flush
  fu.io.in.valid          := io.in.valid
  io.in.ready             := fu.io.in.ready

  fu.io.in.bits.ctrl.robIdx    := io.in.bits.uop.robIdx
  fu.io.in.bits.ctrl.pdest     := io.in.bits.uop.pdest
  fu.io.in.bits.ctrl.fuOpType  := io.in.bits.uop.fuOpType
  fu.io.in.bits.data.imm       := io.in.bits.uop.imm
  fu.io.in.bits.data.src.zip(io.in.bits.src).foreach(x => x._1 := x._2)

  io.out.valid            := fu.io.out.valid
  fu.io.out.ready         := io.out.ready

  io.out.bits             := 0.U.asTypeOf(io.out.bits) // dontCare other fields
  io.out.bits.data        := fu.io.out.bits.res.data
  io.out.bits.uop.robIdx  := fu.io.out.bits.ctrl.robIdx
  io.out.bits.uop.pdest   := fu.io.out.bits.ctrl.pdest
  io.out.bits.uop.fuType  := io.in.bits.uop.fuType
  io.out.bits.uop.fuOpType:= io.in.bits.uop.fuOpType
  io.out.bits.uop.sqIdx   := io.in.bits.uop.sqIdx

  io.out.bits.debug       := 0.U.asTypeOf(io.out.bits.debug)
}
