/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.exu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.hierarchy.{Definition, instantiable}
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import xiangshan.backend.fu.{CSRFileIO, FenceIO, FuncUnitInput}
import xiangshan.backend.Bundles.{ExuInput, ExuOutput, MemExuInput, MemExuOutput}
import xiangshan.{AddrTransType, FPUCtrlSignals, HasXSParameter, Redirect, XSBundle, XSModule}
import xiangshan.backend.datapath.WbConfig.{PregWB, _}
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Bundles.{VType, Vxrm}
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.wrapper.{CSRInput, CSRToDecode}

class ExeUnitIO(params: ExeUnitParams)(implicit p: Parameters) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect()))
  val in = Flipped(DecoupledIO(new ExuInput(params)))
  val out = DecoupledIO(new ExuOutput(params))
  val csrin = Option.when(params.hasCSR)(new CSRInput)
  val csrio = Option.when(params.hasCSR)(new CSRFileIO)
  val csrToDecode = Option.when(params.hasCSR)(Output(new CSRToDecode))
  val fenceio = Option.when(params.hasFence)(new FenceIO)
  val frm = Option.when(params.needSrcFrm)(Input(Frm()))
  val vxrm = Option.when(params.needSrcVxrm)(Input(Vxrm()))
  val vtype = Option.when(params.writeVConfig)((Valid(new VType)))
  val vlIsZero = Option.when(params.writeVConfig)(Output(Bool()))
  val vlIsVlmax = Option.when(params.writeVConfig)(Output(Bool()))
  val instrAddrTransType = Option.when(params.hasJmpFu || params.hasBrhFu)(Input(new AddrTransType))
}

class ExeUnit(val exuParams: ExeUnitParams)(implicit p: Parameters) extends LazyModule {
  override def shouldBeInlined: Boolean = false

  lazy val module = new ExeUnitImp(this)(p, exuParams)
}

class ExeUnitImp(
  override val wrapper: ExeUnit
)(implicit
  p: Parameters, exuParams: ExeUnitParams
) extends LazyModuleImp(wrapper) with HasXSParameter{
  private val fuCfgs = exuParams.fuConfigs

  val io = IO(new ExeUnitIO(exuParams))

  val funcUnits = fuCfgs.map(cfg => {
    assert(cfg.fuGen != null, cfg.name + "Cfg'fuGen is null !!!")
    val module = cfg.fuGen(p, cfg)
    module
  })

  if (EnableClockGate) {
    fuCfgs.zip(funcUnits).foreach { case (cfg, fu) =>
      val clk_en = WireInit(false.B)
      val fuVld_en = WireInit(false.B)
      val fuVld_en_reg = RegInit(false.B)
      val uncer_en_reg = RegInit(false.B)

      def latReal: Int = cfg.latency.latencyVal.getOrElse(0)
      def extralat: Int = cfg.latency.extraLatencyVal.getOrElse(0)

      val uncerLat = cfg.latency.uncertainEnable.nonEmpty
      val lat0 = (latReal == 0 && !uncerLat).asBool
      val latN = (latReal >  0 && !uncerLat).asBool

      val fuVldVec = (io.in.valid && latN) +: Seq.fill(latReal)(RegInit(false.B))
      val fuRdyVec = Seq.fill(latReal)(Wire(Bool())) :+ io.out.ready

      for (i <- 0 until latReal) {
        fuRdyVec(i) := !fuVldVec(i + 1) || fuRdyVec(i + 1)
      }

      for (i <- 1 to latReal) {
        when(fuRdyVec(i - 1) && fuVldVec(i - 1)) {
          fuVldVec(i) := fuVldVec(i - 1)
        }.elsewhen(fuRdyVec(i)) {
          fuVldVec(i) := false.B
        }
      }
      fuVld_en := fuVldVec.map(v => v).reduce(_ || _)
      fuVld_en_reg := fuVld_en

      when(uncerLat.asBool && io.in.fire) {
        uncer_en_reg := true.B
      }.elsewhen(uncerLat.asBool && io.out.fire) {
        uncer_en_reg := false.B
      }

      when(lat0 && io.in.fire) {
        clk_en := true.B
      }.elsewhen(latN && fuVld_en || fuVld_en_reg) {
        clk_en := true.B
      }.elsewhen(uncerLat.asBool && io.in.fire || uncer_en_reg) {
        clk_en := true.B
      }

      if (cfg.ckAlwaysEn) {
        clk_en := true.B
      }

      fu.clock := ClockGate(false.B, clk_en, clock)
      XSPerfAccumulate(s"clock_gate_en_${fu.cfg.name}", clk_en)
    }
  }

  val busy = RegInit(false.B)
  if (exuParams.latencyCertain){
    busy := false.B
  }
  else {
    val robIdx = RegEnable(io.in.bits.robIdx, io.in.fire)
    when(io.in.fire && io.in.bits.robIdx.needFlush(io.flush)) {
      busy := false.B
    }.elsewhen(busy && robIdx.needFlush(io.flush)) {
      busy := false.B
    }.elsewhen(io.out.fire) {
      busy := false.B
    }.elsewhen(io.in.fire) {
      busy := true.B
    }
  }

  exuParams.wbPortConfigs.map{
    x => x match {
      case IntWB(port, priority) => assert(priority >= 0 && priority <= 2,
        s"${exuParams.name}: WbPort must priority=0 or priority=1")
      case FpWB(port, priority) => assert(priority >= 0 && priority <= 2,
        s"${exuParams.name}: WbPort must priority=0 or priority=1")
      case VfWB (port, priority) => assert(priority >= 0 && priority <= 2,
        s"${exuParams.name}: WbPort must priority=0 or priority=1")
      case _ =>
    }
  }
  val intWbPort = exuParams.getIntWBPort
  if (intWbPort.isDefined){
    val sameIntPortExuParam = backendParams.allExuParams.filter(_.getIntWBPort.isDefined)
      .filter(_.getIntWBPort.get.port == intWbPort.get.port)
    val samePortOneCertainOneUncertain = sameIntPortExuParam.map(_.latencyCertain).contains(true) && sameIntPortExuParam.map(_.latencyCertain).contains(false)
    if (samePortOneCertainOneUncertain) sameIntPortExuParam.map(samePort =>
      samePort.wbPortConfigs.map(
        x => x match {
          case IntWB(port, priority) => {
            if (!samePort.latencyCertain) assert(priority == sameIntPortExuParam.size - 1,
              s"${samePort.name}: IntWbPort $port must latencyCertain priority=0 or latencyUnCertain priority=max(${sameIntPortExuParam.size - 1})")
            // Certain latency can be handled by WbBusyTable, so there is no need to limit the exu's WB priority
          }
          case _ =>
        }
      )
    )
  }
  val fpWbPort = exuParams.getFpWBPort
  if (fpWbPort.isDefined) {
    val sameFpPortExuParam = backendParams.allExuParams.filter(_.getFpWBPort.isDefined)
      .filter(_.getFpWBPort.get.port == fpWbPort.get.port)
    val samePortOneCertainOneUncertain = sameFpPortExuParam.map(_.latencyCertain).contains(true) && sameFpPortExuParam.map(_.latencyCertain).contains(false)
    if (samePortOneCertainOneUncertain) sameFpPortExuParam.map(samePort =>
      samePort.wbPortConfigs.map(
        x => x match {
          case FpWB(port, priority) => {
            if (!samePort.latencyCertain) assert(priority == sameFpPortExuParam.size - 1,
              s"${samePort.name}: FpWbPort $port must latencyCertain priority=0 or latencyUnCertain priority=max(${sameFpPortExuParam.size - 1})")
            // Certain latency can be handled by WbBusyTable, so there is no need to limit the exu's WB priority
          }
          case _ =>
        }
      )
    )
  }
  val vfWbPort = exuParams.getVfWBPort
  if (vfWbPort.isDefined) {
    val sameVfPortExuParam = backendParams.allExuParams.filter(_.getVfWBPort.isDefined)
      .filter(_.getVfWBPort.get.port == vfWbPort.get.port)
    val samePortOneCertainOneUncertain = sameVfPortExuParam.map(_.latencyCertain).contains(true) && sameVfPortExuParam.map(_.latencyCertain).contains(false)
    if (samePortOneCertainOneUncertain)  sameVfPortExuParam.map(samePort =>
      samePort.wbPortConfigs.map(
        x => x match {
          case VfWB(port, priority) => {
            if (!samePort.latencyCertain) assert(priority == sameVfPortExuParam.size - 1,
              s"${samePort.name}: VfWbPort $port must latencyCertain priority=0 or latencyUnCertain priority=max(${sameVfPortExuParam.size - 1})")
            // Certain latency can be handled by WbBusyTable, so there is no need to limit the exu's WB priority
          }
          case _ =>
        }
      )
    )
  }
  if(backendParams.debugEn) {
    dontTouch(io.out.ready)
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
  in1ToN.io.in.valid := io.in.valid && !busy
  in1ToN.io.in.bits := io.in.bits
  io.in.ready := !busy && in1ToN.io.in.ready

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
      sink.bits.ctrl.v0Wen       .foreach(x => x := source.bits.v0Wen.get)
      sink.bits.ctrl.vlWen       .foreach(x => x := source.bits.vlWen.get)
      sink.bits.ctrl.flushPipe   .foreach(x => x := source.bits.flushPipe.get)
      sink.bits.ctrl.preDecode   .foreach(x => x := source.bits.preDecode.get)
      sink.bits.ctrl.ftqIdx      .foreach(x => x := source.bits.ftqIdx.get)
      sink.bits.ctrl.ftqOffset   .foreach(x => x := source.bits.ftqOffset.get)
      sink.bits.ctrl.predictInfo .foreach(x => x := source.bits.predictInfo.get)
      sink.bits.ctrl.fpu         .foreach(x => x := source.bits.fpu.get)
      sink.bits.ctrl.vpu         .foreach(x => x := source.bits.vpu.get)
      sink.bits.ctrl.vpu         .foreach(x => x.fpu.isFpToVecInst := 0.U)
      sink.bits.ctrl.vpu         .foreach(x => x.fpu.isFP32Instr   := 0.U)
      sink.bits.ctrl.vpu         .foreach(x => x.fpu.isFP64Instr   := 0.U)
      sink.bits.perfDebugInfo    := source.bits.perfDebugInfo
  }

  private val OutresVecs = funcUnits.map { fu =>
    def latDiff :Int = fu.cfg.latency.extraLatencyVal.getOrElse(0)
    val OutresVec = fu.io.out.bits.res +: Seq.fill(latDiff)(Reg(chiselTypeOf(fu.io.out.bits.res)))
    for (i <- 1 to latDiff) {
      OutresVec(i) := OutresVec(i - 1)
    }
    OutresVec
  }
  OutresVecs.foreach(vec => vec.foreach(res =>dontTouch(res)))

  private val fuOutValidOH = funcUnits.map(_.io.out.valid)
  XSError(PopCount(fuOutValidOH) > 1.U, p"fuOutValidOH ${Binary(VecInit(fuOutValidOH).asUInt)} should be one-hot)\n")
  private val fuOutBitsVec = funcUnits.map(_.io.out.bits)
  private val fuOutresVec = OutresVecs.map(_.last)
  private val fuRedirectVec: Seq[Option[ValidIO[Redirect]]] = fuOutresVec.map(_.redirect)

  // Assume that one fu can only write int or fp or vec,
  // otherwise, wenVec should be assigned to wen in fu.
  private val fuIntWenVec = funcUnits.map(x => x.cfg.needIntWen.B && x.io.out.bits.ctrl.rfWen.getOrElse(false.B))
  private val fuFpWenVec  = funcUnits.map( x => x.cfg.needFpWen.B  && x.io.out.bits.ctrl.fpWen.getOrElse(false.B))
  private val fuVecWenVec = funcUnits.map(x => x.cfg.needVecWen.B && x.io.out.bits.ctrl.vecWen.getOrElse(false.B))
  private val fuV0WenVec = funcUnits.map(x => x.cfg.needV0Wen.B && x.io.out.bits.ctrl.v0Wen.getOrElse(false.B))
  private val fuVlWenVec = funcUnits.map(x => x.cfg.needVlWen.B && x.io.out.bits.ctrl.vlWen.getOrElse(false.B))
  // FunctionUnits <---> ExeUnit.out

  private val outDataVec = Seq(
    Some(fuOutresVec.map(_.data)),
    Option.when(funcUnits.exists(_.cfg.writeIntRf))
      (funcUnits.zip(fuOutresVec).filter{ case (fu, _) => fu.cfg.writeIntRf}.map{ case(_, fuout) => fuout.data}),
    Option.when(funcUnits.exists(_.cfg.writeFpRf))
      (funcUnits.zip(fuOutresVec).filter{ case (fu, _) => fu.cfg.writeFpRf}.map{ case(_, fuout) => fuout.data}),
    Option.when(funcUnits.exists(_.cfg.writeVecRf))
      (funcUnits.zip(fuOutresVec).filter{ case (fu, _) => fu.cfg.writeVecRf}.map{ case(_, fuout) => fuout.data}),
    Option.when(funcUnits.exists(_.cfg.writeV0Rf))
      (funcUnits.zip(fuOutresVec).filter{ case (fu, _) => fu.cfg.writeV0Rf}.map{ case(_, fuout) => fuout.data}),
    Option.when(funcUnits.exists(_.cfg.writeVlRf))
      (funcUnits.zip(fuOutresVec).filter{ case (fu, _) => fu.cfg.writeVlRf}.map{ case(_, fuout) => fuout.data}),
  ).flatten
  private val outDataValidOH = Seq(
    Some(fuOutValidOH),
    Option.when(funcUnits.exists(_.cfg.writeIntRf))
      (funcUnits.zip(fuOutValidOH).filter{ case (fu, _) => fu.cfg.writeIntRf}.map{ case(_, fuoutOH) => fuoutOH}),
    Option.when(funcUnits.exists(_.cfg.writeFpRf))
      (funcUnits.zip(fuOutValidOH).filter{ case (fu, _) => fu.cfg.writeFpRf}.map{ case(_, fuoutOH) => fuoutOH}),
    Option.when(funcUnits.exists(_.cfg.writeVecRf))
      (funcUnits.zip(fuOutValidOH).filter{ case (fu, _) => fu.cfg.writeVecRf}.map{ case(_, fuoutOH) => fuoutOH}),
    Option.when(funcUnits.exists(_.cfg.writeV0Rf))
      (funcUnits.zip(fuOutValidOH).filter{ case (fu, _) => fu.cfg.writeV0Rf}.map{ case(_, fuoutOH) => fuoutOH}),
    Option.when(funcUnits.exists(_.cfg.writeVlRf))
      (funcUnits.zip(fuOutValidOH).filter{ case (fu, _) => fu.cfg.writeVlRf}.map{ case(_, fuoutOH) => fuoutOH}),
  ).flatten

  io.out.valid := Cat(fuOutValidOH).orR
  funcUnits.foreach(fu => fu.io.out.ready := io.out.ready)

  // select one fu's result
  io.out.bits.data := VecInit(outDataVec.zip(outDataValidOH).map{ case(data, validOH) => Mux1H(validOH, data)})
  io.out.bits.robIdx := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.robIdx))
  io.out.bits.pdest := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.pdest))
  io.out.bits.intWen.foreach(x => x := Mux1H(fuOutValidOH, fuIntWenVec))
  io.out.bits.fpWen.foreach(x => x := Mux1H(fuOutValidOH, fuFpWenVec))
  io.out.bits.vecWen.foreach(x => x := Mux1H(fuOutValidOH, fuVecWenVec))
  io.out.bits.v0Wen.foreach(x => x := Mux1H(fuOutValidOH, fuV0WenVec))
  io.out.bits.vlWen.foreach(x => x := Mux1H(fuOutValidOH, fuVlWenVec))
  io.out.bits.redirect.foreach(x => x := Mux1H((fuOutValidOH zip fuRedirectVec).filter(_._2.isDefined).map(x => (x._1, x._2.get))))
  io.out.bits.fflags.foreach(x => x := Mux1H(fuOutValidOH, fuOutresVec.map(_.fflags.getOrElse(0.U.asTypeOf(io.out.bits.fflags.get)))))
  io.out.bits.wflags.foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.fpu.getOrElse(0.U.asTypeOf(new FPUCtrlSignals)).wflags)))
  io.out.bits.vxsat.foreach(x => x := Mux1H(fuOutValidOH, fuOutresVec.map(_.vxsat.getOrElse(0.U.asTypeOf(io.out.bits.vxsat.get)))))
  io.out.bits.exceptionVec.foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.exceptionVec.getOrElse(0.U.asTypeOf(io.out.bits.exceptionVec.get)))))
  io.out.bits.flushPipe.foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.flushPipe.getOrElse(0.U.asTypeOf(io.out.bits.flushPipe.get)))))
  io.out.bits.replay.foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.replay.getOrElse(0.U.asTypeOf(io.out.bits.replay.get)))))
  io.out.bits.predecodeInfo.foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.preDecode.getOrElse(0.U.asTypeOf(io.out.bits.predecodeInfo.get)))))

  io.csrio.foreach(exuio => funcUnits.foreach(fu => fu.io.csrio.foreach{
    fuio =>
      exuio <> fuio
      fuio.exception := DelayN(exuio.exception, 2)
  }))
  io.csrin.foreach(exuio => funcUnits.foreach(fu => fu.io.csrin.foreach{fuio => fuio := exuio}))
  io.csrToDecode.foreach(toDecode => funcUnits.foreach(fu => fu.io.csrToDecode.foreach(fuOut => toDecode := fuOut)))

  io.vtype.foreach(exuio => funcUnits.foreach(fu => fu.io.vtype.foreach(fuio => exuio := fuio)))
  io.fenceio.foreach(exuio => funcUnits.foreach(fu => fu.io.fenceio.foreach(fuio => fuio <> exuio)))
  io.frm.foreach(exuio => funcUnits.foreach(fu => fu.io.frm.foreach(fuio => fuio <> exuio)))
  io.vxrm.foreach(exuio => funcUnits.foreach(fu => fu.io.vxrm.foreach(fuio => fuio <> exuio)))
  io.vlIsZero.foreach(exuio => funcUnits.foreach(fu => fu.io.vlIsZero.foreach(fuio => exuio := fuio)))
  io.vlIsVlmax.foreach(exuio => funcUnits.foreach(fu => fu.io.vlIsVlmax.foreach(fuio => exuio := fuio)))
  io.instrAddrTransType.foreach(exuio => funcUnits.foreach(fu => fu.io.instrAddrTransType.foreach(fuio => fuio := exuio)))

  // debug info
  io.out.bits.debug     := 0.U.asTypeOf(io.out.bits.debug)
  io.out.bits.debug.isPerfCnt := funcUnits.map(_.io.csrio.map(_.isPerfCnt)).map(_.getOrElse(false.B)).reduce(_ || _)
  io.out.bits.debugInfo := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.perfDebugInfo))
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

  XSError(io.in.valid && PopCount(acceptVec) > 1.U, p"[ExeUnit] accept vec should no more than 1, ${Binary(acceptVec.asUInt)} ")
  XSError(io.in.valid && PopCount(acceptVec) === 0.U, "[ExeUnit] there is a inst not dispatched to any fu")

  io.out.zipWithIndex.foreach { case (out, i) =>
    out.valid := acceptVec(i) && io.in.valid
    out.bits := io.in.bits
  }

  io.in.ready := Cat(io.out.map(_.ready)).andR
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
  fu.io.in.bits.perfDebugInfo := io.in.bits.uop.debugInfo

  io.out.valid            := fu.io.out.valid
  fu.io.out.ready         := io.out.ready

  io.out.bits             := 0.U.asTypeOf(io.out.bits) // dontCare other fields
  io.out.bits.data        := fu.io.out.bits.res.data
  io.out.bits.uop.robIdx  := fu.io.out.bits.ctrl.robIdx
  io.out.bits.uop.pdest   := fu.io.out.bits.ctrl.pdest
  io.out.bits.uop.fuType  := io.in.bits.uop.fuType
  io.out.bits.uop.fuOpType:= io.in.bits.uop.fuOpType
  io.out.bits.uop.sqIdx   := io.in.bits.uop.sqIdx
  io.out.bits.uop.debugInfo := fu.io.out.bits.perfDebugInfo

  io.out.bits.debug       := 0.U.asTypeOf(io.out.bits.debug)
}