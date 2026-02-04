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
import xiangshan.backend.fu.{CSRFileIO, FenceIO, FuType, FuncUnitInput, UncertainLatency}
import xiangshan.backend.Bundles._
import xiangshan.{AddrTransType, FPUCtrlSignals, HasXSParameter, Redirect, Resolve, XSBundle, XSModule}
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.fu.vector.Bundles.{VType, Vxrm}
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.wrapper.{CSRInput, CSRToDecode}
import xiangshan.backend.fu.FuConfig.{AluCfg, I2fCfg, needUncertainWakeupFuConfigs}
import xiangshan._

class ExeUnitIO(params: ExeUnitParams)(implicit p: Parameters) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect()))
  val in = Flipped(DecoupledIO(new NewExuInput(params, hasCopySrc = true)))
  val out = DecoupledIO(new NewExuOutput(params))
  val uncertainWakeupOut = Option.when(params.needUncertainWakeup)(DecoupledIO(new IssueQueueIQWakeUpBundle(params.exuIdx, params.backendParam, params.copyWakeupOut, params.copyNum)))
  val csrin = Option.when(params.hasCSR)(new CSRInput)
  val csrio = Option.when(params.hasCSR)(new CSRFileIO)
  val toFrontendBJUResolve = Option.when(params.hasBrhFu)(Valid(new Resolve))
  val I2FDataIn = Option.when(params.needDataFromI2F)(Flipped(ValidIO(new ToRegFile(params))))
  val F2IDataIn = Option.when(params.needDataFromF2I)(Flipped(ValidIO(new ToRegFile(params))))
  val F2VDataIn = Option.when(params.needDataFromF2V)(Flipped(ValidIO(new ToRegFile(params))))
  val V2FDataIn = Option.when(params.needDataFromV2F)(Flipped(ValidIO(new ToRegFile(params))))
  val V2IDataIn = Option.when(params.needDataFromV2I)(Flipped(ValidIO(new ToRegFile(params))))
  val I2VDataIn = Option.when(params.needDataFromI2V)(Flipped(ValidIO(new ToRegFile(params))))
  val I2FOutValid = Option.when(params.writeFpRf)(Bool())
  val V2FOutValid = Option.when(params.writeFpRf)(Bool())
  val F2IOutValid = Option.when(params.writeIntRf)(Bool())
  val V2IOutValid = Option.when(params.writeIntRf)(Bool())
  val I2VOutValid = Option.when(params.writeVecRf)(Bool())
  val F2VOutValid = Option.when(params.writeVecRf)(Bool())
  val csrToDecode = Option.when(params.hasCSR)(Output(new CSRToDecode))
  val fenceio = Option.when(params.hasFence)(new FenceIO)
  val frm = Option.when(params.needSrcFrm)(Input(Frm()))
  val vxrm = Option.when(params.needSrcVxrm)(Input(Vxrm()))
  val vtype = Option.when(params.writeVlRf)((Valid(new VType)))
  val vlIsZero = Option.when(params.writeVlRf)(Output(Bool()))
  val vlIsVlmax = Option.when(params.writeVlRf)(Output(Bool()))
  val instrAddrTransType = Option.when(params.hasJmpFu || params.hasBrhFu || params.hasAluFu)(Input(new AddrTransType))
}

class ExeUnitImp(implicit p: Parameters, val exuParams: ExeUnitParams) extends XSModule with HasXSParameter with HasCriticalErrors {
  private val fuCfgs = exuParams.fuConfigs

  val io = IO(new ExeUnitIO(exuParams))

  val funcUnits = fuCfgs.map(cfg => {
    assert(cfg.fuGen != null, cfg.name + "Cfg'fuGen is null !!!")
    if (exuParams.aluNeedPc && cfg.isAlu) {
      AluCfg.aluNeedPc = true
      println(s"[ExeUnit] ${exuParams.name}'s alu need pc")
    }
    val module = cfg.fuGen(p, cfg)
    AluCfg.aluNeedPc = false
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

      val fuVldVec = (fu.io.in.valid && latN) +: Seq.fill(latReal)(RegInit(false.B))
      val fuRdyVec = Seq.fill(latReal)(Wire(Bool())) :+ fu.io.out.ready

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

      when(uncerLat.asBool && fu.io.in.fire) {
        uncer_en_reg := true.B
      }.elsewhen(uncerLat.asBool && fu.io.out.fire) {
        uncer_en_reg := false.B
      }

      when(lat0 && fu.io.in.fire) {
        clk_en := true.B
      }.elsewhen(latN && fuVld_en || fuVld_en_reg) {
        clk_en := true.B
      }.elsewhen(uncerLat.asBool && fu.io.in.fire || uncer_en_reg) {
        clk_en := true.B
      }

      if (cfg.ckAlwaysEn) {
        clk_en := true.B
      }

      if (latReal != 0 || uncerLat) {
        fu.clock := ClockGate(ClockGate.genTeSink.cgen, clk_en, clock)
      }
      XSPerfAccumulate(s"clock_gate_en_${fu.cfg.name}", clk_en)
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
  if(backendParams.debugEn) {
    dontTouch(io.out.ready)
  }
  // rob flush --> funcUnits
  funcUnits.zipWithIndex.foreach { case (fu, i) =>
    fu.io.flush <> io.flush
  }

  def acceptCond(input: NewExuInput): Seq[Bool] = {
    input.params.fuConfigs.map(_.fuSel(input))
  }

  val in1ToN = Module(new Dispatcher(new NewExuInput(exuParams), funcUnits.length, acceptCond))

  // ExeUnit.in <---> Dispatcher.in
  in1ToN.io.in.valid := io.in.valid
  in1ToN.io.in.bits := io.in.bits
  io.in.ready := in1ToN.io.in.ready

  def pipelineReg(init: NewExuInput, valid: Bool, latency: Int, flush: ValidIO[Redirect]): (Seq[NewExuInput], Seq[Bool]) = {
    val validVec = valid +: Seq.fill(latency)(RegInit(false.B))
    val inVec = init +: Seq.fill(latency)(Reg(new NewExuInput(exuParams)))
    val robIdxVec = inVec.map(_.robIdx)
    // if flush(0), valid 0 will not given, so set flushVec(0) to false.B
    val flushVec = validVec.zip(robIdxVec).map(x => x._1 && x._2.needFlush(flush))
    for (i <- 1 to latency) {
      validVec(i) := validVec(i - 1) && !flushVec(i - 1)
      inVec(i) := inVec(i - 1)
    }
    (inVec, validVec)
  }
  val latencyMax = fuCfgs.map(_.latency.latencyVal.getOrElse(0)).max
  val inPipe = pipelineReg(io.in.bits, io.in.valid, latencyMax, io.flush)
  // Dispatcher.out <---> FunctionUnits
  in1ToN.io.out.zip(funcUnits.map(_.io.in)).foreach {
    case (source: DecoupledIO[NewExuInput], sink: DecoupledIO[FuncUnitInput]) =>
      sink.valid := source.valid
      source.ready := sink.ready

      sink.bits.data.pc          .foreach(x => x := source.bits.data.pc.get)
      sink.bits.data.nextPcOffset.foreach(x => x := source.bits.data.nextPcOffset.get)
      sink.bits.data.imm         := source.bits.data.imm
      sink.bits.ctrl.fuOpType    := source.bits.ctrl.fuOpType
      sink.bits.ctrl.toRobValid  := source.bits.toRobValid
      sink.bits.ctrl.robIdx      := source.bits.robIdx
      sink.bits.ctrl.pdest       := source.bits.toRF.pdest
      sink.bits.ctrl.pdestVl     .foreach(x => x := source.bits.toRF.pdestVl.get)
      sink.bits.ctrl.rfWen       .foreach(x => x := source.bits.ctrl.rfWen.get)
      sink.bits.ctrl.fpWen       .foreach(x => x := source.bits.ctrl.fpWen.get)
      sink.bits.ctrl.vecWen      .foreach(x => x := source.bits.ctrl.vecWen.get)
      sink.bits.ctrl.v0Wen       .foreach(x => x := source.bits.ctrl.v0Wen.get)
      sink.bits.ctrl.vlWen       .foreach(x => x := source.bits.ctrl.vlWen.get)
      sink.bits.ctrl.flushPipe   .foreach(x => x := source.bits.ctrl.flushPipe.get)
      sink.bits.ctrl.isRVC       .foreach(x => x := source.bits.ctrl.isRVC.get)
      sink.bits.ctrl.rasAction   .foreach(x=> x  := source.bits.ctrl.rasAction.get)
      sink.bits.ctrl.ftqIdx      .foreach(x => x := source.bits.ctrl.ftqIdx.get)
      sink.bits.ctrl.ftqOffset   .foreach(x => x := source.bits.ctrl.ftqOffset.get)
      sink.bits.ctrl.predictInfo .foreach(x => x := source.bits.ctrl.predictInfo.get)
      sink.bits.ctrl.fpu         .foreach(x => x := source.bits.ctrl.fpu.get)
      sink.bits.ctrl.vpu         .foreach(x => x := source.bits.ctrl.vpu.get)
      sink.bits.ctrl.vialuCtrl   .foreach(x => x := source.bits.ctrl.vialuCtrl.get)
      sink.bits.ctrl.vpu         .foreach(x => x.fpu.isFpToVecInst := 0.U)
      sink.bits.ctrl.vpu         .foreach(x => x.fpu.isFP32Instr   := 0.U)
      sink.bits.ctrl.vpu         .foreach(x => x.fpu.isFP64Instr   := 0.U)
      sink.bits.perfDebugInfo    .foreach(_ := source.bits.perfDebugInfo.get)
      sink.bits.debug_seqNum     .foreach(_ := source.bits.debug_seqNum.get)
  }
  funcUnits.filter(_.cfg.latency.latencyVal.nonEmpty).map{ fu =>
    val latency = fu.cfg.latency.latencyVal.getOrElse(0)
    if (fu.cfg == I2fCfg){
      println(s"I2fCfg latency = $latency")
    }
    for (i <- 0 until (latency+1)) {
      val sink = fu.io.in.bits.ctrlPipe.get(i)
      val source = inPipe._1(i)
      fu.io.in.bits.validPipe.get(i) := inPipe._2(i)
      sink.fuOpType := source.ctrl.fuOpType
      sink.toRobValid := source.toRobValid
      sink.robIdx := source.robIdx
      sink.pdest := source.toRF.pdest
      sink.pdestVl.foreach(_ := source.toRF.pdestVl.get)
      sink.rfWen.foreach(      x => x := source.ctrl.rfWen.get)
      sink.fpWen.foreach(      x => x := source.ctrl.fpWen.get)
      sink.vecWen.foreach(     x => x := source.ctrl.vecWen.get)
      sink.v0Wen.foreach(      x => x := source.ctrl.v0Wen.get)
      sink.vlWen.foreach(      x => x := source.ctrl.vlWen.get)
      sink.flushPipe.foreach(  x => x := source.ctrl.flushPipe.get)
      sink.isRVC.foreach(      x => x := source.ctrl.isRVC.get)
      sink.rasAction.foreach(  x => x := source.ctrl.rasAction.get)
      sink.ftqIdx.foreach(     x => x := source.ctrl.ftqIdx.get)
      sink.ftqOffset.foreach(  x => x := source.ctrl.ftqOffset.get)
      sink.predictInfo.foreach(x => x := source.ctrl.predictInfo.get)
      sink.fpu.foreach(x => x := source.ctrl.fpu.get)
      sink.vpu.foreach(x => x := source.ctrl.vpu.get)
      sink.vpu.foreach(x => x.fpu.isFpToVecInst := 0.U)
      sink.vpu.foreach(x => x.fpu.isFP32Instr := 0.U)
      sink.vpu.foreach(x => x.fpu.isFP64Instr := 0.U)
      sink.vpu.foreach(x => x.maskVecGen := 0.U)
      sink.vialuCtrl.foreach(x => x := 0.U.asTypeOf(new VIAluCtrlSignals))
      val sinkData = fu.io.in.bits.dataPipe.get(i)
      val sourceData = inPipe._1(i)
      sinkData.src.zip(sourceData.data.src).foreach { case (fuSrc, exuSrc) => fuSrc := exuSrc }
      sinkData.vl.foreach(_ := sourceData.data.vl.get)
      sinkData.pc.foreach(x => x := sourceData.data.pc.get)
      sinkData.nextPcOffset.foreach(x => x := sourceData.data.nextPcOffset.get)
      sinkData.imm := sourceData.data.imm
    }
  }

  funcUnits.zip(exuParams.idxCopySrc).map{ case(fu, idx) =>
    (fu.io.in.bits.data.src).zip(io.in.bits.data.src).foreach { case(fuSrc, exuSrc) => fuSrc := exuSrc }
    if(fu.cfg.srcNeedCopy) {
      (fu.io.in.bits.data.src).zip(io.in.bits.copy.copySrc.get(idx)).foreach { case(fuSrc, copySrc) => fuSrc := copySrc }
    }
    fu.io.in.bits.data.vl.foreach(_ := io.in.bits.data.vl.get)
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

  private val fuOutValidOH = Wire(Vec(funcUnits.length, Bool()))
  fuOutValidOH := funcUnits.map{ case fu => {
    if (needUncertainWakeupFuConfigs.contains(fu.cfg)){
      println(p"${exuParams.name}: ${fu.cfg.name} is needUncertainWakeupFuConfig")
      !funcUnits.filterNot(x => needUncertainWakeupFuConfigs.contains(x.cfg)).map(y => y.io.out.valid).fold(false.B)(_ || _) && fu.io.out.valid
    }
    else {
      fu.io.out.valid
    }
  }
  }
  dontTouch(fuOutValidOH)
  XSError(PopCount(fuOutValidOH) > 1.U, p"fuOutValidOH ${Binary(fuOutValidOH.asUInt)} should be one-hot)\n")
  private val fuOutBitsVec = funcUnits.map(_.io.out.bits)
  private val fuOutresVec = OutresVecs.map(_.last)
  private val fuRedirectVec: Seq[Option[ValidIO[Redirect]]] = fuOutresVec.map(_.redirect)

  // Assume that one fu can only write int or fp or vec,
  // otherwise, wenVec should be assigned to wen in fu.
  private val fuIntWenVec = funcUnits.map(x => x.cfg.needIntWen.B && x.io.out.bits.ctrl.rfWen.getOrElse(false.B)  && !x.cfg.isFpWenInt.B && !x.cfg.isVecWenInt.B)
  private val fuFpWenVec  = funcUnits.map(x => x.cfg.needFpWen.B  && x.io.out.bits.ctrl.fpWen.getOrElse(false.B)  && !x.cfg.isIntWenFp.B && !x.cfg.isVecWenFp.B)
  private val fuVecWenVec = funcUnits.map(x => x.cfg.needVecWen.B && x.io.out.bits.ctrl.vecWen.getOrElse(false.B) && !x.cfg.isIntWenVec.B && !x.cfg.isFpWenVec.B)
  private val fuV0WenVec = funcUnits.map(x => x.cfg.needV0Wen.B && x.io.out.bits.ctrl.v0Wen.getOrElse(false.B))
  private val fuVlWenVec = funcUnits.map(x => x.cfg.needVlWen.B && x.io.out.bits.ctrl.vlWen.getOrElse(false.B))
  // FunctionUnits <---> ExeUnit.out

  private val i2fWenVec = funcUnits.map(x => x.cfg.isIntWenFp.B && x.io.out.bits.ctrl.fpWen.getOrElse(false.B))
  private val v2fWenVec = funcUnits.map(x => x.cfg.isVecWenFp.B && x.io.out.bits.ctrl.fpWen.getOrElse(false.B))
  private val f2iWenVec = funcUnits.map(x => x.cfg.isFpWenInt.B && x.io.out.bits.ctrl.rfWen.getOrElse(false.B))
  private val v2iWenVec = funcUnits.map(x => x.cfg.isVecWenInt.B && x.io.out.bits.ctrl.rfWen.getOrElse(false.B))
  private val i2vWenVec = funcUnits.map(x => x.cfg.isIntWenVec.B && x.io.out.bits.ctrl.vecWen.getOrElse(false.B))
  private val f2vWenVec = funcUnits.map(x => x.cfg.isFpWenVec.B && x.io.out.bits.ctrl.vecWen.getOrElse(false.B))

  private val outIntData = Option.when(funcUnits.exists(_.cfg.writeIntRf))(WireInit(0.U(exuParams.destDataBitsMax.W)))
  private val outFpData  = Option.when(funcUnits.exists(_.cfg.writeFpRf)) (WireInit(0.U(exuParams.destDataBitsMax.W)))
  private val outVecData = Option.when(funcUnits.exists(_.cfg.writeVecRf))(WireInit(0.U(exuParams.destDataBitsMax.W)))
  private val outIntPdest = Option.when(funcUnits.exists(_.cfg.writeIntRf))(WireInit(0.U(IntPhyRegIdxWidth.W)))
  private val outFpPdest  = Option.when(funcUnits.exists(_.cfg.writeFpRf)) (WireInit(0.U(FpPhyRegIdxWidth.W)))
  private val outVecPdest = Option.when(funcUnits.exists(_.cfg.writeVecRf))(WireInit(0.U(VfPhyRegIdxWidth.W)))

  Option.when(funcUnits.exists(_.cfg.writeIntRf)) {
    val vld = if (exuParams.needDataFromF2I) {
        (funcUnits.zip(fuOutValidOH).filter { case (fu, _) => fu.cfg.writeIntRf }.map { case (fu, fuoutOH) =>
          !io.F2IDataIn.get.valid && fuoutOH && fu.io.out.bits.ctrl.rfWen.getOrElse(false.B) } :+ io.F2IDataIn.get.valid)
      }
      else if (exuParams.needDataFromV2I) {
        (funcUnits.zip(fuOutValidOH).filter { case (fu, _) => fu.cfg.writeIntRf }.map { case (fu, fuoutOH) =>
          !io.V2IDataIn.get.valid && fuoutOH && fu.io.out.bits.ctrl.rfWen.getOrElse(false.B) } :+ io.V2IDataIn.get.valid)
      }
      else {
        (funcUnits.zip(fuOutValidOH).filter { case (fu, _) => fu.cfg.writeIntRf }.map { case (fu, fuoutOH) =>
          fuoutOH && fu.io.out.bits.ctrl.rfWen.getOrElse(false.B) })
      }
    val pdest = if (exuParams.needDataFromF2I) {
        (funcUnits.zip(fuOutBitsVec).filter { case (fu, _) => fu.cfg.writeIntRf }.map { case (_, fuout) => fuout.ctrl.pdest } :+ io.F2IDataIn.get.bits.pdest)
      }
      else if (exuParams.needDataFromV2I) {
        (funcUnits.zip(fuOutBitsVec).filter { case (fu, _) => fu.cfg.writeIntRf }.map { case (_, fuout) => fuout.ctrl.pdest } :+ io.V2IDataIn.get.bits.pdest)
      }
      else {
        (funcUnits.zip(fuOutBitsVec).filter { case (fu, _) => fu.cfg.writeIntRf }.map { case (_, fuout) => fuout.ctrl.pdest })
      }
    val data = if (exuParams.needDataFromF2I) {
        (funcUnits.zip(fuOutresVec).filter { case (fu, _) => fu.cfg.writeIntRf }.map { case (_, fuout) => fuout.data } :+ io.F2IDataIn.get.bits.data)
      }
      else if (exuParams.needDataFromV2I) {
        (funcUnits.zip(fuOutresVec).filter { case (fu, _) => fu.cfg.writeIntRf }.map { case (_, fuout) => fuout.data } :+ io.V2IDataIn.get.bits.data)
      }
      else {
        (funcUnits.zip(fuOutresVec).filter { case (fu, _) => fu.cfg.writeIntRf }.map { case (_, fuout) => fuout.data })
      }
    outIntPdest.foreach(_ := Mux1H(vld, pdest))
    outIntData.foreach(_ := Mux1H(vld, data))
  }
  Option.when(funcUnits.exists(_.cfg.writeFpRf)) {
    val vld = if (exuParams.needDataFromI2F || exuParams.needDataFromV2F) {
        (funcUnits.zip(fuOutValidOH).filter { case (fu, _) => fu.cfg.writeFpRf }.map { case (fu, fuoutOH) =>
          !io.I2FDataIn.get.valid && !io.V2FDataIn.get.valid && fuoutOH && fu.io.out.bits.ctrl.fpWen.getOrElse(false.B) } :+ (io.I2FDataIn.get.valid || io.V2FDataIn.get.valid))
      }
      else {
        (funcUnits.zip(fuOutValidOH).filter { case (fu, _) => fu.cfg.writeFpRf }.map { case (fu, fuoutOH) =>
          fuoutOH && fu.io.out.bits.ctrl.fpWen.getOrElse(false.B) })
      }
    val pdest = if (exuParams.needDataFromI2F || exuParams.needDataFromV2F) {
        (funcUnits.zip(fuOutBitsVec).filter { case (fu, _) => fu.cfg.writeFpRf }.map { case (_, fuout) => fuout.ctrl.pdest } :+ Mux(io.I2FDataIn.get.valid, io.I2FDataIn.get.bits.pdest, io.V2FDataIn.get.bits.pdest))
      }
      else {
        (funcUnits.zip(fuOutBitsVec).filter { case (fu, _) => fu.cfg.writeFpRf }.map { case (_, fuout) => fuout.ctrl.pdest })
      }
    val data = if (exuParams.needDataFromI2F || exuParams.needDataFromV2F) {
        (funcUnits.zip(fuOutresVec).filter { case (fu, _) => fu.cfg.writeFpRf }.map { case (_, fuout) => fuout.data } :+ Mux(io.I2FDataIn.get.valid, io.I2FDataIn.get.bits.data, io.V2FDataIn.get.bits.data))
      }
      else {
        (funcUnits.zip(fuOutresVec).filter { case (fu, _) => fu.cfg.writeFpRf }.map { case (_, fuout) => fuout.data })
      }
    outFpPdest.foreach(_ := Mux1H(vld, pdest))
    outFpData.foreach(_ := Mux1H(vld, data))
  }
  Option.when(funcUnits.exists(_.cfg.writeVecRf)) {
    val vld = if (exuParams.needDataFromF2V || exuParams.needDataFromI2V) {
      (funcUnits.zip(fuOutValidOH).filter { case (fu, _) => fu.cfg.writeVecRf }.map { case (fu, fuoutOH) =>
        !io.F2VDataIn.get.valid && !io.I2VDataIn.get.valid && fuoutOH && fu.io.out.bits.ctrl.vecWen.getOrElse(false.B) } :+ (io.F2VDataIn.get.valid || io.I2VDataIn.get.valid))
    }
    else {
      (funcUnits.zip(fuOutValidOH).filter { case (fu, _) => fu.cfg.writeVecRf }.map { case (fu, fuoutOH) =>
        fuoutOH && fu.io.out.bits.ctrl.vecWen.getOrElse(false.B) })
    }
    val pdest = if (exuParams.needDataFromF2V || exuParams.needDataFromI2V) {
      (funcUnits.zip(fuOutBitsVec).filter { case (fu, _) => fu.cfg.writeVecRf }.map { case (_, fuout) => fuout.ctrl.pdest } :+ Mux(io.F2VDataIn.get.valid, io.F2VDataIn.get.bits.pdest, io.I2VDataIn.get.bits.pdest))
    }
    else {
      (funcUnits.zip(fuOutBitsVec).filter { case (fu, _) => fu.cfg.writeVecRf }.map { case (_, fuout) => fuout.ctrl.pdest })
    }
    val data = if (exuParams.needDataFromF2V || exuParams.needDataFromI2V) {
      (funcUnits.zip(fuOutresVec).filter { case (fu, _) => fu.cfg.writeVecRf }.map { case (_, fuout) => fuout.data } :+ Mux(io.F2VDataIn.get.valid, io.F2VDataIn.get.bits.data, io.I2VDataIn.get.bits.data))
    }
    else {
      (funcUnits.zip(fuOutresVec).filter { case (fu, _) => fu.cfg.writeVecRf }.map { case (_, fuout) => fuout.data })
    }
    outVecPdest.foreach(_ := Mux1H(vld, pdest))
    outVecData.foreach(_ := Mux1H(vld, data))
  }

  val criticalErrors = funcUnits.filter(fu => fu.cfg.needCriticalErrors).flatMap(fu => fu.getCriticalErrors)
  generateCriticalErrors()

  val F2IIntWen = io.F2IDataIn.getOrElse(0.U.asTypeOf(ValidIO(new ToRegFile(exuParams)))).valid
  val I2FFpWen  = io.I2FDataIn.getOrElse(0.U.asTypeOf(ValidIO(new ToRegFile(exuParams)))).valid
  val F2VVecWen = io.F2VDataIn.getOrElse(0.U.asTypeOf(ValidIO(new ToRegFile(exuParams)))).valid
  val V2FFpWen  = io.V2FDataIn.getOrElse(0.U.asTypeOf(ValidIO(new ToRegFile(exuParams)))).valid
  val V2IIntWen = io.V2IDataIn.getOrElse(0.U.asTypeOf(ValidIO(new ToRegFile(exuParams)))).valid
  val I2VVecWen = io.I2VDataIn.getOrElse(0.U.asTypeOf(ValidIO(new ToRegFile(exuParams)))).valid

  io.out.valid := Cat(fuOutValidOH).orR
  funcUnits.foreach{ fu =>
    fu.io.out.ready := io.out.ready
    fu.io.wakeupSuccess.foreach(_ := false.B)
  }
  io.uncertainWakeupOut.foreach{ out => {
    val uncertainFus = funcUnits.filter(x => needUncertainWakeupFuConfigs.contains(x.cfg))
    if (uncertainFus.length == 1) {
      val fu = uncertainFus(0)
      out.valid := fu.io.outValidAhead3Cycle.get
      fu.io.wakeupSuccess.get := out.ready
      out.bits := 0.U.asTypeOf(out.bits)
      // div
      fu.io.out.bits.ctrl.rfWen.foreach(x => out.bits.rfWen := x)
      fu.io.out.bits.ctrl.fpWen.foreach(x => out.bits.fpWen := x)
      fu.io.out.bits.ctrl.vecWen.foreach(x => out.bits.vecWen := x)
      fu.io.out.bits.ctrl.v0Wen.foreach(x => out.bits.v0Wen := x)
      fu.io.out.bits.ctrl.vlWen.foreach(x => out.bits.vlWen := x)
      out.bits.pdest := fu.io.out.bits.ctrl.pdest
      // csr
      if (fu.cfg.isCsr) {
        out.bits.rfWen := fu.io.outRFWenAhead3Cycle.get
        out.bits.pdest := fu.io.outPdestAhead3Cycle.get
      }
    }
    else {
      val outOH = VecInit(uncertainFus.map(_.io.outValidAhead3Cycle.get))
      val outBits = uncertainFus.map(_.io.out.bits)
      out.valid := outOH.asUInt.orR
      out.bits := 0.U.asTypeOf(out.bits)
      outBits(0).ctrl.rfWen.foreach(x =>  out.bits.rfWen  := Mux1H(outOH, outBits.map(_.ctrl.rfWen .get)))
      outBits(0).ctrl.fpWen.foreach(x =>  out.bits.fpWen  := Mux1H(outOH, outBits.map(_.ctrl.fpWen .get)))
      outBits(0).ctrl.vecWen.foreach(x => out.bits.vecWen := Mux1H(outOH, outBits.map(_.ctrl.vecWen.get)))
      outBits(0).ctrl.v0Wen.foreach(x =>  out.bits.v0Wen  := Mux1H(outOH, outBits.map(_.ctrl.v0Wen .get)))
      outBits(0).ctrl.vlWen.foreach(x =>  out.bits.vlWen  := Mux1H(outOH, outBits.map(_.ctrl.vlWen .get)))
      out.bits.pdest := Mux1H(outOH, outBits.map(_.ctrl.pdest))
      out.bits.pdestVl := Mux1H(outOH, outBits.map(_.ctrl.pdestVl.getOrElse(0.U)))
    }
  }
  }
  // select one fu's result
  io.out.bits.toIntRf.          foreach(x => x.valid      := Mux1H(fuOutValidOH, fuIntWenVec) || F2IIntWen || V2IIntWen)
  io.out.bits.toIntRf.          foreach(x => x.bits.pdest := outIntPdest.get)
  io.out.bits.toIntRf.          foreach(x => x.bits.data  := outIntData.get)
  io.out.bits.toFpRf.           foreach(x => x.valid      := Mux1H(fuOutValidOH, fuFpWenVec) || V2FFpWen || I2FFpWen)
  io.out.bits.toFpRf.           foreach(x => x.bits.pdest := outFpPdest.get)
  io.out.bits.toFpRf.           foreach(x => x.bits.data  := outFpData.get)
  io.out.bits.toVecRf.          foreach(x => x.valid      := Mux1H(fuOutValidOH, fuVecWenVec) || F2VVecWen || I2VVecWen)
  io.out.bits.toVecRf.          foreach(x => x.bits.pdest := outVecPdest.get)
  io.out.bits.toVecRf.          foreach(x => x.bits.data  := outVecData.get)
  io.out.bits.toV0Rf.           foreach(x => x.valid      := Mux1H(fuOutValidOH, fuV0WenVec))
  io.out.bits.toV0Rf.           foreach(x => x.bits.pdest := Mux1H(fuOutValidOH, funcUnits.map(_.io.out.bits.ctrl.pdest)))
  io.out.bits.toV0Rf.           foreach(x => x.bits.data  := Mux1H(fuOutValidOH, fuOutresVec.map(_.data)))
  io.out.bits.toVlRf.           foreach(x => x.valid      := Mux1H(fuOutValidOH, fuVlWenVec))
  io.out.bits.toVlRf.           foreach(x => x.bits.pdest := Mux1H(fuOutValidOH, funcUnits.map(_.io.out.bits.ctrl.pdestVl.getOrElse(0.U))))
  io.out.bits.toVlRf.           foreach(x => x.bits.data  := Mux1H(fuOutValidOH, fuOutresVec.map(_.data)))
  io.out.bits.redirect.               foreach(x => x := Mux1H((fuOutValidOH zip fuRedirectVec).filter(_._2.isDefined).map(x => (x._1, x._2.get))))
  io.out.bits.toRob.valid                            := Mux1H(fuOutValidOH, funcUnits.map(_.io.out.bits.ctrl.toRobValid))
  io.out.bits.toRob.bits.robIdx                      := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.robIdx))
  io.out.bits.toRob.bits.fflags.      foreach(x => x := Mux1H(fuOutValidOH, fuOutresVec.map(_.fflags.getOrElse(0.U.asTypeOf(io.out.bits.toRob.bits.fflags.get)))))
  io.out.bits.toRob.bits.wflags.      foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.fpu.getOrElse(0.U.asTypeOf(new FPUCtrlSignals)).wflags)))
  io.out.bits.toRob.bits.vxsat.       foreach(x => x := Mux1H(fuOutValidOH, fuOutresVec.map(_.vxsat.getOrElse(0.U.asTypeOf(io.out.bits.toRob.bits.vxsat.get)))))
  io.out.bits.toRob.bits.exceptionVec.foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.exceptionVec.getOrElse(0.U.asTypeOf(io.out.bits.toRob.bits.exceptionVec.get)))))
  io.out.bits.toRob.bits.flushPipe.   foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.flushPipe.getOrElse(0.U.asTypeOf(io.out.bits.toRob.bits.flushPipe.get)))))
  io.out.bits.toRob.bits.replay.      foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.replay.getOrElse(0.U.asTypeOf(io.out.bits.toRob.bits.replay.get)))))
  io.out.bits.toRob.bits.isRVC.       foreach(x => x := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.ctrl.isRVC.getOrElse(false.B))))

  io.I2FOutValid.foreach(x => x := Mux1H(fuOutValidOH, i2fWenVec))
  io.V2FOutValid.foreach(x => x := Mux1H(fuOutValidOH, v2fWenVec))
  io.F2IOutValid.foreach(x => x := Mux1H(fuOutValidOH, f2iWenVec))
  io.V2IOutValid.foreach(x => x := Mux1H(fuOutValidOH, v2iWenVec))
  io.I2VOutValid.foreach(x => x := Mux1H(fuOutValidOH, i2vWenVec))
  io.F2VOutValid.foreach(x => x := Mux1H(fuOutValidOH, f2vWenVec))

  io.toFrontendBJUResolve.foreach{ case resolve => {
    val bjus = funcUnits.filter(x => x.cfg.isJmp || x.cfg.isBrh)
    val resolveVec = VecInit(bjus.map(_.io.toFrontendBJUResolve.get))
    resolve.valid := resolveVec.map(_.valid).reduce(_ || _)
    resolve.bits := Mux1H(resolveVec.map(_.valid), resolveVec.map(_.bits))
  }}
  io.csrio.foreach(exuio => funcUnits.foreach(fu => fu.io.csrio.foreach{
    fuio =>
      exuio <> fuio
      fuio.exception := DelayN(exuio.exception, 2)
      fuio.robDeqPtr := DelayN(exuio.robDeqPtr, 2)
  }))
  io.csrin.foreach(exuio => funcUnits.foreach(fu => fu.io.csrin.foreach{fuio => fuio := exuio}))
  io.csrToDecode.foreach(toDecode => funcUnits.foreach(fu => fu.io.csrToDecode.foreach(fuOut => toDecode := fuOut)))

  io.vtype.foreach(exuio => funcUnits.foreach(fu => fu.io.vtype.foreach(fuio => exuio := fuio)))
  io.fenceio.foreach(exuio => funcUnits.foreach(fu => fu.io.fenceio.foreach(fuio => fuio <> exuio)))
  io.frm.foreach(exuio => funcUnits.foreach(fu => fu.io.frm.foreach(fuio => fuio <> exuio)))
  io.vxrm.foreach(exuio => funcUnits.foreach(fu => fu.io.vxrm.foreach(fuio => fuio <> exuio)))
  io.vlIsZero.foreach(exuio => funcUnits.foreach(fu => fu.io.vlIsZero.foreach(fuio => exuio := fuio)))
  io.vlIsVlmax.foreach(exuio => funcUnits.foreach(fu => fu.io.vlIsVlmax.foreach(fuio => exuio := fuio)))
  // RegNext for better timing and it should be fine
  io.instrAddrTransType.foreach(exuio => funcUnits.foreach(fu => fu.io.instrAddrTransType.foreach(fuio => fuio := RegNext(exuio))))

  // debug info
  io.out.bits.debug     := 0.U.asTypeOf(io.out.bits.debug)
  io.out.bits.debug.isPerfCnt := funcUnits.map(_.io.csrio.map(_.isPerfCnt)).map(_.getOrElse(false.B)).reduce(_ || _)
  io.out.bits.perfDebugInfo.foreach(_ := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.perfDebugInfo.getOrElse(0.U.asTypeOf(new PerfDebugInfo)))))
  io.out.bits.debug_seqNum.foreach(_ := Mux1H(fuOutValidOH, fuOutBitsVec.map(_.debug_seqNum.getOrElse(0.U.asTypeOf(InstSeqNum())))))
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
